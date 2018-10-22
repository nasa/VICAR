////////////////////////////////////////////////////////////////////////
// Parallel amoeba algorithm for finding a minimum of the given
// multidimensional function.  Implements the "parallel simplex" method
// described by Lee and Wiswall, itself a modification of the "downhill
// simplex method" of Nelder and Mead.
//
// Re-implemented to remove Numerical Recipes code by Bob Deen.
//
// In all cases, parallel_degree is the amount of parallelism (P in the
// paper).  If P is 1, this devolves to the standard N-M simplex.  If
// P is 0, it decides on its own what to use for P.  Separately, omp_on
// says whether to actually enable parallel execution.  This is separate
// because according to the paper, sometimes the parallel algorithm is
// more efficient even on single-threaded machines, because it considers
// multiple vertices at once, not just the worst one. 
//
// Other than the addition of parallel_degree and omp_on, these routines
// are drop-in compatible with the amoeba*() (non-parallel) routines,
// although the formal parameters have been renamed.
//
// simplex is an array of ndim+1 rows, where each row is a potential solution,
// and a vertex of the simplex.  The algorithm operates by moving the solutions
// around in multidimensional space using one of 4 defined transforms,
// "flowing" the highest values "downhill", in an amoeba-like manner.
// See the paper for more details.
//
// ndim is the number of dimensions (independent variables).
//
// simplex must be initialized as follows:  the first row is an
// initial guess at the solution.  Each successive row is equal to that
// first row, with one of the variables perturbed by an amount "which is
// your	guess of the problem's characteristic length scale", so each
// variable is perterbed in one and only one row.  Upon completion, any
// row will hold a valid solution, although simplex[0] is normally
// used.  See amoeba2 or amoeba3 for wrapper functions which do this
// initialization for you.
//
// Physically, simplex must be an array[][width], where width is
// the physical size of the second dimension.  Only ndim elements are
// used from the second dimension.  The first dimension must be big
// enough to hold ndim+1 rows.  Note that simplex must be a true
// two-dimensional array, not an array of pointers to arrays.  The code
// internally is complicated somewhat in order to support a variable
// width dimension.  But a variable width could be used to, for example,
// make sure that each vertex of the simplex is in a different memory page,
// if the OMP implementation cared about such things.
//
// results is an array of ndim+1 values, which hold the results of the
// function evaluation for each trial in simplex.  It must be
// pre-initialized based on the initial simplex.
//
// ftol is the fractional convergence tolerance to be achieved in the
// function. 0.00000001 (1e-8) is a good value.
//
// The number of iterations is returned in iter.  itmax is the maximum
// number of iterations allowed (suggestion: 5000).
//
// "func" is a pointer to the actual function to be minimized.  It
// must have the signature:
//   double func(double p[], int ndim, void *func_args);
// AmoebaFunc is a typedef for this pointer defined in the include
// file.  If you're using C++, this function must be declared
// extern "C".
//
// "func_args" is an opaque pointer that is passed into the function.
// It will normally be a pointer to a structure that contains all the
// additional arguments that the function needs.
//
// "print_interval" says how often to print a progress message.  When
// the number of iterations is a multiple of this, the message is
// printed.  If 0, no message is printed.
//
// Sample call:
//	struct CostParams {
//	    int a, b;  double c;
//	};
//	double cost(double p[], int ndim, void *func_args)
//	    struct CostParams *params = (struct CostParams *)func_args;
//	    return params->a + p[0] * params->c - p[1];	    (whatever)
//	}
//      ... in the caller ...
//	struct CostParams parm;
//	double simplex[11][10], results[11];
//	parm.a = 5;  parm.b = 10;  parm.c = 3.14159;
//	... fill up simplex[0][*] with initial solution ...
//	... copy simplex[0][*] to simplex[1:10][*] ...
//	... add labmda to simplex[i+1][i] ...
//	for (i=0; i<11; i++)
//	    Y[i] = cost(simplex[i], 10, &parm);
//	amoeba(simplex, Y, 10, 10, 1e-8, 1000, &iter, cost, &parm);
//	... simplex[0][*] contains solution and Y[0] the
//		 function value ...
//
// Note that this is callable only from C/C++; due to the function
// pointers and such a Fortran bridge is not feasible.  Write a small 
// C/C++ wrapper that calls this if you need to.
////////////////////////////////////////////////////////////////////////

#include "amoeba.h"
#include <zvproto.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef _OPENMP
#include <omp.h>
#endif

// Access to simplex as A(row,col)
// IMPORTANT NOTE!  A rows are sorted, so use the index variable.  AR and AT
// are not, they are simply indexed by the loop index.  BUT - CAUTION - the
// "j" loop does not start at 0 so you always have to use j0 for AR and AT.

#define A(row,col) (simplex[(row)*width + (col)])
#define AR(row,col) (result[(row)*width + (col)])
#define AT(row,col) (trial[(row)*width + (col)])

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

static void pamoeba_mergesort(double *Y, int *&ms_index,
		int *ms_index1, int *ms_index2, int n);
static void pamoeba_bottom_up_merge(double *Y, int *AA,
			int iLeft, int iRight, int iEnd, int *BB);

extern "C" void pamoeba_base(double *simplex, double Y[], int ndim, int width,
	double ftol, int itmax, int *iter, AmoebaFunc func, void *func_args,
	int print_interval, int parallel_degree, int omp_on)
{
    double alpha = 1.0;	// Reflection factor
    double beta = 0.5;	// Contraction factor
    double gamma = 2.0;	// Expansion factor
    double tau = 0.5;	// Entire simplex contraction factor

    *iter = 0;

    // Determine parallelism if necessary.  Initial guess shows optimum
    // to be somewhat more than half of the number of variables.  In order
    // to avoid thread starvation we try to use an integer multiple of the
    // number of actual threads, ending up around 1/2 of the variables.

    int num_threads = 1;
#ifdef _OPENMP
    if (omp_on)
	num_threads = omp_get_max_threads();
#endif

    if (parallel_degree <= 0) {
	double usage_factor = 0.5;	// tunable parameter!

	int target = ndim * usage_factor;
	if (num_threads > target)
	    parallel_degree = target;	// more procs than we need
	else {
	    int f = (int)(target / num_threads); // truncate, no more than target
	    parallel_degree = num_threads * f;
	}
	if (!omp_on)
	    parallel_degree = 1;	// turn off parallel if no OMP.  Can be
					// overridden with specific param input
    }

    if (parallel_degree > ndim)
	parallel_degree = ndim;	// remember simplex is ndim+1.  Has to be at
				// least one less than simplex so there's
				// always a "better" entry to compare against

    if (parallel_degree < num_threads)
	num_threads = parallel_degree;	// don't waste threads we don't need

    // Allocate working storage.  Remember there's ndim+1 vertices in a simplex

    int *ms_index1 = (int *)malloc((ndim+1) * sizeof(int));
    int *ms_index2 = (int *)malloc((ndim+1) * sizeof(int));
    int *ms_index;

    // centroid is one vertex, thus dimensioned "ndim"

    double *centroid = (double *)malloc(ndim * sizeof(double));

    // Hold result points, and trial points.  Only need P vertices, not
    // the entire simplex.  Also the answers.  Dimensioned to "width" in
    // in case caller is trying to make sure all are on separate cache lines.

    double *result = (double *)malloc(width * parallel_degree * sizeof(double));
    double *trial = (double *)malloc(width * parallel_degree * sizeof(double));
    double *result_Y = (double *)malloc(parallel_degree * sizeof(double));

    if (!ms_index1  || !ms_index2 || !centroid ||
		!result || !trial || !result_Y) {
	zvmessage("PAmoeba unable to allocate memory!", "");

	free(ms_index1); free(ms_index2); free(centroid);
	free(result); free(trial); free(result_Y);
	return;
    }

    // Step 1: Initialization has been done prior to entry.

    while (1) {			// breaks from middle of loop

        // Step 2: Assign each thread one of the parallel_degree(P) worst points

        // To do this we virtually sort the Y array to find the P worst
        // points.  We use an index array to avoid shuffling the simplex around
        // a bunch of times.  We probably only really need to sort the worst P+1
        // values, and find the best value, but it's hard to stop most sort
        // algorithms and mergesort in particular has good performance if the
        // array is already sorted (which will often be the case, at least
	// below P).

        pamoeba_mergesort(Y, ms_index, ms_index1, ms_index2, ndim+1);

	// Check for loop termination conditions.  There are two (plus a
	// degenerate case): the Y range of the simplex is less than ftol
	// (this is our normal condition), or we exceed the max # of iterations.

	// Compute fractional range: 2*(HI-LO)/(HI+LO)

	double denom = fabs(Y[ms_index[0]]) + fabs(Y[ms_index[ndim]]);
	if (denom == 0.0) {	// should maybe use an epsilon but we get
	    break;		// very, very close to 0 sometimes
	}
	double rtol = 2.0 * fabs(Y[ms_index[0]] - Y[ms_index[ndim]])/denom;
	if (rtol < ftol) {
	    break;		// success!!
	}
	if (*iter >= itmax) {
	    zvmessage("Amoeba exceeding maximum iterations", "");
	    break;
	}

	(*iter)++;
	if (print_interval != 0 && ((*iter) % print_interval) == 0) {
	    // message time!
	    char msg[256];
	    sprintf(msg, "Iteration %d, rtol=%lf, ftol=%lf, best Y=%lf, worst Y=%lf", *iter, rtol, ftol, Y[ms_index[0]], Y[ms_index[ndim]]);
	    zvmessage(msg, "");
	}

	// Determine the centroid (M) of the "good" (exclude the P worst) points

	int first_bad = ndim - parallel_degree + 1;

	for (int i=0; i < ndim; i++) {
	    centroid[i] = 0.0;
	    for (int j=0; j < first_bad; j++) {
		centroid[i] += A(ms_index[j], i);
	    }
	    centroid[i] /= first_bad;
	}

        // Assign each processor one of the P worst points

	int bad_count = 0;	// number of procs that did NOT improve
				// (case 3 where contraction didn't help)

	// Step 3: Each processor returns updated point in AR(j0,*),result_Y[j0]

#pragma omp parallel for if (omp_on) num_threads(num_threads)

	for (int j = first_bad; j < ndim+1; j++) {

	    int j0 = j - first_bad;		// offset index, for AR and AT

	    // Important note:  A and Y are indexed via ms_index, while
	    // everything else (for which sorted order doesn't matter) is
	    // simply indexed with j.

	    int simp_idx = ms_index[j];		// index into the simplex
	    int best_idx = ms_index[0];

	    // Compute the reflected point

	    for (int i=0; i < ndim; i++) {
		AR(j0,i) = (1+alpha)*centroid[i] - alpha*A(simp_idx, i);
	    }

	    // Evaluate the function at the reflection point

	    double result_YY = (*func)(&AR(j0,0), ndim, func_args);

	    if (result_YY <= Y[best_idx]) {

		// Case 1: continue with expansion point
		// NOTE: The L-W paper has gamma=1 and AR+gamma*(AR-centroid).
		// Original N-M has gamma=2 and centroid+gamma*(AR-centroid).
		// We use the latter (rearranged so array lookup done only once)

		for (int i=0; i < ndim; i++) {
		    AT(j0,i) = (1-gamma)*centroid[i] + gamma*AR(j0,i);
		}
		double trial_Y = (*func)(&AT(j0,0), ndim, func_args);

		if (trial_Y < Y[best_idx]) {

		    // Use the expansion

		    for (int i=0; i < ndim; i++) {
			AR(j0,i) = AT(j0,i);
			result_YY = trial_Y;
		    }
		} // else use the reflection, already in AR

	    } else if (result_YY <= Y[ms_index[j-1]]) {

		// Case 2: Not best, but better than the next worse point
		// so return the reflection point
		// But it's already in AR at result_YY, so there's nothing to do

	    } else {

		// Case 3: try the contraction point instead.  If the
		// reflection point is better than the original, use it
		// (outside contraction), otherwise use the original
		// (inside contraction).

		if (result_YY <= Y[simp_idx]) {		// better, use refl
		    for (int i=0; i < ndim; i++) {
			AT(j0,i) = (1-beta)*centroid[i] + beta*AR(j0,i);
		    }
		} else {				// worse, use original
		    for (int i=0; i < ndim; i++) {
			AT(j0,i) = (1-beta)*centroid[i] + beta*A(simp_idx,i);
		    }
		}

		double trial_Y = (*func)(&AT(j0,0), ndim, func_args);

		if (trial_Y < Y[simp_idx]) {

		    // Use the contraction

		    for (int i=0; i < ndim; i++) {
			AR(j0,i) = AT(j0,i);
			result_YY = trial_Y;
		    }
		} else {

		    // Contraction isn't better, use refl (already in AR) if
		    // better, or go back to original if not.

		    if (result_YY > Y[simp_idx]) {	// worse than original
		        for (int i=0; i < ndim; i++) {
			    AR(j0,i) = A(simp_idx,i);	// reset to original
			}
		    }
#pragma omp atomic
		    bad_count++;		// flag as no real improvement

		}
	    }
	    result_Y[j0] = result_YY;
	}

	// End of parallel loop

	// Step 4: For the new simplex

	// Copy over new parts of simplex.  Even if we contract, we still
	// need the AR vs A decision from all the part 3's, so we do this
	// unconditionally.

	for (int j = first_bad; j < ndim+1; j++) {
	    int j0 = j-first_bad;
	    for (int i=0; i < ndim; i++) {
		A(ms_index[j],i) = AR(j0,i);
	    }
	    Y[ms_index[j]] = result_Y[j0];
	}

	// If anyone was "good" (case 1 or 2, or 3 where contraction worked),
	// use the new simplex.  Otherwise, if everyone was bad, contract.

	if (bad_count >= parallel_degree) {	// really ==, > just for safety

	    // Nobody found a better spot, so contract the entire simplex.
	    // Since we're contracting everything, we don't need to worry
	    // about the sort index now (except to get the best point).
	    // Interestingly, the paper did not mention parallelizing this
	    // loop, but since we're calculating every vertex of the simplex
	    // again, it certainly makes sense to do so...

#pragma omp parallel for if (omp_on)
	    for (int j=0; j < ndim+1; j++) {
	        int best_idx = ms_index[0];
		// This is a no-op calc at best_idx, but avoid doing it just
		// so we don't have parallel update issues to A(best_idx,*).
		// Leaves a processor idle but that doesn't really matter here.
		if (j == best_idx)
		    continue;

		for (int i=0; i < ndim; i++) {
		    A(j,i) = tau*A(best_idx,i) + (1-tau)*A(j,i);
		}
	        Y[j] = (*func)(&A(j,0), ndim, func_args);
	    }
	    // end parallel
	}
    }

    // Now that we're done, copy the best to slot 0.  It doesn't matter for
    // small ftol, but for larger ones it might.  We already know where the
    // best is, and this is cheap enough to do, doesn't hurt anything (the
    // original amoeba() does not do this).

    int best_idx = ms_index[0];
    if (best_idx != 0) {
	for (int i=0; i < ndim; i++) {
	    double tmp = A(best_idx, i);
	    A(best_idx,i) = A(0,i);
	    A(0,i) = tmp;
	}
	double tmp = Y[best_idx];
	Y[best_idx] = Y[0];
	Y[0] = Y[best_idx];
    }

    free(ms_index1);
    free(ms_index2);
    free(centroid);
    free(result);
    free(trial);
    free(result_Y);
}

////////////////////////////////////////////////////////////////////////
// Implementation of bottom-up merge sort, adapted from Wikipedia
// (https://en.wikipedia.org/wiki/Merge_sort) under the Creative Commons
// license.  Adaptations including use of an index array, and swapping index
// array pointers rather than copying the array.
//
// ms_index is an index array for Y, so we don't have to physically
// reorder it.  The input and output indices (AA and BB) swap between index1
// and index2 to avoid having to physically copy the array.
////////////////////////////////////////////////////////////////////////

static void pamoeba_mergesort(double *Y, int *&ms_index,
		int *ms_index1, int *ms_index2, int n)
{
    int *AA = ms_index1;
    int *BB = ms_index2;

    // Create initial index array

    for (int i=0; i < n; i++)
	AA[i] = i;

    // Each 1-element run in AA is already "sorted".
    // Make successively longer sorted runs of length 2, 4, 8, 16, ... until
    // whole array is sorted.

    for (int width = 1; width < n; width = 2*width) {

	// Array AA is full of runs of length width.

        for (int i=0; i < n; i = i + 2*width) {

	    // Merge two runs: AA[i:i+width-1] and AA[i+width:i+2*width-1] to BB[]
	    // or copy AA[i:i:n-1] to BB[] if i+width >= n

	    pamoeba_bottom_up_merge(Y, AA, i,
				 MIN(i+width, n), MIN(i+2*width, n), BB);
	}

	// Now work array BB is full of runs of length 2*width.  Swap AA and BB
	// for the next iteration.

	if (AA == ms_index1) {
	    AA = ms_index2;
	    BB = ms_index1;
	} else {
	    AA = ms_index1;
	    BB = ms_index2;
	}

	// Now array AA is full of runs of length 2*width
    }
    ms_index = AA;
}

////////////////////////////////////////////////////////////////////////
// Left run is AA[iLeft:iRight-1], right run is AA[iRight:iEnd-1].
////////////////////////////////////////////////////////////////////////

static void pamoeba_bottom_up_merge(double *Y, int *AA,
			int iLeft, int iRight, int iEnd, int *BB)
{
    int i = iLeft;
    int j = iRight;

    // While there are elements in the left or right runs...

    for (int k = iLeft; k < iEnd; k++) {

	// if left run head exists and is <= existing right run head

	if (i < iRight && (j >= iEnd || Y[AA[i]] <= Y[AA[j]])) {
	    BB[k] = AA[i];
	    i = i + 1;
	} else {
	    BB[k] = AA[j];
	    j = j + 1;
	}
    }
}

////////////////////////////////////////////////////////////////////////
// This is the traditional call, with parallel_degree and omp_on added
////////////////////////////////////////////////////////////////////////

extern "C" void pamoeba(double *simplex, double Y[], int ndim, int width,
	double ftol, int itmax, int *iter, AmoebaFunc func, void *func_args,
	int parallel_degree, int omp_on)
{
    pamoeba_base(simplex, Y, ndim, width, ftol, itmax, iter, func, func_args, 0,
		parallel_degree, omp_on);
}

////////////////////////////////////////////////////////////////////////
// This is a wrapper around the pmoeba algorithm which does the
// initialization for you.  You provide only the initial solution,
// simplex0, as a simple vector of size NDIM, plus the "length scale"
// constant, lambda.  This wrapper will allocate simplex and Y, fill them
// up appropriately, call pamoeba, and return the simplex[0] solution in
// simplex0 and the Y[0] value as the function return.
//
// This should handle all uses of amoeba except when you want lambda
// to be a vector, e.g. a different "length scale" for each variable.
// See amoeba3 for this case.
//
// Note that the A macro from above is used here as well.
//
// Sample call:
//	struct CostParams {
//	    int a, b;  double c;
//	};
//	double cost(double p[], int ndim, void *func_args)
//	    struct CostParams *params = (struct CostParams *)func_args;
//	    return params->a + p[0] * params->c - p[1];	    (whatever)
//	}
//      ... in the caller ...
//	struct CostParams parm;
//	double simplex0[10], value;
//	parm.a = 5;  parm.b = 10;  parm.c = 3.14159;
//	... fill up simplex0[*] with initial solution ...
//	value = pamoeba2(simplex0, .1, 10, 1e-8, 1000, &iter, cost, &parm, -1);
//	... simplex0 contains solution and "value" the function value ...
//
////////////////////////////////////////////////////////////////////////

extern "C" double pamoeba2(double *simplex0, double lambda,
		int ndim, double ftol, int itmax, int *iter,
		AmoebaFunc func, void *func_args,
		int parallel_degree, int omp_on)
{
    double *simplex, *Y;
    double result;
    int width = ndim;

    simplex = (double *)malloc(ndim * (ndim+1) * sizeof(double));
    Y = (double *)malloc((ndim+1) * sizeof(double));

    if (!simplex || !Y) {
	zvmessage("pamoeba2 unable to allocate memory!", "");
	free(simplex); free(Y);
	return 0;
    }

    // Fill in the initial solution

    for (int i=0; i < ndim; i++) {
	for (int j=0; j < ndim+1; j++) {
	    A(j,i) = simplex0[i];
	}
    }

    // Perturb the initial solutions

    for (int j=0; j < ndim; j++) {
	A(j+1,j) += lambda;		// diagonal elements, skipping row 0
    }

    // Calculate the initial Y's.  Might as well do it parallel!

#pragma omp parallel for if (omp_on)
    for (int j=0; j < ndim+1; j++) {
	Y[j] = (*func)(&A(j,0), ndim, func_args);
    }

    // Call pamoeba

    pamoeba_base(simplex, Y, ndim, width, ftol, itmax, iter, func, func_args, 0,
		parallel_degree, omp_on);

    // Return results

    for (int i=0; i < ndim; i++) {
	simplex0[i] = A(0,i);
    }
    result = Y[0];

    free(simplex); free(Y);

    return result;
}

////////////////////////////////////////////////////////////////////////
// This is another wrapper around the amoeba algorithm which does the
// initialization for you.  It is exactly like pamoeba2, except that the
// "length scale" constant, lambda, is an array of doubles (of size
// ndim) instead of a single value.  This allows you to have a
// different length scale for each variable.
////////////////////////////////////////////////////////////////////////

extern "C" double pamoeba3(double *simplex0, double *lambda_vec,
		int ndim, double ftol, int itmax, int *iter,
		AmoebaFunc func, void *func_args,
		int parallel_degree, int omp_on)
{
    double *simplex, *Y;
    double result;
    int width = ndim;

    simplex = (double *)malloc(ndim * (ndim+1) * sizeof(double));
    Y = (double *)malloc((ndim+1) * sizeof(double));

    if (!simplex || !Y) {
	zvmessage("pamoeba3 unable to allocate memory!", "");
	free(simplex); free(Y);
	return 0;
    }

    // Fill in the initial solution

    for (int i=0; i < ndim; i++) {
	for (int j=0; j < ndim+1; j++) {
	    A(j,i) = simplex0[i];
	}
    }

    // Perturb the initial solutions

    for (int j=0; j < ndim; j++) {
	A(j+1,j) += lambda_vec[j];	// diagonal elements, skipping row 0
    }

    // Calculate the initial Y's.  Might as well do it parallel!

#pragma omp parallel for if (omp_on)
    for (int j=0; j < ndim+1; j++) {
	Y[j] = (*func)(&A(j,0), ndim, func_args);
    }

    // Call pamoeba

    pamoeba_base(simplex, Y, ndim, width, ftol, itmax, iter, func, func_args, 0,
		parallel_degree, omp_on);

    // Return results

    for (int i=0; i < ndim; i++) {
	simplex0[i] = A(0,i);
    }
    result = Y[0];

    free(simplex); free(Y);

    return result;
}

////////////////////////////////////////////////////////////////////////
// This is another wrapper around the pamoeba algorithm.  Just like
// pamoeba3 except a "print_interval" argument allows you to specify
// how often to print a progress message.  A message will be printedy
// every "print_interval" iterations, if it is >= 0.
/************************************************************************/

extern "C" double pamoeba4(double *simplex0, double *lambda_vec,
		int ndim, double ftol, int itmax, int *iter,
		AmoebaFunc func, void *func_args, int print_interval,
		int parallel_degree, int omp_on)
{
    double *simplex, *Y;
    double result;
    int width = ndim;

    simplex = (double *)malloc(ndim * (ndim+1) * sizeof(double));
    Y = (double *)malloc((ndim+1) * sizeof(double));

    if (!simplex || !Y) {
	zvmessage("pamoeba4 unable to allocate memory!", "");
	free(simplex); free(Y);
	return 0;
    }

    // Fill in the initial solution

    for (int i=0; i < ndim; i++) {
	for (int j=0; j < ndim+1; j++) {
	    A(j,i) = simplex0[i];
	}
    }

    // Perturb the initial solutions

    for (int j=0; j < ndim; j++) {
	A(j+1,j) += lambda_vec[j];	// diagonal elements, skipping row 0
    }

    // Calculate the initial Y's.  Might as well do it parallel!

#pragma omp parallel for if (omp_on)
    for (int j=0; j < ndim+1; j++) {
	Y[j] = (*func)(&A(j,0), ndim, func_args);
    }

    // Call pamoeba

    pamoeba_base(simplex, Y, ndim, width, ftol, itmax, iter, func, func_args,
		print_interval, parallel_degree, omp_on);

    // Return results

    for (int i=0; i < ndim; i++) {
	simplex0[i] = A(0,i);
    }
    result = Y[0];

    free(simplex); free(Y);

    return result;
}

