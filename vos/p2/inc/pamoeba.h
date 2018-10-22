/************************************************************************/
/* Parallel amoeba algorithm for finding a minimum of the given		*/
/* multidimensional function.  Implements the "parallel simplex" method	*/
/* described by Lee and Wiswall, itself a modification of the "downhill	*/
/* simplex method" of Nelder and Mead.					*/
/*									*/
/* Re-implemented to remove Numerical Recipes code by Bob Deen.		*/
/* See the .c file for detailed comments.				*/
/*									*/
/* In all cases, parallel_degree is the amount of parallelism (P in the	*/
/* paper).  If P is 1, this devolves to the standard N-M simplex.  If	*/
/* P is 0, it decides on its own what to use for P.  Separately, omp_on	*/
/* says whether to actually enable parallel execution.  This is separate*/
/* because according to the paper, sometimes the parallel algorithm is	*/
/* more efficient even on single-threaded machines, because it considers*/
/* multiple vertices at once, not just the worst one.			*/
/*									*/
/* Other than the addition of parallel_degree and omp_on, these routines*/
/* are drop-in compatible with the amoeba*() (non-parallel) routines,	*/
/* although the formal parameters have been renamed.			*/
/************************************************************************/

#ifndef _PAMOEBA_H
#define _PAMOEBA_H

#include "xvmaininc.h"
#include "amoeba.h"		// for the AmoebaFunc definition

#ifdef __cplusplus
extern "C" {
#endif

/* Main pamoeba algorithm */

#ifdef _NO_PROTO
void pamoeba_base();
#else
void pamoeba_base(double *simplex, double Y[],
		int ndim, int width, double ftol, int itmax, int *iter,
		AmoebaFunc func, void *func_args, int print_interval,
		int parallel_degree, int omp_on);
#endif

#ifdef _NO_PROTO
void pamoeba();
#else
void pamoeba(double *simplex, double Y[],
		int ndim, int width, double ftol, int itmax, int *iter,
		AmoebaFunc func, void *func_args,
		int parallel_degree, int omp_on);
#endif

/* Easy-start pamoeba algorithm */

#ifdef _NO_PROTO
double pamoeba2();
#else
double pamoeba2(double *simplex0, double lambda,
		int ndim, double ftol, int itmax, int *iter,
		AmoebaFunc func, void *func_args,
		int parallel_degree, int omp_on);
#endif

/* Easy-start pamoeba algorithm with lambda as a vector */

#ifdef _NO_PROTO
double pamoeba3();
#else
double pamoeba3(double *simplex0, double *lambda_vec,
		int ndim, double ftol, int itmax, int *iter,
		AmoebaFunc func, void *func_args,
		int parallel_degree, int omp_on);
#endif

/* Like pamoeba3 but with a progress-print option */

#ifdef _NO_PROTO
double pamoeba4();
#else
double pamoeba4(double *simplex0, double *lambda_vec,
		int ndim, double ftol, int itmax, int *iter,
		AmoebaFunc func, void *func_args, int print_interval,
		int parallel_degree, int omp_on);
#endif

#ifdef __cplusplus
}
#endif

#endif	/* _PAMOEBA_H */

