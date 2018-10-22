/************************************************************************/
/* Amoeba algorithm for finding a minimum of the given multidimensional	*/
/* function.  Implements the "downhill simplex method" of Nelder and	*/
/* Mead.								*/
/* Taken by Jean Lorre from Numerical Recipes by Jean Lorre.		*/
/* Genericized by Bob Deen.						*/
/* See the .c file for detailed comments.				*/
/************************************************************************/

#ifndef _AMOEBA_H
#define _AMOEBA_H

#include "xvmaininc.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Function pointer prototype for the cost function */

typedef double (*AmoebaFunc)(
#ifndef _NO_PROTO
    double p[],
    int ndim,
    void *func_args
#endif
);

/* Main amoeba algorithm */

#ifdef _NO_PROTO
void amoeba_base();
#else
void amoeba_base(double *P, double Y[],
		int NDIM, int WIDTH, double FTOL, int ITMAX, int *ITER,
		AmoebaFunc func, void *func_args, int print_interval);
#endif

#ifdef _NO_PROTO
void amoeba();
#else
void amoeba(double *P, double Y[],
		int NDIM, int WIDTH, double FTOL, int ITMAX, int *ITER,
		AmoebaFunc func, void *func_args);
#endif

/* Easy-start amoeba algorithm */

#ifdef _NO_PROTO
double amoeba2();
#else
double amoeba2(double *Pzero, double lambda,
		int NDIM, double FTOL, int ITMAX, int *ITER,
		AmoebaFunc func, void *func_args);
#endif

/* Easy-start amoeba algorithm with lambda as a vector */

#ifdef _NO_PROTO
double amoeba3();
#else
double amoeba3(double *Pzero, double *lambda_vec,
		int NDIM, double FTOL, int ITMAX, int *ITER,
		AmoebaFunc func, void *func_args);
#endif

/* Like amoeba3 but with a progress-print option */

#ifdef _NO_PROTO
double amoeba4();
#else
double amoeba4(double *Pzero, double *lambda_vec,
		int NDIM, double FTOL, int ITMAX, int *ITER,
		AmoebaFunc func, void *func_args, int print_interval);
#endif

#ifdef __cplusplus
}
#endif

#endif	/* _AMOEBA_H */

