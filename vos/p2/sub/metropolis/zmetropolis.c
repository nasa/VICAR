#include "xvmaininc.h"
#include "ftnbridge.h"

void (* cost)();   /* the cost function to be called */
/************************************************************************/
/* FORTRAN-Callable Version: cost  -                                    */
/************************************************************************/

void FTN_NAME2(zzcost, ZZCOST) (answer, array, array2, npts, retcost, ind)

float *answer;       /* The solution vector */
float *array;     /* An array of data */
float *array2;    /* Another array of data */
int *npts;         /* The number of points in array and array2 */
float *retcost;	  /* The returned cost */
int *ind;         /* Status indicator */

{

(* cost)(answer, array, array2, *npts, retcost, ind);

}


/************************************************************************/
/* C-Callable Version: zmetropolis -                                    */
/************************************************************************/

void zmetropolis(costz,narg, array, array2, range, nten, ans, lim, 
                   norm, npts, prnt, ind)
void (* costz)();   /* the cost function to be called */
int narg;         /* The number of variables */  
void *array;      /* An array of data */
float *array2;    /* Another array of data */
float *range;     /* Bounds of solution vector elements */
int nten;         /* The number of iteration to reduce the error by 10 */
float *ans;       /* The solution vector */
int lim;          /* The total number of iterations permitted */
int norm;         /* The number of iterations between normalizations */
int npts;         /* The number of points in array and array2 */
int prnt;	  /* The number of iterations between printouts */
int *ind;         /* Status indicator */

{
void (* costp)();      /* The bridge cost function to be called */
costp = FTN_NAME2(zzcost, ZZCOST);
cost = costz;        /* The cost function to be called */
FTN_NAME2(metropolis,METROPOLIS)(costp, &narg, array, array2, range, &nten, ans,
                   &lim, &norm, &npts, &prnt, ind); /* invoke metropolis */

}
