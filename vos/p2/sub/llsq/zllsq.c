#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of LLSQ                                           */
/************************************************************************/

void zllsq(a,b,m,n,l,x,ipiv,eps,ier,aux)
void   *a;    /* mxn coefficient matrix             (input) */
void   *b;    /* mxl righthandside matrix           (input) */
int     m;    /* row number of matrices a and b     (input) */
int     n;    /* column number of matrix a, row number of   */
              /*    matrix b. n <= m.               (input) */
int     l;    /* column number of matrices b and x. (input) */
void   *x;    /* nxl solution matrix.              (output) */
void   *ipiv; /* output vector of size n with pivotal info (output) */
double  eps;  /* tolerance for determination of rank (input) */
int    *ier;  /* error parameter, 0=success.        (output) */
void   *aux;  /* dummy auxilliary storage array, dimension max(2*n,l) (input) */
{
  float sigma;
  sigma = (float) eps;
  FTN_NAME2(llsq, LLSQ) (a,b,&m,&n,&l,x,ipiv,&eps,ier,aux);
}

