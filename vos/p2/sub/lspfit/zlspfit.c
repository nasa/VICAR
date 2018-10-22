/*
   This file contains a set of C-bridges to LSPFIT subroutines.  
   See lspfit.hlp for more information.

   History:

      April 6, 1998     Thomas Huang     Initial release.
*/
 
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "zlspfit.h"

/*
   This is a C-bridge to the Fotran subroutine LSPFIT, which solves the 
   linear equation SX = U for X where X is the COEF matrix and U is the 
   UMOM matrix.  

   Arguments:
      m        Input; the order of the polynomial.
      mom      Input; an array that  contains sums of products 
                  where S(jk)=SUM(Xi^j * Yi^k), for i = 1..n.
      umom     Input; an array that contains sums of products 
                  where U(jk) = SUM(Ui * Xi^j * Yi^k) and 
                  V(jk) = SUM(Vi * Xi^j * Yi^k), for i = 1..n.
      coef     Output; an array that contains the coefficients A(ij) and B(ij).
*/ 
int zlspfit (int m, double *mom, double *umom, double *coef)
{
   int ier;  /* flag to indicate if there is an error. */

   FTN_NAME2(lspfit, LSPFIT) (&m, mom, umom, coef, &ier);

   return ier;
}



/*
   This is a C-bridge to the Fortran subroutine CLFIT, which is a constrained
   Linear FIT routine.  It finds coefficients a,b,e,f such that 
      U =  a*X + b*Y + e
      V = -b*X + a*Y + f
   subject to various constraints on a,b,e and f.

   Arguments:
      ifit     Input; code which specifies the combination of operations 
                  (rotate, scale, offset) to be used in the fit.
      mom      Input; an array that  contains sums of products
                  where S(jk)=SUM(Xi^j * Yi^k), for i = 1..n.
      umom     Input; an array that contains sums of products
                  where U(jk) = SUM(Ui * Xi^j * Yi^k) and
                  V(jk) = SUM(Vi * Xi^j * Yi^k), for i = 1..n.
      coef     Output; an array that contains the coefficients A(ij) and B(ij).
*/ 
int zclfit (int ifit, double *mom, double *umom, double *coef)
{
   int ier;  /* flag to indicate if there is an error. */

   FTN_NAME2(clfit, CLFIT) (&ifit, mom, umom, coef, &ier);

   return ier;
}



/*
   This is a C-bridge to the Fortran subroutine MOMENT, which computes 
   the moments of the x,y and u,v coordinates.

   Arguments:
      npow     Input; the order of the polynomial.
      pts      Input; an array of N data points in transposed order, because
                  Fortran matrix is column-major order and C is row-major
                  order.
      n        Input; number of tiepoints in 'pts' array.
      mom      Output; an array that  contains sums of products
                  where S(jk)=SUM(Xi^j * Yi^k), for i = 1..n.
      umom     Output; an array that contains sums of products
                  where U(jk) = SUM(Ui * Xi^j * Yi^k) and
                  V(jk) = SUM(Vi * Xi^j * Yi^k), for i = 1..n.
*/ 
void zmoment (int npow, float *pts, int n, double *mom, double *umom)
{
   FTN_NAME2(moment, MOMENT) (&npow, pts, &n, mom, umom);
}



/*
   This is a C-bridge to the Fortran subroutine RMSFIT, which computes
   the RMS error of polynomial fit.

   Arguments:
      mode     Input; if mode=1, the input (Ui, Vi) values in the 'pts' array
                  are replaced by their computed polynomial values.
      npow     Input; the order of the polynomial.
      pts      Input; an array of N data points in transposed order, because
                  Fortran matrix is column-major order and C is row-major
                  order.
      n        Input; number of tiepoints in 'pts' array.
      coef     Input; An array that contains the coefficients A(ij) and B(ij).
      drmx     Output; RMS error.
      dmax     Output; the maximum error.
      imax     Output; index to 'pts' with the largest error.
*/
void zrmsfit (int mode, int npow, float *pts, int n, 
              double *coef, double *drms, double *dmax, int *imax)
{
   FTN_NAME2(rmsfit, RMSFIT) (&mode, &npow, pts, &n, coef, drms, dmax, imax);
}



/*
   This is a C-bridge to the Fortran subroutine POLYTRAN, which computes
   coordinates (U0, V0) as a polynomial of coordinates (X0, Y0).

   Arguments:
      npow     Input; the order of the polynomial.
      coef     Input; an array that contains the coefficients A(ij) and B(ij).
      x0       Input; x coordinate.
      y0       Input; y coordinate.
      u0       Output; x coordinate.
      v0       Output; y coordinate.
*/
void zpolytran (int m, double *coef, float x0, float y0, float *u0, float *v0)
{
   FTN_NAME2(polytran, POLYTRAN) (&m, coef, &x0, &y0, u0, v0);
}

