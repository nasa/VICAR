#include "xvmaininc.h"
#include "ftnbridge.h"

void zmrev( r, u)
float *r;
int u;				/* order of matrix a */

/************************************************************************/
/* A Fortran matrix is colume-major order and C is row-major order, thus*/
/* the row and columns will be interchanged in this bridge before and   */
/* after the call to the MINV subroutine                                */
/************************************************************************/

{
int i,j;
float k, *mp, *p;

for (i=0; i<u-1; i++)   /* for each row = i */
    {
    for (j=i+1; j<u; j++)  /* for each column = j skipping */
                           /* intersetion of each row and column */
        {
        p=(r+(i*u)+j);  /* set pointer to [i][j] of exchange */
        mp=(r+(j*u)+i); /* set pointer to [j][i] of exchange */

        k = *p;         /* save [i][j] */
        *p= *mp;        /* move [j][i] into [i][j] */
        *mp= k;         /* store [[i][j] into [j][i] */
        }
    }
}
/************************************************************************/
/* C-Callable Version: zminv - invert matrix and calculate determinant 	*/
/************************************************************************/

void zminv( a, n, d, l, m)
float *a;			/* array to invert, returned inverted */
int n;				/* order of matrix a */
float *d;                       /* determinant returned,
                                   d=0 if a is singular */
float *l;                       /* work vector of length n */
float *m;                       /* work vector of length n */

{
zmrev( a, n);			/* reverse major/minor */

FTN_NAME2(minv, MINV) ( a, &n, d, l, m); /* invoke minv */

zmrev( a, n);			/* reverse major/minor */
}
