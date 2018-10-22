#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/*  This routine prints the test matrix.		           	*/
/************************************************************************/
tzminvmat(a1,n1) 
   float *a1;
   int n1;
   {
       int i, j;
       char msg[80], *mp;
       
       zvmessage("","");

       for (i=0; i<n1; i++)
       {
           mp = msg; 
           for (j=0; j<n1; j++)
               {
               sprintf (mp," %10f ",*(a1+(i*n1)+j));
               mp=(mp+11);
               }
           zvmessage(msg,"");
       }
       zvmessage("","");

   }
/************************************************************************/
/*  This routine prints output of tzminv for analysis.           	*/
/************************************************************************/
tzminvprt(a1,n1,d1) 
   float *a1,d1;
   int n1;
   {
       int i, j;
       char msg[80], *mp;
       
       zvmessage("","");
       zvmessage("LOWER TRIANGLE OF INVERSE","");

       for (i=0; i<n1; i++)
       {
           mp = msg; 
           for (j=0; j<=i; j++)
               {
               sprintf (mp," %10f ",*(a1+(i*n1)+j));
               mp=(mp+11);
               }
           zvmessage(msg,"");
       }
       zvmessage("","");

       sprintf (msg,"VALUE OF DETERMINANT = %12.8f\n",d1);
       zvmessage(msg,"");
   }

/************************************************************************/
/*   Main Test routine for the "C" call to minv.   This routine builds  */
/*   an array with a determine of one and invokes the "C" bridge zminv. */
/*   The bridge will reverse the rows and columns prior to invoking     */
/*   minv.  After minv has executed, the bridge will again reverse the  */
/*   rows and columns.  This reversal is done because of the row versus */
/*   columns major between FORTRAN and "C" for multi-dimentioned arrays.*/
/*   The second and third arrays are to test the reversal logic in the  */
/*   bridge as array a is identical in row and column.                  */
/************************************************************************/

void FTN_NAME(tzminv)() 
   {
       float a[4][4];
       float c[4][4];
       float e[4][4];

       float b[4], z[4], d, *dp, *mp, *wp1, *wp2;
       int n;
       n=4;

       /* initialize the arrays: array a is a square array with     */
       /* a determinant of 1, while array c is an arbritrary array  */
       /* and array e is array c with row and column reversal       */

       a[0][0] = 5; a[0][1] = 7; a[0][2] = 6; a[0][3] = 5; /* row 0 */
       a[1][0] = 7; a[1][1] =10; a[1][2] = 8; a[1][3] = 7; /* row 1 */
       a[2][0] = 6; a[2][1] = 8; a[2][2] =10; a[2][3] = 9; /* row 2 */
       a[3][0] = 5; a[3][1] = 7; a[3][2] = 9; a[3][3] =10; /* row 3 */

       c[0][0] = 6; c[0][1] = 4; c[0][2] = 6; c[0][3] = 5; /* row 0 */
       c[1][0] = 7; c[1][1] = 2; c[1][2] = 3; c[1][3] = 4; /* row 1 */
       c[2][0] = 4; c[2][1] = 2; c[2][2] = 1; c[2][3] = 5; /* row 2 */
       c[3][0] = 6; c[3][1] = 6; c[3][2] = 6; c[3][3] = 6; /* row 3 */

       e[0][0] = 6; e[0][1] = 7; e[0][2] = 4; e[0][3] = 6; /* row 0 */
       e[1][0] = 4; e[1][1] = 2; e[1][2] = 2; e[1][3] = 6; /* row 1 */
       e[2][0] = 6; e[2][1] = 3; e[2][2] = 1; e[2][3] = 6; /* row 2 */
       e[3][0] = 5; e[3][1] = 4; e[3][2] = 5; e[3][3] = 6; /* row 3 */

       /* Test with matrix a */

       zvmessage("DETERMINANT = 1 MATRIX","");

       mp  = &a[0][0];
       wp1 = &b[0];
       wp2 = &z[0];
       dp  = &d;

       tzminvmat(mp,n);      /* print the matrix  */

       zminv(mp,n,dp,wp1,wp2);   /* invoke the bridge  */

       tzminvprt(mp,n,d);    /* print the results  */

       zvmessage("NOW INVERT BACK TO ORIGINAL","");

       zminv(mp,n,dp,wp1,wp2);   /* invoke the bridge  */

       tzminvprt(mp,n,d);    /* print the results  */

       /* Test with matrix c */

       zvmessage("ARBRITRARY MATRIX","");

       mp = &c[0][0];

       tzminvmat(mp,n);      /* print the matrix  */

       zminv(mp,n,dp,wp1,wp2);   /* invoke the bridge  */

       tzminvprt(mp,n,d);    /* print the results  */

       zvmessage("NOW INVERT BACK TO ORIGINAL","");

       zminv(mp,n,dp,wp1,wp2);   /* invoke the bridge  */

       tzminvprt(mp,n,d);    /* print the results  */

       /* Test with matrix e which is opposite c  */

       zvmessage("ROW/COLUMN REVERSED ARBRITRARY MATRIX","");

       mp = &e[0][0];

       tzminvmat(mp,n);      /* print the matrix  */

       zminv(mp,n,dp,wp1,wp2);   /* invoke the bridge  */

       tzminvprt(mp,n,d);    /* print the results  */

       zvmessage("NOW INVERT BACK TO ORIGINAL","");

       zminv(mp,n,dp,wp1,wp2);   /* invoke the bridge  */

       tzminvprt(mp,n,d);    /* print the results  */
   }
