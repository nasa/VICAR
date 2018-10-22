#include "xvmaininc.h"
#include "ftnbridge.h"

/****************************************************************************
     C-Callable Version: zcmul - perform complex multiplication of arrays
*****************************************************************************/

/* ***************************************************************************
    This function performs the same processing as the Fortran subroutine
    'CMUL'.  Since 'C' does not support complex math, the 'C' function
    'zcmul' emulates the fortran subroutine.  The original fortran 
    subroutine listing is duplicated in the following lines of code.
     
	FORTRAN SUBROUTINE CMUL(N,R1,I,R2,J,ROUT)

C-------CMUL PERFORMS A COMPLEX MULTIPLICATION OF ARRAYS

	COMPLEX*8 R1(N),R2(N),ROUT(N),A,B

	DO 10 L=1,N
           A = R1(L)
	   B = R2(L)
	   IF (I.EQ.1) A = CONJG(A)
	   IF (J.EQ.1) B = CONJG(B)
	   ROUT (L) = A*B
10      continue
	RETURN
	END

   The following logic and 'C' code were developed to emulate the complex
   math function for 'multiply'.

        if   z1 = (x1,y1)
        and  z2 = (x2,y2)

then 

        z1 + z2 = (x1+x2,y1+y2)

        z1 - z2 = (x1-x2,y1-y2)

        z1 * z2 = (x1*x2-y1*y2,x1*y2+x2*y1)

                  (x1*x2+y1*y2,x2*y1-x1*y2
        z1 / z2 =  ----------- -----------
                  (x2*x2+y2*y2 x2*x2+y2*y2)   

*************************************************************************** */

typedef struct {
      float real;
      float imaginary;
}complex;

void zcmul( n, r1, i, r2, j, results)
   complex *r1[1], *r2[1], *results[1];
   int      n, i, j;
{
complex a, b, c;
void    *r1_ptr, *r2_ptr, *r3_ptr;
int     ii;

   r1_ptr = (complex *)r1;
   r2_ptr = (complex *)r2;
   r3_ptr = (complex *)results;

   for (ii = 0; ii < n; ii++) {
      a = *((complex *)r1_ptr + ii);
      b = *((complex *)r2_ptr + ii);
      if (i == 1) {
         a.imaginary = a.imaginary * -1.0;
      }
      if (j == 1) {
         b.imaginary = b.imaginary * -1.0;
      }
      c.real = (a.real * b.real) - (a.imaginary * b.imaginary);
      c.imaginary = (a.real * b.imaginary) + (b.real * a.imaginary);
      *((complex *)r3_ptr + ii) = c;
   }
   return;
}
