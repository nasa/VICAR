#include "xvmaininc.h"
#include "ftnbridge.h"

/****************************************************************************
    Test 'C' Version of cmul ... perform complex multiplication of arrays
*****************************************************************************/

typedef struct  {
      float real;
      float imaginary;
}complex;

void FTN_NAME(tzcmul)() 
{
   complex c1[3], c2[3], result[3];
   int     n, i, j;
   char    string[132];

   c1[0].real = 1.0;
   c1[1].real = 2.0;
   c1[2].real = 3.0;
   c1[0].imaginary = 5.0;
   c1[1].imaginary = 2.0;
   c1[2].imaginary = 0.5;

   c2[0].real = 0.5;
   c2[1].real = 1.0;
   c2[2].real = 2.0;
   c2[0].imaginary = 2.0;
   c2[1].imaginary = 5.0;
   c2[2].imaginary = 1.0;

   result[0].real = 0.0;
   result[1].real = 0.0;
   result[2].real = 0.0;
   result[0].imaginary = 0.0;
   result[1].imaginary = 0.0;
   result[2].imaginary = 0.0;

   n = 3;
   i = 0;
   j = 0;

   (void) sprintf (string,"in1    %12.4E%12.4E%12.4E%12.4E%12.4E%12.4E",
             c1[0].real, c1[0].imaginary,
             c1[1].real, c1[1].imaginary,
             c1[2].real, c1[2].imaginary);
   zvmessage (string, 0);

   (void) sprintf (string,"in2    %12.4E%12.4E%12.4E%12.4E%12.4E%12.4E",
             c2[0].real, c2[0].imaginary,
             c2[1].real, c2[1].imaginary,
             c2[2].real, c2[2].imaginary);
   zvmessage (string, 0);

/* #1 */

   (void) sprintf (string,"calculate product and print result");
   zvmessage (string,0);

   (void) sprintf (string,"for both not conjugated");
   zvmessage (string,0);

   zcmul( n, c1, i, c2, j, result);

   (void) sprintf (string,"result    %12.4E%12.4E%12.4E%12.4E%12.4E%12.4E",
             result[0].real, result[0].imaginary,
             result[1].real, result[1].imaginary,
             result[2].real, result[2].imaginary);
   zvmessage (string, 0);

/* #2 */

   (void) sprintf (string,"calculate product and print result");
   zvmessage (string,0);

   (void) sprintf (string,"for first conjugated");
   zvmessage (string,0);

   i = 1;
   zcmul( n, c1, i, c2, j, result);

   (void) sprintf (string,"result    %12.4E%12.4E%12.4E%12.4E%12.4E%12.4E",
             result[0].real, result[0].imaginary,
             result[1].real, result[1].imaginary,
             result[2].real, result[2].imaginary);
   zvmessage (string, 0);

/* #3 */

   (void) sprintf (string,"calculate product and print result");
   zvmessage (string,0);

   (void) sprintf (string,"for second conjugated");
   zvmessage (string,0);

   i = 0;
   j = 1;
   zcmul( n, c1, i, c2, j, result);

   (void) sprintf (string,"result    %12.4E%12.4E%12.4E%12.4E%12.4E%12.4E",
             result[0].real, result[0].imaginary,
             result[1].real, result[1].imaginary,
             result[2].real, result[2].imaginary);
   zvmessage (string, 0);

/* #4 */

   (void) sprintf (string,"calculate product and print result");
   zvmessage (string,0);

   (void) sprintf (string,"for both conjugated");
   zvmessage (string,0);

   i = 1;
   j = 1;
   zcmul( n, c1, i, c2, j, result);

   (void) sprintf (string,"result    %12.4E%12.4E%12.4E%12.4E%12.4E%12.4E",
             result[0].real, result[0].imaginary,
             result[1].real, result[1].imaginary,
             result[2].real, result[2].imaginary);
   zvmessage (string, 0);

}
