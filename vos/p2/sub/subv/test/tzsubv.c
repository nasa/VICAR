#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzsubv)() 

{
  char pbuf[81];
  static unsigned char b[2] = {5,64}, 
                  c[2] = {10,3};

  static float bf[2] = {5.0,64.0}, cf[2] = {10.0,3.0};

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      zsubv( 1,2, b, c, 1,1);   /*  subtract b from c */

      sprintf( pbuf, "Output from zsubv = %u   %u", c[0], c[1]);
      zvmessage(pbuf, "");
      zvmessage("Correct values are   5    195","");

      zsubv( 7,2, bf, cf, 1,1);   /*  subtract bf from cf */

      sprintf( pbuf, "Output from zsubv = %f   %f", cf[0], cf[1]);
      zvmessage(pbuf, "");
      zvmessage("Correct values are   5.000000    -61.000000","");
}
