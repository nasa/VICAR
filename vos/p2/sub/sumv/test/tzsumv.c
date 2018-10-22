#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzsumv)() 

{
  char pbuf[81];
  int b[2], c;

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      b[0] = -16;
      b[1] =  640116;
      zsumv( 4, 2, b, &c, 1);   /*  sum b[0] and b[1] */

      sprintf( pbuf, "Output from zsumv = %d", c);
      zvmessage(pbuf, "");
      zvmessage("Correct value is 640100","");
}
