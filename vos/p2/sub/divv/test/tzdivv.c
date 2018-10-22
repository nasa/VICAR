#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzdivv)() 

{
  char pbuf[81];
  static unsigned char b[2] = {10,1}, 
                  c[2] = {5,255};

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      zdivv( 1,2, b, c, 1,1);   /*  divide c by b*/

      sprintf( pbuf, "Output from zdivv = %u   %u", c[0], c[1]);
      zvmessage(pbuf, "");
      zvmessage("Correct values are   0    255","");
}
