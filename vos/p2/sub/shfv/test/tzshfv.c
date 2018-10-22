#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzshfv)() 

{
  char pbuf[81];
  int b[1];

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      b[0] = -16;
      zshfv( 4, 1, -2, b, 1);   /*  shift b to the right by 2 bits.  */

      sprintf( pbuf, "Output from zshfv = %d", b[0]);
      zvmessage(pbuf, "");
      zvmessage("Correct value is -4","");
}
