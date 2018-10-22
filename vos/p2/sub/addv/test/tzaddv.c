#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzaddv)() 

{
  char pbuf[81];
  static unsigned char b[2] = {5,64}, 
                  c[2] = {10,100};

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      zaddv( 1,2, b, c, 1,1);   /*  add b to c*/

      sprintf( pbuf, "Output from zaddv = %u   %u", c[0], c[1]);
      zvmessage(pbuf, "");
      zvmessage("Correct values are   15    164","");
}
