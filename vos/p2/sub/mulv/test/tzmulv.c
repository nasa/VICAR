#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzmulv)() 

{
  char pbuf[81];
  static unsigned char b[2] = {5,64}, 
                  c[2] = {10,3};

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      zmulv( 1,2, b, c, 1,1);   /*  multiply b and c*/

      sprintf( pbuf, "Output from zmulv = %u   %u", c[0], c[1]);
      zvmessage(pbuf, "");
      zvmessage("Correct values are   50    192","");
}
