#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzfloata)() 

{
  char pbuf[81];
  static unsigned char b[2] = {5,255};
  float c[2];

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      zfloata( 1,2, b, c);   /*  convert b to float, storing in c*/

      sprintf( pbuf, "Output from zfloata = %f   %f", c[0], c[1]);
      zvmessage(pbuf, "");
      zvmessage("Correct values are   5.0   255.0","");
}
