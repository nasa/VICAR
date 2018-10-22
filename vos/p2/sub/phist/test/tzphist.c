/*  This is the test program for the C-Callable portion of PHIST */
/*  which is a subroutine that produces a histogram.             */

#include "vicmain_c"
#include "ftnbridge.h"

void main44()
{
     int freq[] = { 1,2,3,5,5,6,7,8,9,0,1,5,3,4,5,6,7,8,9,0};
     int freq2[256],freq3[256],freq4[256],freq5[256];
     int ilow,ihigh,ispike,imode,ns,i;

     zvmessage("Test the C interface"," ");
     zvmessage(" "," ");

     ilow = 0;
     ihigh = 19;
     ispike = 1;
     imode = 0;
     ns= 20;

     zphist(freq,ns,ilow,ihigh,ispike,imode);

     ilow = 0;
     ihigh = 254;
     ispike = 1;
     imode = 0;
     ns= 254;

     for (i=1; i<256; i++)
     {
          freq2[i] = i;
     }

     zphist(&freq2[1],ns,ilow,ihigh,ispike,imode);

     ilow = 0;
     ihigh = 154;
     ispike = 125;
     imode = 0;
     ns= 154;

     for (i=1; i<100; i++)
     {
          freq3[i]=0;
     }

     for (i=100; i<256; i++)
     {
          freq3[i] = i;
     }

     zphist(&freq3[1],ns,ilow,ihigh,ispike,imode);

     ilow = 0;
     ihigh = 99;
     ispike = 0;
     imode = 0;
     ns= 100;

     for (i=1; i<101; i++)
     {
          freq4[i] = i;
     }

     zphist(&freq4[1],ns,ilow,ihigh,ispike,imode);

     ilow = 0;
     ihigh = 254;
     ispike = 5;
     imode = 0;
     ns= 255;

     for (i=0; i<255; i++)
     {
          freq5[i] = i;
     }

     zphist(&freq5[0],ns,ilow,ihigh,ispike,imode);

     zvmessage("Test the FORTRAN interface"," ");
     zvmessage(" "," ");

     FTN_NAME(tphist)();

}
