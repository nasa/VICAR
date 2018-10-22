/*   Test program for the C-Callable VGROS Subroutine.                  */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
     int icam,i;
     float oloc[202][2];
     char msg[100];

     zvmessage("Test the C interface"," ");
     zvmessage(" "," ");

     for (icam=4; icam<8; icam++)
     {
          zvgros(icam,oloc);
          sprintf(msg,"Voyager OS reseau for camera= %d", icam);
          zvmessage(msg," ");
          zvmessage(" "," ");
          for (i=0;i<191; )
          {
               sprintf(msg,"%6d %6d %6d %6d %6d %6d %6d %6d %6d %6d",i+1,i+2,i+3,i+4,i+5,i+6,i+7,i+8,i+9,i+10);
               zvmessage(msg," ");
               sprintf(msg,"%6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f",oloc[i][0],oloc[i+1][0],oloc[i+2][0],oloc[i+3][0],oloc[i+4][0],oloc[i+5][0],oloc[i+6][0],oloc[i+7][0],oloc[i+8][0],oloc[i+9][0]);
               zvmessage(msg," ");
               sprintf(msg,"%6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f %6.1f",oloc[i][1],oloc[i+1][1],oloc[i+2][1],oloc[i+3][1],oloc[i+4][1],oloc[i+5][1],oloc[i+6][1],oloc[i+7][1],oloc[i+8][1],oloc[i+9][1]);
               zvmessage(msg," ");
               zvmessage(" "," ");
               if (i == 190)
               {
                    zvmessage("   201   202"," ");
                    sprintf(msg,"%6.1f %6.1f",oloc[200][0],oloc[201][0]);
                    zvmessage(msg," ");
                    sprintf(msg,"%6.1f %6.1f",oloc[200][1],oloc[201][1]);
                    zvmessage(msg," ");
                    zvmessage(" "," ");
               }
               
               i = i+10;
          }              
     }

     zvmessage("Test the Fortran Interface"," ");
     zvmessage(" "," ");

     FTN_NAME(tvgros)();

}
