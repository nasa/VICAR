/* tztbl is the C program that test the tbl subroutine.  */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
     unsigned char buf[10], tab[256];
     char  ms1[100];
     int k,n;

     sprintf(ms1,"Test the C interface");
     zvmessage(ms1," ");

     for (k=0; k<256; k++)
     {
          tab[k]=k;
     }

     tab[75]=20;      /*  75 => 20 */
     tab[125]=50;     /* 125 => 50 */
     tab[250]=90;     /* 250 => 90 */

     for (k=0; k<10; k++)
     {
          buf[k] = (k+1)*25;
     }

     n=10;

     ztbl(buf, tab, n);

     sprintf(ms1,"  %X  %X  %X  %X  %X  %X  %X  %X  %X  %X",buf[0],buf[1],buf[2],buf[3],buf[4],buf[5],buf[6],buf[7],buf[8],buf[9]);
     zvmessage(ms1," ");

     sprintf(ms1,"Test the FORTRAN interface");
     zvmessage(ms1," ");

     FTN_NAME(ttbl)();

}
