/* Test for subroutine zxtime for C callable         */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
     char msg[7];
     unsigned char time_now[7];

     zvmessage("Test the C interface"," ");
     zvmessage(" "," ");
     zvmessage("The time in HHMMSS format is:"," ");
     zvmessage(" "," ");

     zxtime(time_now);
     
     sprintf(msg,"%s", time_now);
     zvmessage(msg," ");
     zvmessage(" "," ");

     zvmessage("Test the Fortran interface"," ");
     zvmessage(" "," ");

     FTN_NAME(txtime)();

}
