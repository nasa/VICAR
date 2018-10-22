/*  This is a program that will tes the C Callable portion of the 
    TFICOR subroutine.                                             */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
     int inunit,status;
     char buf[7200];
     float x[2];
     char msg[200];

     zvmessage("Test the C Interface"," ");
     zvmessage(" "," ");

     status = zvunit(&inunit, "INP", 1, 0);
     status = zvopen(inunit, 0);

     zficor(inunit, buf, x, 0);
     sprintf(msg,"Mode = 0: %6.4e", x[0]);
     zvmessage(msg," ");
     zvmessage(" "," ");

     zficor(inunit, buf, x, 1);
     sprintf(msg,"Mode = 1: %6.4e", x[0]);
     zvmessage(msg," ");
     zvmessage(" "," ");

     zficor(inunit, buf, x, 2);
     sprintf(msg,"Mode = 2: %6.4e %6.4e", x[0], x[1]);
     zvmessage(msg," ");
     zvmessage(" "," ");

     status = zvclose(inunit,0);

     zvmessage(" "," ");
     zvmessage("Test the FORTRAN Interface"," ");
     zvmessage(" "," ");

     FTN_NAME(tficor)();
     return 0;
}
 
