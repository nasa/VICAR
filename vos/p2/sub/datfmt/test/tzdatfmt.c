/* Test for subroutine datfmt for C callable         */

#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzdatfmt) ()
{
     char intmsg[8], ascmsg[46];
     unsigned char A[20];
     int I;

     zvmessage("Test the C interface"," ");
     zvmessage(" "," ");

     zvmessage ("*** Testing for flag = 1", " ");
     zdatfmt(1, A, &I);
     
     zprnt (4,1,&I,"   -> The integer date is .");

     sprintf(ascmsg,"   -> The ASCII date is %s", A);
     zvmessage(ascmsg," ");
     zvmessage(" "," ");

     zvmessage ("*** Testing for flag = 2", " ");
     zdatfmt(2, A, &I);

     zprnt (4,1,&I,"   -> The integer date is .");
     
     sprintf(ascmsg,"   -> The ASCII date is %s", A);
     zvmessage(ascmsg," ");
     zvmessage(" "," ");

}
