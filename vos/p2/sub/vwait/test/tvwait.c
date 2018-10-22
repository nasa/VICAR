#include "vicmain_c"
#include "ftnbridge.h"
#include <time.h>

/* Program to test subroutine vwait().                                       */
main44()
{
   char msg[80];
   time_t time1, time2;
   double delta;
/*  ==================================================================  */
   zvmessage("\nTesting VWAIT subroutine C interface\n", "");
   zvmessage("**suspend execution for approximately 10-seconds", "");
   time(&time1);
   zvwait(1000);   /*  10 times 100  */
   time(&time2);
   delta = difftime(time2,time1);
   sprintf(msg, "Time interval was approximately %3.0f seconds", delta);
   zvmessage("It generally should be 10 or 11 seconds", "");
   zvmessage(msg, "");

   zvmessage("\nTesting VWAIT subroutine Fortran interface\n", "");
   zvmessage("**suspend execution for approximately 10-seconds", "");
   time(&time1);
   FTN_NAME(tvwaitf)();
   time(&time2);
   delta = difftime(time2,time1);
   sprintf(msg, "Time interval was approximately %3.0f seconds", delta);
   zvmessage(msg, "");

   exit(0);
}

