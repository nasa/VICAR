/************************************************************************
 * get_seconds.c 
 ************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <zvproto.h>
#include <stdio.h>
#include <time.h>

#define Y2K_CORRECTION 0X7FFFFFFF

/* Y2K_CORRECTION will turn off the first bit when time returned by
   time() function will be greater then 2**31 - 1, thus forcing the
   value to be positive in 2's complement for the calling fortran
   program. */

#define TEST_PURPOSE   0X00000000

/* Only used for purpose of testing. Set the flag to some hex value such
   as 0XC0000000 to forward time in future. */
/************************************************************************/
/* Fortran-Callable Version (no C-version needed)                       */
/************************************************************************/


void FTN_NAME2_(get_seconds, GET_SECONDS) (sec_out)
long *sec_out;
{
  time_t thetime;
  char msg[100];
  thetime = time(NULL);

  if(TEST_PURPOSE) {
     zvmessage("*************************************************","");
     zvmessage("*** This is test of get_seconds for Y2K only. ***","");
     zvmessage("*** This is simulation of how program will    ***","");
     zvmessage("*** behave once the time function starts      ***","");
     zvmessage("*** returning values greater then 2**31-1     ***","");
     zvmessage("*************************************************","");
     sprintf(msg,"Time returned by time function is %u",(int)thetime);
     zvmessage(msg,"");
     thetime+=TEST_PURPOSE;
  }

  *sec_out = (long) (thetime & Y2K_CORRECTION);

  if(TEST_PURPOSE) {
    sprintf(msg,"Time after offseted by get_seconds is %u",(int)thetime);
     zvmessage(msg,"");
     sprintf(msg,"Time after corrected by get seconds is %d",(int)*sec_out);
     zvmessage(msg,"");
  }

  return;
}
