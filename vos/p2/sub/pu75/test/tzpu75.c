#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzpu75) ()
{
      float loc[2][1000];
      int   status, inunit, ii, count;

      ii = 1;
      zveaction("SA","");
      zifmessage("TZPU75 version 02-MAY-1994");
 
      status = zvunit(&inunit,"INP",ii,"");
      status = zvopen(inunit,"OPEN_ACT","SA","IO_ACT","SA","");
      status = zvread(inunit,loc,"");

      zpu75 (loc, 1);

      status = zvclose(inunit,"");     
}

