/*       Test program for zpmjs        */

#include "xvmaininc.h"
#include "ftnbridge.h"


void FTN_NAME(tzpmjs) (buf)

void *buf;
{
      float *loc; 

      loc = (float *)buf;

      zifmessage("TZPMJS version 01-JULY-1994");
 
      zvmessage("NOMINAL LOCATIONS","");
      zpmjs (loc, 1);

}


