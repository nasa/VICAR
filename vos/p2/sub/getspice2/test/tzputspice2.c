/************************************************************************/
/* Test program for C subroutine zputspice2
/************************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "spiceinc.h"

void FTN_NAME(tzputspice2)(unit, source)
int *unit;
char *source;
{
 int ind,zunit;
 int buf[200];

 zunit = *unit;
 ind = zgetspice2(zunit,0,buf);
 if (ind != SUCCESS) {
    zvmessage("***Call to zgetspice2 failed",0);
    return; 
 }

 ind = zputspice2(source,"TZPUTS",buf);
 if (ind != SUCCESS) zvmessage("***Call to zputspice2 failed",0);
 else zvmessage("Call to zputspice2 succeeded",0);
 }
