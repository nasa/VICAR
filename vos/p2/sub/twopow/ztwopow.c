/*  This is the C Callable Subroutine for the twopow.f program */
/*  Returns a 1 if |n| is a power of 2.  pow is an optional    */
/*  argument giving first power of 2 .GE. |n|                  */

#include "xvmaininc.h"
#include "ftnbridge.h"

void ztwopow(result,n, pow)
short int *result, n, *pow;
{
     short int ptemp;

     *result=FTN_NAME(twopow)(&n, &ptemp);
     *pow=ptemp;
}
