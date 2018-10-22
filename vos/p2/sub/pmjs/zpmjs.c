/******************************************************************/
/* C-Callable Version : zpmjs - print grid coordinate information */
/******************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"


void zpmjs ( u, n)
int n;                     /* integer specifying real*4 or real*8 */ 
void *u;                   /* array for grid information          */

{
FTN_NAME(pmjs) ( u, &n);
}

