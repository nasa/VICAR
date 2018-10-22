/******************************************************************/
/* C-Callable Version : zpu75 - print grid coordinate information */
/******************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"


void zpu75 ( u, n)
int n;                     /* integer specifying real*4 or real*8 */ 
void *u;                   /* array for grid information          */

{
FTN_NAME(pu75) ( u, &n);
}

