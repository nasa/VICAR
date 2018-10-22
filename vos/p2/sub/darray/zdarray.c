/*  This is the C Callable Subroutine for the darray.f program */
/*  darray converts a double precision data array from single  */
/*  to double dimension or vice versa.                         */

#include "xvmaininc.h"
#include "ftnbridge.h"

void zdarray(mode,i,j,n,m,s,d)

int mode,i,j,n,m;
double *s, *d;
{
     FTN_NAME2(darray, DARRAY) (&mode,&i,&j,&n,&m,s,d); 
     return;
}
