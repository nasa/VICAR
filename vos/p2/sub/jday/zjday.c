#include  "xvmaininc.h"  
#include  "ftnbridge.h"

/************************************************************************/
/*  C-Callable Version zjday - Calcualte Day of Year value              */
/*  (See Fortran Source code of JDAY)                                   */
/************************************************************************/


void zjday (m,d,y,doy)  
int   m;             /* month,         input  */
int   d;             /* day of month,  input  */
int   y;             /* year,          input  */
int   *doy;           /* day of year,    output */
{
FTN_NAME2(jday, JDAY) (&m, &d, &y, doy) ;
}
