#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zippcov - CONVERT LINE, SAMP to LAT,LON FOR PERSPECTIVE*/
/************************************************************************/

void zippcov(rlat,rlon,z,x,a,rs,om,e,cl,cs,flag)
float *rlat, *rlon, *z, *x, *cl, *cs, *flag;
double *om, *rs, *e;
float *a;
{
FTN_NAME2(ippcov, IPPCOV) (rlat,rlon,z,x,a,rs,om,e,cl,cs,flag);
}
