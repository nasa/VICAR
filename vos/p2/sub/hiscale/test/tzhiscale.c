#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzhiscale)(hist,npts,scale,ohist,lsat,hsat) 
void *hist,*ohist;
int *npts;
float *lsat, *hsat, *scale;
{
/*  ============================================  */
      zhiscale(hist,*npts,*scale,ohist,lsat,hsat);
}
