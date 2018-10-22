#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzhistat)(ohist,npts,mean,sigma,maxfreq) 
void *ohist;
int *npts,*maxfreq;
float *mean, *sigma;
{
/*  ============================================  */

      zhistat(ohist,*npts,mean,sigma,maxfreq );
}
/************************************************************************/
void FTN_NAME(tzhistat2)(hist,npts,mean,sigma,mindn,maxdn,maxfreq) 
void *hist;
int *npts,*mindn,*maxdn,*maxfreq;
float *mean, *sigma;
{
/*  ============================================  */

      zhistat2(hist,*npts,mean,sigma,mindn,maxdn,maxfreq );
}
