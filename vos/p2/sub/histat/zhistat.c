#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void   zhistat(ohist,npts,mean,sigma,maxfreq)
void   *ohist;	
int    npts;
float  *mean;	
float  *sigma;	
int    *maxfreq;	

{
FTN_NAME2(histat, HISTAT) (ohist, &npts, mean, sigma, maxfreq);
}
void   zhistat2(hist,npts,mean,sigma,mindn,maxdn,maxfreq)
void   *hist;	
int    npts;
float  *mean;	
float  *sigma;	
int    *mindn,*maxdn,*maxfreq;	

{
FTN_NAME2(histat2, HISTAT2) (hist, &npts, mean, sigma,mindn,maxdn, maxfreq);
}
