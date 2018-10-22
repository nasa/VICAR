/************************************************************************/
/*  C-Callable Version zring_light                                      */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

void  zring_light(vsc,vsun,r,lon,i,e,p)
double *vsc, *vsun;
double r;		/* ring radius */
double lon;		/* ring longitude */
double *i;		/* incidence angle */
double *e;		/* emission angle */ 
double *p;		/* phase angle */
{
FTN_NAME(ring_light) (vsc,vsun,&r,&lon,i,e,p);
}

















