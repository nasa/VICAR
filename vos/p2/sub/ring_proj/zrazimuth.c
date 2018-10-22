/************************************************************************/
/*  C-Callable Version zrazimuth                                        */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

void  zrazimuth(om,op,vsc,vsun,noraz,scaz,sunaz)
double om[3][3];	/* OM matrix */
double op[3];		/* vector from target center to surface point */
double vsc[3];		/* vector from target center to spacecraft */
double vsun[3];		/* vector from target center to sun */
double *noraz;		/* north azimuth */
double *scaz;		/* spacecraft azimuth */
double *sunaz;		/* solar azimuth */
{
FTN_NAME(razimuth) (om,op,vsc,vsun,noraz,scaz,sunaz);
}
