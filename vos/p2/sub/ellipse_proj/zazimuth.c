/************************************************************************/
/*  C-Callable Version zazimuth                                         */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

void  zazimuth(om,op,vsc,vsun,ra,rb,rc,tc_flag,noraz,scaz,sunaz)
double om[3][3];	/* OM matrix */
double op[3];		/* vector from target center to surface point */
double vsc[3];		/* vector from target center to spacecraft */
double vsun[3];		/* vector from target center to sun */
double ra,rb,rc;	/* target vectors */
int    tc_flag;		/* =1 if surface pt is target center, =0 otherwise */
double *noraz;		/* north azimuth */
double *scaz;		/* spacecraft azimuth */
double *sunaz;		/* solar azimuth */
{
FTN_NAME2(azimuth,AZIMUTH)(om,op,vsc,vsun,&ra,&rb,&rc,&tc_flag,
							noraz,scaz,sunaz);
}
