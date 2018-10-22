/************************************************************************/
/*  C-Callable Version zellipse_radius                                  */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

void zellipse_radius(lat,lon,ra,rb,rc,lora,gcr)
double lat,lon;		/* coordinates of surface point */
double ra,rb,rc;	/* three radii of target body */
double lora;		/* longitude of semi-major axis */
double *gcr;		/* geocentric radius */
{

FTN_NAME2_(ellipse_radius, ELLIPSE_RADIUS) (&lat,&lon,&ra,&rb,&rc,&lora,gcr);
return;
}
