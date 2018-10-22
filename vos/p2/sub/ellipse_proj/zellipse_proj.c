/************************************************************************/
/*  C-Callable Version zellipse_proj                                    */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

int zellipse_proj(om,vsc,fl,oal,oas,scale,ra,rb,rc,v,rline,rsamp)

double om[3][3];	/* Camera to planet transformation matrix */
double vsc[3];  	/* Vector from target center to spacecraft (RS) */
double fl;		/* Camera focal length in mm */
double oal,oas;		/* Optical axis intercept line-sample */
double scale;		/* Picture scale in pixels/mm */
double ra,rb,rc;	/* Target radii */
double v[3];		/* Vector for target center to surface point */
double *rline,*rsamp;	/* Output image coordinates */
{
int ind;		/* =0 if is not on the target, =1 success */
float flx,oalx,oasx,scalex;

flx = fl;
oalx = oal;
oasx = oas;
scalex = scale;
FTN_NAME2_(ellipse_proj, ELLIPSE_PROJ) (om,vsc,&flx,&oalx,&oasx,&scalex,
	&ra,&rb,&rc, v,rline,rsamp,&ind);
return(ind);
}
