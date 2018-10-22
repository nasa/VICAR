/************************************************************************/
/*  C-Callable Version zring_proj                                       */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

int zring_proj(om,vsc,fl,oal,oas,scale,radius,rlon,rline,rsamp)

double om[3][3];	/* Camera to planet transformation matrix */
double vsc[3];  	/* Vector from target center to spacecraft (RS) */
float *fl;		/* Camera focal length in mm */
float *oal,*oas;	/* Optical axis intercept line-sample */
float *scale;		/* Picture scale in pixels/mm */
double radius,rlon;	/* Radius and longitude of ring point */
double *rline,*rsamp;	/* Output image coordinates */
{
int ind;		/* =0 if is not on the target, =1 success */

FTN_NAME(ring_proj)(om,vsc,fl,oal,oas,scale,
	&radius,&rlon,rline,rsamp,&ind);
return(ind);
}
