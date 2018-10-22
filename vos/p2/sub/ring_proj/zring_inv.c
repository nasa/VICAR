/************************************************************************/
/*  C-Callable Version zring_inv                                        */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

int zring_inv(om,vsc,fl,oal,oas,scale,rline,rsamp,radius,rlon)

double om[3][3];	/* Camera to planet transformation matrix */
double vsc[3];  	/* Vector from target center to spacecraft (RS) */
float *fl;		/* Camera focal length in mm */
float *oal,*oas;	/* Optical axis intercept line-sample */
float *scale;		/* Picture scale in pixels/mm */
double rline,rsamp;	/* Input image coordinates */
double *radius,*rlon;	/* Radius and longitude of ring point */
{
int ind;		/* =0 if is not on the target, =1 success */

FTN_NAME(ring_inv)(om,vsc,fl,oal,oas,scale,
	&rline,&rsamp,radius,rlon,&ind);
return(ind);
}
