/************************************************************************/
/* C-Callable Version zring_scale                                       */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

int zring_scale(om,vsc,fl,oal,oas,scale,nl,ns,r,rlon,slant_range,
    rline,rsamp,rscl,ascl)

double om[3][3];	/* Camera to planet transformation matrix */
double vsc[3];  	/* Vector from target center to spacecraft (RS) */
float *fl;		/* Camera focal length in mm */
float *oal,*oas;	/* Optical axis intercept line-sample */
float *scale;		/* Picture scale in pixels/mm */
int nl,ns;		/* Number of lines and samples in image */
double r;		/* Radius of ring point */
double *rlon;		/* Longitude of ring point */
double *slant_range;	/* Distance from ring point to spacecraft */
double *rline,*rsamp;	/* Output image coordinates */
double *rscl,*ascl;	/* Radial and azimuthal picture scale in km/pixel */
{
int ind;		/* =0 if error, =1 success */

FTN_NAME(ring_scale)(om,vsc,fl,oal,oas,scale,&nl,&ns,
	&r,rlon,slant_range,rline,rsamp,rscl,ascl,&ind);
return(ind);
}
