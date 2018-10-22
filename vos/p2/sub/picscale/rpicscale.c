#include <math.h>
#include <stdlib.h>
#include <math.h>
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "mp_routines.h" 

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

double zrpd(void);
double zdpr(void);

/*****************************************************************************
 ***   FORTRAN BRIDGE: "rpicscale"                                             
 *****************************************************************************/
void FTN_NAME2 (rpicscale,RPICSCALE) (sbuf,data,mp,mptype,sclat,sclon,r,
						nl,ns,pbuf,line,samp)
double *sbuf;
float  *data;
MP      mp; 
double *sclat,*sclon;
double *r;
int    *mptype,*nl,*ns;
float  *pbuf;
double *line,*samp;
{
    zrpicscale(sbuf,data,mp,*mptype,*sclat,*sclon,*r,*nl,*ns,pbuf,line,samp);
}
/******************************************************************************/
/***   "rzpicscale"  subroutine                                                */
/******************************************************************************/
int zrpicscale(sbuf,data,mp,mptype,sclat,sclon,r,nl,ns,pbuf,line,samp)
double *sbuf;		/* input SPICE buffer */
float  *data;           /* input 40-word geometry buffer for PPROJ call */
MP      mp;		/* Pointer to map projection data */
int     mptype;         /* Map projection type */
double  sclat,sclon;	/* spacecraft latitude/longitude */
double  r;              /* ring radius at which scale is calculated */
int     nl,ns;		/* size of input image */
float  *pbuf;		/* output buffer */
double *line,*samp;	/* output coordinates at which scale applies */
{

   float  fl,oal,oas,scale;
   double srange;
   double scrange,sunrange;	/* spacecraft and sun range from targ center */
   double vsc[3],vsun[3];	/* spacecraft and solar vectors */
   double om[3][3];		/* tranformation from target to camera coords */
   double rlon;			/* ring longitude */
   double p[3],na,noraz,sunaz,scaz,sunlat,sunlon;
   double rscale,ascale;  /* radial and azimuthal ring scale in km/pixel */
   int    i,ind;
	   /*  INITIALIZE VARIABLES */
   for (i=0; i<30; i++) *(pbuf+i) = -999.0; 
   rscale = 1.0e+20;		/* initialize as invalid scale */
   ascale = 1.0e+20;
   zmve(8,9,(sbuf+58),om,1,1);
   zmve(8,3,(sbuf+21),vsc,1,1);
   fl = data[26];
   oal = data[27];
   oas = data[28];
   scale = data[29];

        /* Compute north angle */
   na = atan2(om[2][1],om[2][0])*zdpr();
   if (na < 0.) na=na+360.;
   *(pbuf+2) =(float) na;

   ind = zring_scale(om,vsc,&fl,&oal,&oas,&scale,nl,ns,r,
		&rlon,&srange,line,samp,&rscale,&ascale);
   if (ind == 0) return ind;
   *(pbuf+0) = 1000.*rscale;
   *(pbuf+1) = 1000.*ascale;
   
   p[0] = r*cos(rlon*zrpd());
   p[1] = -r*sin(rlon*zrpd());
   p[2] = 0.0;
	/* Compute solar vector */
   sunrange = *(sbuf+24);		/* sun distance to target center */
   sunlat = *(sbuf+27)*zrpd();		/* Subsolar geocentric latitude */
   sunlon = (360.0 - *(sbuf+28))*zrpd(); /* Subsolar east longitude */
   vsun[0] = sunrange*cos(sunlat)*cos(sunlon);
   vsun[1] = sunrange*cos(sunlat)*sin(sunlon);
   vsun[2] = sunrange*sin(sunlat);

   ind = zrazimuth(om,p,vsc,vsun,&noraz,&scaz,&sunaz);

   *(pbuf+3) = (float) sunaz;
   *(pbuf+4) = (float) scaz;
   *(pbuf+5) = (float) srange;
   *(pbuf+6) =(float) noraz;
   *(pbuf+7) = (float) r;		/* radius-longitude coordinates */
   *(pbuf+8) = (float) rlon;		/* where calculations are done */
   return ind;
}
