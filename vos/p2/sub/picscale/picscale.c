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

int   picscal1(float *,double,double,double,double,double *);
void  picscal2(double,double,double,double,double,double,double *); 
void  picscal3(double,double,double,double,double,double,double,double *);
int   psll2xy(float *,double,double,double *,double *);
int   psxy2ll(float *,double,double,double *,double *,float *);
int   xy2ll(MP,double,double,double *,double *);
/*
int  *matrixmult(double *,double *,double *);
*/
double zrpd(void);
double zdpr(void);

/*****************************************************************************
 ***   FORTRAN BRIDGE: "picscale"                                             
 *****************************************************************************/
void FTN_NAME2 (picscale, PICSCALE) (sbuf,data,mp,mptype,sclat,sclon,nl,ns,
							pbuf,line,samp)
double *sbuf;
float  *data;
MP      mp; 
double *sclat,*sclon;
int    *mptype,*nl,*ns;
float  *pbuf;
double *line,*samp;
{
    zpicscale(sbuf,data,mp,*mptype,*sclat,*sclon,*nl,*ns,pbuf,line,samp);
}
/******************************************************************************/
/***   "zpicscale"  subroutine                                                */
/******************************************************************************/
int zpicscale(sbuf,data,mp,mptype,sclat,sclon,nl,ns,pbuf,line,samp)
double *sbuf;		/* input SPICE buffer */
float  *data;           /* input 40-word geometry buffer for PPROJ call */
MP      mp;		/* Pointer to map projection data */
int     mptype;         /* Map projection type */
double  sclat,sclon;	/* spacecraft latitude/longitude */
int     nl,ns;		/* size of input image */
float  *pbuf;		/* output buffer */
double *line,*samp;	/* output coordinates at which scale applies */
{

   double  fl,oal,oas,scale;
   float  srange;
   double scrange,sunrange;	/* spacecraft and sun range from targ center */
   double vsc[3],vsun[3];	/* spacecraft and solar vectors */
   double om[3][3];		/* tranformation from target to camera coords */
   double ra,rb,rc;	/* radii for triaxial ellipse */
   double re,rp;	/* equatorial and polar radius */
   double pcl,pcs;	/* line,rsamp of planet center */
   double rnl,rns;	/* number of lines and samples in image */
   double rsamp,rline;
   double rlat,rlon,rlat2,rlon2;
   double p[3],na,noraz,sunaz,scaz,sunlat,sunlon;
   double hscale,vscale,hscale_pc,vscale_pc;
   double lora;		/* longitude of semi-major axis (unknown) */
   double gcr;		/* geocentric radius */
   double x,y,z,d;
   int    inc,ind,ind1,ind2,ind3,ind4;
   int    ibeg,iend,i,use_pc;
   char   projection[mpMAX_KEYWD_LENGTH];
   char   direction[mpMAX_KEYWD_LENGTH];
	   /*  INITIALIZE VARIABLES */
   for (i=0; i<6; i++) *(pbuf+i) = -999.0; 
   hscale = 1.0e+20;		/* initialize as invalid scale */
   vscale = 1.0e+20;
   rnl = (double) nl; 
   rns = (double) ns; 

   if (mptype!=7 && mptype!=8) {
      ind = mpGetValues(mp,"MAP_PROJECTION_TYPE",projection,"");   
      if (strncmp(projection,"POINT_PERSPECTIVE",17)==0) {
         mptype = 16;
         mpMpo2Buf(mp,data);
         sclat = data[30];
         sclon = data[31];
      }
   }
	/* If image is map projected, compute scale at center of image */
   if (mptype != 7 && mptype != 8 && mptype != 16) {
      ind = mpGetValues(mp,"A_AXIS_RADIUS",&ra,"");   
      if (ind != mpSUCCESS) goto ERROR1;
      ind = mpGetValues(mp,"B_AXIS_RADIUS",&rb,"");   
      if (ind != mpSUCCESS) goto ERROR1;
      ind = mpGetValues(mp,"C_AXIS_RADIUS",&rc,"");   
      if (ind != mpSUCCESS) goto ERROR1;
      ind = mpGetValues(mp,"POSITIVE_LONGITUDE_DIRECTION",direction,"");
      *line = (double) nl/2.0;		/* line,samp at center of image */
      *samp = (double) ns/2.0;
      rline = (double) *line; 
      rsamp = (double) *samp;
      ind = mpxy2ll(mp,rline,rsamp,&rlat,&rlon,1);
      if (ind != mpSUCCESS) goto ERROR2;
      if (strncmp(direction,"WEST",4)==0) rlon=360.-rlon;
      ind = mpxy2ll(mp,rline,rsamp+1.0,&rlat2,&rlon2,1);
      if (ind == mpSUCCESS) {
         if (strncmp(direction,"WEST",4)==0) rlon2=360.-rlon2;
         picscal3(rlat,rlon,rlat2,rlon2,ra,rb,rc,&hscale);
         pbuf[0] = (float) hscale;
      }
      ind = mpxy2ll(mp,rline+1.0,rsamp,&rlat2,&rlon2,1);
      if (ind == mpSUCCESS) {
         if (strncmp(direction,"WEST",4)==0) rlon2=360.-rlon2;
         picscal3(rlat,rlon,rlat2,rlon2,ra,rb,rc,&vscale);
         pbuf[1]=(float) vscale;
      }
      return ind;
   }

/* Here if mptype=7, 8, or 16 (image or object space).  Geometric camera
   distortions will be ignored (image is treated as object-space frame). */

   zmve(8,9,(sbuf+58),om,1,1);
   zmve(8,3,(sbuf+21),vsc,1,1);
   ra = *(sbuf+12);
   rb = *(sbuf+13);
   rc = *(sbuf+14);
   fl = data[26];
   oal = data[27];
   oas = data[28];
   scale = data[29];
   re = (double) data[25];		/* equitorial radius (km) */
   rp = (double) data[24];		/* polar radius */
	/* See if all four corners are on the target body... */
   ind1 = psxy2ll(data,1.,1.,&rlat,&rlon,&srange);  /* upper left corner */
   ind2 = psxy2ll(data,1.,rns,&rlat,&rlon,&srange); /* upper right corner */
   ind3 = psxy2ll(data,rnl,1.,&rlat,&rlon,&srange); /* lower left corner */
   ind4 = psxy2ll(data,rnl,rns,&rlat,&rlon,&srange);/* lower right corner */
   use_pc = 0; 
	/* If all 4 corners are on planet, image is high-resolution */
	/* compute scale at center of image */
   if (ind1==1 && ind2==1 && ind3==1 && ind4==1) { 
      *line = (double) nl/2.0;		/* line,samp at center of image */
      *samp = (double) ns/2.0;
      rline = (double) *line; 
      rsamp = (double) *samp;
      ind1 = psxy2ll(data,rline,rsamp,&rlat,&rlon,&srange);
      ind2 = psxy2ll(data,rline,rsamp+1.,&rlat2,&rlon2,&srange);
      picscal2(rlat,rlon,rlat2,rlon2,re,rp,&hscale); 
      ind3 = psxy2ll(data,rline+1.,rsamp,&rlat2,&rlon2,&srange);
      picscal2(rlat,rlon,rlat2,rlon2,re,rp,&vscale); 
      goto COMPUTE_AZIMUTHS;
   }
	/* Here if 4 corners not on planet (low-resolution image */
	/* Calculate scale at planet center */
   ind = psll2xy(data,sclat,sclon,&pcl,&pcs);
   if (ind != 1) return ind;
   ind = psxy2ll(data,pcl,pcs+0.1,&rlat2,&rlon2,&srange);
   if (ind != 1) return ind;
   picscal2(sclat,sclon,rlat2,rlon2,re,rp,&hscale_pc);	/* horizontal scale */
   hscale_pc*=10.0; /* To accomodate for pcl+0.1 */
   ind = psxy2ll(data,pcl+0.1,pcs,&rlat2,&rlon2,&srange);
   if (ind != 1) return ind;
   picscal2(sclat,sclon,rlat2,rlon2,re,rp,&vscale_pc); 	/* vertical scale */
   vscale_pc*=10.0; /* To accomodate for pcl+0.1 */
   use_pc = 1;
	/* If PC is in the image, use scale at PC */
   if (pcl>0 && pcs>0 && pcl<=rnl && pcs<rns) goto COMPUTE_AZIMUTHS;
	/* If the target is smaller than the width of image, use scale at PC */
   if (1000.*re/hscale > rnl) goto COMPUTE_AZIMUTHS;

	/* Search margin of image for point on planet with the
	   largest scale */
   inc = 20;			/* margin search increment is 20 pixels */
   if (pcl < pcs) {		/* top or right margin */
      if (pcl < rns-pcs) {	/* top margin */
         *line = 1; 
         if (pcs < 1.0) {	/* top-left corner */
            ibeg = 1; 
            iend = 1; 
         } 
         else if (pcs > rns) {  /* top-right corner */
            ibeg = ns; 
            iend = ns; 
         } 
         else {			/* entire top margin */
            ibeg = 1; 
            iend = ns; 
         } 
             /* Search for smallest scale along margin */
         rline = 1.0;
         for (i=ibeg; i<=iend; i+=inc) { 
             rsamp = (double) i; 
             ind = picscal1(data,rline,rsamp,re,rp,&hscale);
             if (ind == 1) *samp = i;
         } 
      }

      else {			/* Right margin */
         *samp = ns;
         if (pcl < 1.0) {	/* top-right corner */
            ibeg = 1; 
            iend = 1; 
         } 
         else if (pcl > rnl) {	/* bottom-right corner */
            ibeg = nl; 
            iend = nl; 
         } 
         else { 		/* entire right margin */
            ibeg = 1; 
            iend = nl; 
         } 

            /* Search for smallest scale along margin */
         rsamp = rns;
         for (i=ibeg; i<=iend; i+=inc) { 
            rline = (double) i; 
            ind = picscal1(data,rline,rsamp,re,rp,&hscale);
            if (ind == 1) *line = i;
         } 
      } 
   }

   else {                     /* left or bottom margins */
       if (pcs < rnl-pcl) {   /* left margin */
          *samp = 1; 
          if (pcl < 1.0) {        /* top-left corner */
             ibeg = 1; 
             iend = 1; 
          }
          else if (pcl > rnl) {    /* bottom-left corner */
             ibeg = nl; 
             iend = nl; 
          }
          else { 
             ibeg = 1; 
             iend = nl; 
          } 
                    /* search for smallest scale along margin */
          rsamp = 1.0;
          for (i=ibeg; i<=iend; i+=inc) { 
             rline = (double) i; 
             ind = picscal1(data,rline,rsamp,re,rp,&hscale);
             if (ind == 1) *line = i; 
          } 
       }
       else { 
           *line = nl; 
           if (pcs < 1.0) {         /* bottom-left corner */
              ibeg = 1; 
              iend = 1; 
           } 
           else if (pcs > rns) {  /* bottom-right corner */
               ibeg = ns; 
               iend = ns; 
           } 
           else { 
               ibeg = 1; 
               iend = ns; 
           } 

                   /* search for smallest scale along margin */
           rline = rnl;
           for (i=ibeg; i<=iend; i+=inc) { 
              rsamp =(double) i; 
              ind = picscal1(data,rline,rsamp,re,rp,&hscale);
              if(ind == 1) *samp = i;
           } 
        } 
     } 

   if (hscale < 1.0e+20) {
      rline = (double) *line; 
      rsamp = (double) *samp; 
      ind = psxy2ll(data,rline,rsamp,&rlat,&rlon,&srange);
      if (ind != 1) return ind;
      ind = psxy2ll(data,rline+1.,rsamp,&rlat2,&rlon2,&srange); 
      if (ind != 1) return ind;
      picscal2(rlat,rlon,rlat2,rlon2,re,rp,&vscale); 
      use_pc = 0;
   }

COMPUTE_AZIMUTHS:
   if (use_pc==1) {
      *line = pcl; 
      *samp = pcs; 
      hscale = hscale_pc;
      vscale = vscale_pc;
      zvmessage("Picture scale calculated at target center","");
   }
   *(pbuf+0) = (float) hscale;	/* horizontal scale in meters/pixel */
   *(pbuf+1) = (float) vscale;  /* vertical scale in meters/pixel */

	/* Compute north angle */
   na = atan2(om[2][1],om[2][0])*zdpr();
   if (na < 0.) na=na+360.;
   *(pbuf+2) =(float) na;

	/* Compute vector to surface point */
   rline = *line;
   rsamp = *samp;
   if (use_pc==1) {
      rline = pcl; 
      rsamp = pcs; 
   }
   ind = zellipse_inv(om,vsc,fl,oal,oas,scale,ra,rb,rc,rline,rsamp,p);

	/* Compute solar vector */
   sunrange = *(sbuf+24);		/* sun distance to target center */
   sunlat = *(sbuf+27)*zrpd();		/* Subsolar geocentric latitude */
   sunlon = (360.0 - *(sbuf+28))*zrpd(); /* Subsolar east longitude */
   vsun[0] = sunrange*cos(sunlat)*cos(sunlon);
   vsun[1] = sunrange*cos(sunlat)*sin(sunlon);
   vsun[2] = sunrange*sin(sunlat);

   zazimuth(om,p,vsc,vsun,ra,rb,rc,use_pc,&noraz,&scaz,&sunaz);
   *(pbuf+3) = (float) sunaz;
   *(pbuf+4) = (float) scaz;
   *(pbuf+5) = srange;
   *(pbuf+6) =(float) noraz;

	/* Compute lat,lon at surface point */
   x = p[0];
   y = p[1];
   z = p[2];
   d = sqrt(x*x + y*y);
   rlat = atan2(z,d)*zdpr();
   rlon = - atan2(y,x)*zdpr();
   if (rlon < 0.) rlon=rlon+360.;
   *(pbuf+7) = (float) rlat;
   *(pbuf+8) = (float) rlon;
   return ind;

ERROR1:
   zvmessage("***Error calling mpGetValues","PICSCALE");
   return ind;   

ERROR2:
   zvmessage("***Error calling mpxy2ll","PICSCALE");
   return ind;
}

/******************************************************************************/
/*  Returns ind=1 if scale is replaced by a smaller value.		      */
/******************************************************************************/
int picscal1(data,rline,rsamp,re,rp,scale)
float   *data;
double  rline,rsamp,re,rp;
double  *scale;
{
   int ind;
   double tscale,rlat,rlon,rlat2,rlon2;
   float srange;

   ind = psxy2ll(data,rline,rsamp,&rlat,&rlon,&srange);
   if (ind != 1) return(0);
   ind = psxy2ll(data,rline,rsamp+1.,&rlat2,&rlon2,&srange);
   if (ind != 1) return(0);
   picscal2(rlat,rlon,rlat2,rlon2,re,rp,&tscale);
   if (tscale < *scale) {
      *scale = tscale;
      return(1);
   }
   return(0);
}

/****************************************************************************** 
    Compute the picture scale
*******************************************************************************/
void picscal2(rlat,rlon,rlat2,rlon2,re,rp,scale) 
double rlat,rlon,rlat2,rlon2,re,rp,*scale;
{
   double  alat,dlat,dlon,gcr,dm,dz,d;

   alat = zrpd()*(rlat+rlat2)/2.0;   /* average latitude */
   dlat = zrpd()*fabs(rlat2-rlat);   /* delta latitude */
   dlon = zrpd()*fabs(rlon2-rlon);   /* delta longitude */
	   /* Compute geocentric radius */
   gcr = re*rp/sqrt(rp*cos(alat)*rp*cos(alat) + re*sin(alat)*re*sin(alat));

   dm  = gcr*dlat;                 /* meridional displacement(km) */
   dz  = gcr*cos(alat)*dlon;       /* zonal displacement(km) */
   d   = sqrt(dm*dm + dz*dz);      /* resultant displacement(km) */

   *scale = 1000.0 * d;           /* output scale in meters/pixel */
   if (*scale < 1.0) *scale = 0.0;
} 

/****************************************************************************
  This routine needs to be corrected PORT/PICSCALE/OLD for triaxial 
  ellipsoid Compute the picture scale.
*****************************************************************************/
void picscal3(rlat,rlon,rlat2,rlon2,ra,rb,rc,scale)
double rlat,rlon,rlat2,rlon2,ra,rb,rc,*scale;
{
  double d1,d2,d3,x,y,z,r1,r2,x2,y2,z2,d;

  rlon = zrpd()*rlon;
  rlon2 = zrpd()*rlon2;
  rlat = zrpd()*rlat;
  rlat2 = zrpd()*rlat2;
  d1 =(rb*rc*cos(rlat)*cos(rlon))*(rb*rc*cos(rlat)*cos(rlon));
  d2 =(ra*rc*cos(rlat)*sin(rlon))*(ra*rc*cos(rlat)*sin(rlon));
  d3 =(ra*rb*sin(rlat))*(ra*rb*sin(rlat));
  r1 =(ra*rb*rc)/sqrt(d1+d2+d3);
   x = r1*cos(rlat)*cos(rlon);
   y = r1*cos(rlat)*sin(rlon);
   z = r1*sin(rlat);
  d1 =(rb*rc*cos(rlat2)*cos(rlon2))*(rb*rc*cos(rlat2)*cos(rlon2));
  d2 =(ra*rc*cos(rlat2)*sin(rlon2))*(ra*rc*cos(rlat2)*sin(rlon2));
  d3 =(ra*rb*sin(rlat2))*(ra*rb*sin(rlat2));
  r2 =(ra*rb*rc) / sqrt(d1 + d2 + d3);
  x2 = r2*cos(rlat2)*cos(rlon2);
  y2 = r2*cos(rlat2)*sin(rlon2);
  z2 = r2*sin(rlat2);
   d = sqrt(((x2-x)*(x2-x)) + ((y2-y)*(y2-y)) + ((z2-z)*(z2-z)));
  *scale = 1000.0 * d;
  if (*scale < 1.0) *scale = 0.0;
}

/******************************************************************************/
/*  This routine multiplies a 3x3 double precision matrix with a 3-element    */
/*  double precision vector and returns a 3-element double precision vector.  */
/******************************************************************************/
/*** txh::Subroutine not being called by any programs and generates compile
 ***      warnings.
int *matrixmult(OMmatrix,vin,vout)
double *OMmatrix,*vin,*vout;
{
   int i,j,k,l;

   for (i=0; i<3; i++) {
      j = i + 0;
      k = i + 3;
      l = i + 6;
      *(vout+i) =(*(OMmatrix+j) * *(vin+0))    
		+(*(OMmatrix+k) * *(vin+1))    
		+(*(OMmatrix+l) * *(vin+2));
   }
}
***/
/******************************************************************************/
/*  This function returns the number of degrees per radian in double precision*/
/*  format by calculating 180/pi.  The value of pi is determined by the acos()*/
/*  function--that is:  zdpr = 180.0 / acos(-1.0).  The first time this func-  */
/*  tion is called,it calculates the value to return.  It saves this value   */
/*  and does not re-calculate it for subsequent calls.                        */
/******************************************************************************/
/* Convert from line,sample to lat,lon */
int psxy2ll(data,rline,rsamp,rlat,rlon,srange)
float  *data,*srange;
double rline,rsamp,*rlat,*rlon;
{
   float zline,zsamp,zlat,zlon,zradius;
   int   ind;

   zline = (float) rline;
   zsamp = (float) rsamp;
   zpproj(data,&zline,&zsamp,&zlat,&zlon,2,0,&zradius,srange,&ind);
   if (ind == 1) {
      *rlat = (double) zlat;
      *rlon = (double) zlon;
   }
   return(ind);
}

/******************************************************************************/
/* Convert from lat,lon to line sample */
int psll2xy(data,rlat,rlon,rline,rsamp)
float  *data;
double rlat,rlon,*rline,*rsamp;
{
   float zline,zsamp,zlat,zlon,zradius,zrange;
   int   ind;

   zlat = (float) rlat;
   zlon = (float) rlon;
   zpproj(data,&zline,&zsamp,&zlat,&zlon,1,0,&zradius,&zrange,&ind);
   if (ind == 1) {
      *rline = (double) zline;
      *rsamp = (double) zsamp;
   }
   else {
      *rline = -999.0;
      *rsamp = -999.0;
   }
   return(ind);
}
