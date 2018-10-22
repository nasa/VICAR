/* triaxtran rewritten in C,
   Th. Roatsch, DLR March 2001 */
#include <math.h>
#include "mp_routines.h"

#define DEGTORAD (atan(1.0)/45.0)

/* mp_dsimq is used from mp_routines.com */
int mp_dsimq();

/* dsign from FORTRAN */
double triaxdsign (double a, double b)
{
double dhelp;

if (b < 0) dhelp = - fabs(a);
else       dhelp =    fabs(a);

return dhelp;
}

/* ********************************************************************/
void triax_xyz_in(double z, double *outlat, double *outlon)
{
if(z > 0)
   {  
   *outlat=90.0;
   *outlon=0.0;
   }
else
   {
   *outlat=-90.0;
   *outlon=0.0;
   }
return;
}
/* ********************************************************************/
int triax_inlat(double inlat, double c, double *x, double *y, double *z)
{
if(inlat >= 90)
   {
   *x=0.0;
   *y=0.0;
   *z=c;
   return -1;
   }
if(inlat <= -90)
   {
   *x=0.0;
   *y=0.0;
   *z=-c;
   return -1;
   }
return 0;
}
/* ********************************************************************/
void xyz_to_centric_c(double b, double c,
                    double x, double y, double z,
                    double *outlat, double *outlon)
/*converts from xyz on the ellipsoid to planetocentric lat and lon.
  b= middle radius
  c= minor radius
  x= ellipsoid x value
  y= ellipsoid y value
  z= ellipsoid z value
  outlat= planetocentri  latitude in degrees
  outlon= planetocentri  longitude in degrees east */
{
if( (x == 0) && (y == 0) )
   {
   triax_xyz_in(z, outlat, outlon);
   return;
   }

*outlat=atan2(z,sqrt(x*x+y*y))/DEGTORAD;
*outlon=atan2(y,x)/DEGTORAD;
if(*outlon < 0) *outlon=360.0 + *outlon;

return;
}
/* ********************************************************************/
void xyz_to_detic_c(double b, double c,
                    double x, double y, double z,
                    double *outlat, double *outlon)
/*converts from xyz on the ellipsoid to planetodeti  lat and lon.
  b= middle radius
  c= minor radius
  x= ellipsoid x value
  y= ellipsoid y value
  z= ellipsoid z value
  outlat= planetodeti  latitude in degrees
  outlon= planetodeti  longitude in degrees east */
{
double b4,c4,cosphi,phi,coslon;

if( (x == 0) && (y == 0) )
   {
   triax_xyz_in(z, outlat, outlon);
   return;
   }
/* a = 1 */
b4 = pow(b,4.0);
c4 = pow(c,4.0);
cosphi = (b*b*z) / sqrt( b4*c4*x*x + c4*y*y + b4*z*z);
if(cosphi >  1) cosphi= 1.0;
if(cosphi < -1) cosphi=-1.0;
phi=acos(cosphi)/DEGTORAD;
*outlat=90.0-phi;
coslon=b*b*x/sqrt(b4*x*x + y*y);
if(coslon >  1) coslon= 1.0;
if(coslon < -1) coslon=-1.0;
*outlon=acos(coslon)/DEGTORAD;
if(y < 0) *outlon=- *outlon;
if(*outlon < 0) *outlon=360.0 + *outlon;

return;
} 
/* ********************************************************************/
void  xyz_to_snyder_c(double b, double c,
                    double x, double y, double z,
                    double *outlat, double *outlon)
/*converts from xyz on the ellipsoid to snyder lat and lon.
  b= middle radius
  c= minor radius
  x= ellipsoid x value
  y= ellipsoid y value
  z= ellipsoid z value
  outlat= snyder latitude in degrees
  outlon= snyder longitude in degrees east */
{
if( (x == 0) && (y == 0) )
   {
   triax_xyz_in(z, outlat, outlon);
   return;
   }
   
*outlat=atan2(z,b*sqrt(1.0-(z*z)/(c*c)))/DEGTORAD;
*outlon=atan2(y,x)/DEGTORAD;
if(*outlon < 0) *outlon=360.0 + *outlon;

return;
}
/* ********************************************************************/
void snyder_to_xyz_c(double b, double c,
                     double inlat, double inlon,
                     double *x, double *y, double *z)
/*converts from snyder lat/lon to xyz on the triaxial ellipsoid
  b= middle radius
  c= minor radius
  inlat= planetocentri  latitude in degrees
  inlon= planetocentri  longitude in degrees east
  x= ellipsoid x value
  y= ellipsoid y value
  z= ellipsoid z value */
{
double sinlat,coslat,sinlon,coslon;
double sqrtflon,sqrtflat;
int    status;

status = triax_inlat(inlat, c, x, y, z);
if (status != 0) return;

/* removed division by a, since a=1 */
coslat=cos(inlat*DEGTORAD);
sinlat=sin(inlat*DEGTORAD);
coslon=cos(inlon*DEGTORAD);
sinlon=sin(inlon*DEGTORAD);
sqrtflon=sqrt(sinlon*sinlon + b*b*coslon*coslon);        /* equation 9  */
sqrtflat=sqrt(sinlat*sinlat + (c*c/b/b)*coslat*coslat);  /* equation 10 */
*z=c*sinlat/sqrtflat;                                    /* equation 6  */
*y=c*coslat*sinlon/(sqrtflon*sqrtflat);                  /* equation 11 */
*x=c*coslat*coslon/(sqrtflon*sqrtflat);                  /* equation 8  */

return;
}
/* ********************************************************************/
void centric_to_xyz_c(double b, double c,
                      double inlat, double inlon,
                      double *x, double *y, double *z)
/*converts from centri  lat/lon to xyz on the triaxial ellipsoid
  b= middle radius
  c= minor radius
  inlat= planetocentri  latitude in degrees
  inlon= planetocentri  longitude in degrees east
  x= ellipsoid x value
  y= ellipsoid y value
  z= ellipsoid z value */
{
double lon,tanlat,tanlon;
int    status;

status = triax_inlat(inlat, c, x, y, z);
if (status != 0) return;

lon=inlon;
if(lon < 0) lon=360 + lon;
tanlat=tan(inlat*DEGTORAD);
if( (lon == 90) || (lon == 270) )
   {
   *x=0.0;
   *y=sqrt(c*c*b*b/(b*b*tanlat*tanlat+c*c));
   *z=tanlat * *y;
   if(lon > 180) *y = - *y;
   }
else
   {
   tanlon=tan(inlon*DEGTORAD);
   *x=sqrt(1.0/(1.0 + tanlon*tanlon/b/b + tanlat*tanlat/c/c +
           tanlon*tanlon*tanlat*tanlat/c/c) );
   if( (lon > 90) && (lon < 270) ) *x = - *x;
   *y = *x * tanlon;
   *z=tanlat*sqrt(*x * *x + *y * *y);
   }      

return;
}
/* ********************************************************************/
void detic_to_xyz_c(double b, double c,
                    double inlat, double inlon,
                    double *x, double *y, double *z)
/*converts from deti  lat/lon to xyz on the triaxial ellipsoid
  b= middle radius
  c= minor radius
  inlat= planetodeti  latitude in degrees
  inlon= planetodeti  longitude in degrees east
  x= ellipsoid x value
  y= ellipsoid y value
  z= ellipsoid z value */
{
double help,cosang2,coslon2,b4,k1,k2,x2,y2,z2,lon;
int    status;

status = triax_inlat(inlat, c, x, y, z);
if (status != 0) return;

help = cos((90.0-inlat)*DEGTORAD);
cosang2 = help*help;
help = cos(inlon*DEGTORAD);
coslon2=help*help;

/* a = 1 ! */
if(coslon2 != 1)
   { /* normal */
   b4=pow(b,4.0);
   k1=cosang2*c*c*(1.0-coslon2/(coslon2-1.0));
   k2=(1.0-cosang2)*( coslon2/(coslon2-1.0) - b*b);
   y2=(1.0-cosang2)*b4/(k1-k2);
   if (y2 <= 0) *y = 0;
   else *y=sqrt(y2);
   x2 = -coslon2 * *y * *y / (b4*(coslon2-1.0));
   }
else  
   { /* longitude=0 or 180 */
   y2=0.0;
   *y=0.0;
   x2 = (1.0-cosang2)/(cosang2*c*c - cosang2 + 1.0);
   }

if (x2 <= 0) *x=0.0;
else *x=sqrt(x2);
z2=c*c*(1.0 - x2 - y2/b/b);
if (z2 <= 0) *z=0.0;
else *z=sqrt(z2);
lon=inlon;
if(lon < 0) lon=360+lon;
if( (lon > 90) && (lon < 270) ) *x = - *x;
if(lon > 180) *y = - *y;
if(inlat < 0) *z = - *z;

return;
}
/* ********************************************************************/
int authtran1_c(double coef[KLIMIT+1][MLIMIT+1],double coefp[NLIMIT],
                double lat, double lon, double *alat, double *alon)

/*Routine to convert from Snyder lat/lon to authali  lat/lon.
  Reference: Snyder, Survey Review Vol28, 217, July 1985
  Is called by authtran.

  coef = longitude matrix of coefficients              input      real*8
  coefp= latitude primed array of coefficients        input      real*8
  lat= snyder latitude in degrees           input      real*8
  lon= snyder longitude in degrees east     input      real*8
  alat= authali  latitude in degrees        output     real*8
  alon= authali  longitude in degrees east  output     real*8
  return status.  0 is OK, 1 is error condition. */
{
double latitude,longitude,alonn;
int    k,n,m;

if( (coef[1][1] == 0) || (coefp[0] == 0) ) return 1;

if( (lat == 90) || (lat == -90) )
   {
   /* was clat,clon in Fortran ! */
   *alat=lat;
   *alon=lon;
   return 0;
   }
   
latitude  = lat*DEGTORAD;
longitude = lon*DEGTORAD;

/* Compute longitude */
*alon=longitude;
for (m=0; m<=MLIMIT; m++)
   {
   alonn=0.0;
   for (k=0; k<=KLIMIT; k++)
      alonn = alonn + coef[m][k] * cos(2*k*latitude);
   *alon = *alon + alonn *sin(2*m*longitude);
   }
*alon = *alon/DEGTORAD;

/* Compute latitude */
*alat = latitude;
for (n=1; n<=NLIMIT; n++)
   *alat = *alat + coefp[n-1] * sin(2*n*latitude);
*alat = *alat/DEGTORAD;
      
return 0;
}
/* ********************************************************************/
int authtran_c(double c[KLIMIT+1][MLIMIT+1], double cp[NLIMIT],
               double *slat, double *slon, double *alat, double *alon,
               int mode)

/*Routine to convert between Snyder lat/lon and authali  lat/lon on
  the triaxial ellipsoid (both directions). 
  Note in that authtran1_  only goes from snyder to authalic. To get the
  reverse we fit a polynomial to 3 points near alat & alon and compute
  the reverse from the coefficients.
  Reference: Snyder, Survey Review Vol28, 217, July 1985

    =   matrix of coefficients              input      real*8
  CP =   primed array of coefficients       input      real*8
  slat= snyder latitude in degrees           input/output real*8
  slon= snyder longitude in degrees east     input/output real*8
  alat= authali  latitude in degrees           input/output real*8
  alon= authali  longitude in degrees east     input/output real*8
  mode=1 for snyder to authali  , mode=2 for authali  to snyder
         input integer*4
  return value 0 normal, ind=1 abnormal status */
{
double a[3][3],b[3],aa[3][3],bb[3];
double rlat[3],rlon[3],ala[3],alo[3];
double delta;
int    ipass, status,i;

ipass = 0;

switch (mode)
   {
   case 1: /* snyder to authalic */
      if(*slat >= 90)
         {
         *alat = 90.0;
         *alon = *slon;
         return 0;
         }
      if(*slat <= -90.0)
         {
         *alat = -90.0;
         *alon = *slon;
         return 0;
         }
      status = authtran1_c(c,cp,*slat,*slon,alat,alon);
      return status;
      
   case 2: /* authalic to snyder */
      if(*alat > 90)
         {
         *slat = 90.0;
         *slon = *alon;
         return 0;
         }
      if(*alat <= -90.0)
         {
         *slat = -90.0;
         *slon = *alon;
         return 0;
         }

      for (ipass=1; ipass <= 2; ipass++)
         {
         if(ipass == 1)
            {
            rlat[0] = *alat;      /* take authalic as snyder input */
            rlon[0] = *alon;
            delta   = 0.1;          /* increment in degrees */
            }
         else
            {
            rlat[0] = *slat;      /* take first iterated snyder as input */
            rlon[0] = *slon;
            delta   = 0.001;      /* increment in degrees */
            }
         status = authtran1_c(c,cp,rlat[0],rlon[0],&ala[0],&alo[0]);
         if (status != 0) return status;
         rlat[1] = rlat[0] - triaxdsign(delta,rlat[0]);
         rlon[1] = rlon[0];
         status = authtran1_c(c,cp,rlat[1],rlon[1],&ala[1],&alo[1]);
         if (status != 0) return status;
         rlat[2] = rlat[0];
         rlon[2] = rlon[0] + delta;
         if(rlon[2] > 360.0) rlon[2] = rlon[0] - delta;
         status = authtran1_c(c,cp,rlat[2],rlon[2],&ala[2],&alo[2]);
         if (status != 0) return status;
         for (i=0; i<3; i++)
            {
            a[0][i]  = alo[i];
            a[1][i]  = ala[i];
            a[2][i]  = 1.0;
            b[i]     = rlat[i];
            aa[0][i] = a[0][i];
            aa[1][i] = a[1][i];
            aa[2][i] = a[2][i];
            }
         status = mp_dsimq(a,b,3);      /* solve latitude eqn */
         if (status != 0) return status;
         for (i=0; i<3; i++) bb[i] = rlon[i];
         status = mp_dsimq(aa,bb,3);    /* solve longitude eqn */
         if (status != 0) return status;
         *slat = b[0]  * *alon+b[1] * *alat+b[2];
         *slon = bb[0] * *alon+bb[1]* *alat+bb[2];
         }
      return 0;
      
   default: return 1;
   }
}
/* ********************************************************************/
int conftran1_c(double c [MLIMIT][NLIMIT], 
                double cp[MLIMIT][NLIMIT],
                double lat, double lon,
                double *clat, double *clon)

/*  Routine to convert from Snyder lat/lon to conformal lat/lon.
  Reference: Snyder, Survey Review Vol28, 217, July 1985
  Is called by conftran.

    =   matrix of coefficients              input      real*8
  CP =   primed matrix of coefficients      input      real*8
  lat= snyder latitude in degrees           input      real*8
  lon= snyder longitude in degrees east     input      real*8
  clat= conformal latitude in degrees          output  real*8
  clon= conformal longitude in degrees east    output  real*8
  return status.  0 is OK, 1 is error condition.  integer */
{
double latitude,longitude;
double t,t1,q;
int    n,m;

if( (c[1][1] == 0) || (cp[1][1] == 0) ) return 1;
if( (lat == 90) || (lat == -90) )
   {
   *clat = lat;
   *clon = lon;
   return 0;
   }
         
latitude  = lat * DEGTORAD;
longitude = lon * DEGTORAD;

/* Compute latitude */
t=0.0;
for (m=0; m<=MLIMIT-1; m++)
   {
   t1=0.0;
   for (n=1; n<=NLIMIT; n++)  
      t1 = t1 + cp[m][n-1] * sin(n*latitude);
   t = t + t1 * cos(2*m*longitude);
   }
q = exp(t) * tan(45.0*DEGTORAD+latitude/2.0);
*clat = 2.0 * atan(q)/DEGTORAD - 90.0;

/* Compute longitude */
t=0.0;
for (m=1; m<=MLIMIT; m++)
   {
   t1=0.0;
   for (n=0; n<=NLIMIT-1; n++)
       t1 = t1 +c[m-1][n] * cos(n*latitude);
   t = t + t1 * sin(2*m*longitude);
   }
*clon = (longitude+t)/DEGTORAD;

return 0;
}
/* ********************************************************************/
int conftran_c(double c[MLIMIT][NLIMIT], double cp[MLIMIT][NLIMIT],
               double *slat, double *slon, double *clat, double *clon,
               int mode)

/*  Routine to convert between Snyder lat/lon and conformal lat/lon on
  the triaxial ellipsoid (both directions). 
  Note in that conftran1_  only goes from snyder to conformal. To get the
  reverse we fit a polynomial to 3 points near clat & clon and compute
  the reverse from the coefficients.
  Reference: Snyder, Survey Review Vol28, 217, July 1985

    =   matrix of coefficients              input      real*8
  CP =   primed matrix of coefficients      input      real*8
  slat= snyder latitude in degrees           input/output real*8
  slon= snyder longitude in degrees east     input/output real*8
  clat= conformal latitude in degrees           input/output real*8
  clon= conformal longitude in degrees east     input/output real*8
  mode=1 for snyder to conformal, mode=2 for conformal to snyder
         input integer*4
  return 0 normal, ind=1 abnormal status */
{
double a[3][3],b[3],aa[3][3],bb[3];
double rlat[3],rlon[3],cla[3],clo[3];
double delta;
int    ipass,i,status;

switch (mode)
   {
   case 1: /* snyder to conformal */
      if(*slat >= 90)
         {
         *clat = 90.0;
         *clon = *slon;
         return 0;
         }
      if(*slat <= -90)
         {
         *clat = -90.0;
         *clon = *slon;
         return 0;
         }
      status = conftran1_c(c,cp,*slat,*slon,clat,clon);
      return status;

   case 2: /* conformal to snyder */
      if(*clat >= 90)
         {
         *slat = 90.0;
         *slon = *clon;
         return 0;
         }
      if(*clat <= -90)
         {
         *slat = -90.0;
         *slon = *clon;
         return 0;
         }
      for (ipass=1; ipass <= 2; ipass++)
         {
         if(ipass == 1)
            {
            rlat[0] = *clat;      /* take authalic as snyder input */
            rlon[0] = *clon;
            delta   = 0.1;        /* increment in degrees */
            }
         else
            {
            rlat[0] = *slat;      /* take first iterated snyder as input */
            rlon[0] = *slon;
            delta   = 0.001;      /* increment in degrees */
            }
 
         status = conftran1_c(c,cp,rlat[0],rlon[0],&cla[0],&clo[0]);
         if (status != 0) return status;
         rlat[1] = rlat[0] - triaxdsign(delta,rlat[0]);
         rlon[1] = rlon[0];
         status = conftran1_c(c,cp,rlat[1],rlon[1],&cla[1],&clo[1]);
         if (status != 0) return status;
         rlat[2] = rlat[0];
         rlon[2] = rlon[0] + delta;
         if(rlon[2] > 360) rlon[2] = rlon[0] - delta;
         status = conftran1_c(c,cp,rlat[2],rlon[2],&cla[2],&clo[2]);
         if (status != 0) return status;
         for (i=0; i<3; i++)
            {
            a[0][i]  = clo[i];
            a[1][i]  = cla[i];
            a[2][i]  = 1.0;
            b[i]     = rlat[i];
            aa[0][i] = a[0][i];
            aa[1][i] = a[1][i];
            aa[2][i] = a[2][i];
            }
         status = mp_dsimq(a,b,3);      /* solve latitude eqn */
         if (status != 0) return status;
         for (i=0; i<3; i++) bb[i] = rlon[i];
         status = mp_dsimq(aa,bb,3);    /* solve longitude eqn */
         if (status != 0) return status;
         *slat = b[0]  * *clon + b[1]  * *clat +b[2];
         *slon = bb[0] * *clon + bb[1] * *clat +bb[2];
         }
      return 0;

   default: return 1;
   }
}
/* ********************************************************************/

int triaxtran_c(double ain, double bin, double cin,
                double cc[MLIMIT][NLIMIT], double cp[MLIMIT][NLIMIT],
                double ac[KLIMIT+1][MLIMIT+1],double ap[NLIMIT],
                double inlat, double inlon, int infmt,
                double *outlat, double *outlon, int outfmt)

/*Routine to convert between any type of lat/lon and any other type of 
  lat/lon on the triaxial ellipsoid.
  Reference: Snyder, Survey Review Vol28, 217, July 1985

  A = major axis radius of the triaxial ellipsoid 
      at lat=0 eastlon=0.                              input real*8
      Normalized to A=1, B=B/A, C=C/A
  B = next major axis radius of the triaxial ellipsoid 
      at lat=0 eastlon=90.                             input real*8
      Normalized to A=1, B=B/A, C=C/A
    = polar axis radius of the triaxial ellipsoid.     input real*8
      Normalized to A=1, B=B/A, C=C/A
  C  =   matrix of coefficients             input      real*8
       for computing conformal longitude
       C  is dimensioned cc(nlimit,mlimit)
  CP =   primed matrix of coefficients      input      real*8
       for computing conformal latitude
       CP is dimensioned cp(nlimit,mlimit)
  A  = COEF matrix of coefficients          input      real*8
       for computing authali  longitude
       A  is dimensioned AC(0:mlimit,0:klimit)
  AP = COEFP primed array of coefficients   input      real*8
       for computing authali  latitude
       AP is dimensioned AP(nlimit)
  nlimit= n dimension limit                 input      integer*4
  klimit= k dimension limit                 input      integer*4
  mlimit= m dimension limit                 input      integer*4
  inlat= input latitude in degrees          input      real*8
  inlon= input longitude in degrees east    input      real*8
  infmt= the input lat/lon type             input      integer*4
  outlat= output latitude in degrees        input      real*8
  outlon= output longitude in degrees east  input      real*8
  outfmt= the output lat/lon type           input      integer*4
  ind=0 normal, ind=1 abnormal status       output     integer*4
  The infmt & outfmt codes are:
   1 means planetocentri  latitude & longitude
   2 means planetodeti  latitude & longitude
   3 means Snyder latitude & longitude
   4 means conformal latitude & longitude
   5 means authali  latitude & longitude
  For example, to convert from centri  to conformal, infmt=1 & outfmt=4 */

{

double b,c,x,y,z,snylat,snylon;
int    status;

if(ain == 1.0)
   {
   b=bin;
   c=cin;
   }
else 
   {
   b=bin/ain;
   c=cin/ain;
   }
      
switch (outfmt)
   {
   case 5: /* authalic output */

   switch (infmt)
      {
      case 1: /* centric input */
         centric_to_xyz_c(b,c,inlat,inlon,&x,&y,&z);
         xyz_to_snyder_c(b,c,x,y,z,&snylat,&snylon);
         status = authtran_c(ac,ap,&snylat,&snylon,outlat,outlon,1);
         if (status == 1) return 1; 
         break;
      case 2: /* detic input */
         detic_to_xyz_c(b,c,inlat,inlon,&x,&y,&z);
         xyz_to_snyder_c(b,c,x,y,z,&snylat,&snylon);
         status = authtran_c(ac,ap,&snylat,&snylon,outlat,outlon,1);
         if (status == 1) return 1;
         break;
      case 3: /* snyder input */
         status = authtran_c(ac,ap,&inlat,&inlon,outlat,outlon,1);
         if (status == 1) return 1;
         break;
      case 4: /* conformal input */
         status = conftran_c(cc,cp,&snylat,&snylon,&inlat,&inlon,2);
         if (status == 1) return 1;
         status = authtran_c(ac,ap,&snylat,&snylon,outlat,outlon,1);
         if (status == 1) return 1;
         break;
      case 5: /* authalic input */
         *outlat=inlat;
         *outlon=inlon;
         break;
      default: /* Illegal input format */
         return -1;
      }
   break;
  
   case 4: /* conformal output */
   
   switch (infmt)
      {
      case 1: /* centric input */
         centric_to_xyz_c(b,c,inlat,inlon,&x,&y,&z);
         xyz_to_snyder_c(b,c,x,y,z,&snylat,&snylon);
         status = conftran_c(cc,cp,&snylat,&snylon,outlat,outlon,1);
         if (status == 1) return 1;
         break;
      case 2: /* detic input */
         detic_to_xyz_c(b,c,inlat,inlon,&x,&y,&z);
         xyz_to_snyder_c(b,c,x,y,z,&snylat,&snylon);
         status = conftran_c(cc,cp,&snylat,&snylon,outlat,outlon,1);
         if (status == 1) return 1;
         break;
      case 3: /* snyder input */
         status = conftran_c(cc,cp,&inlat,&inlon,outlat,outlon,1);
         if (status == 1) return 1;
         break;
      case 4: /* conformal input */
         *outlat=inlat;
         *outlon=inlon;
         break;
      case 5: /* authalic input */
          status = authtran_c(ac,ap,&snylat,&snylon,&inlat,&inlon,2);
         if (status == 1) return 1;
         status = conftran_c(cc,cp,&snylat,&snylon,outlat,outlon,1);
         if (status == 1) return 1;
         break;
      default: /* Illegal input format */
         return -1;
      }
   break;
   
   case 3: /* snyder output */

      switch (infmt)
      {
      case 1: /* centric input */
         centric_to_xyz_c(b,c,inlat,inlon,&x,&y,&z);
         xyz_to_snyder_c(b,c,x,y,z,outlat,outlon);
         break;
      case 2: /* detic input */
         detic_to_xyz_c(b,c,inlat,inlon,&x,&y,&z);
         xyz_to_snyder_c(b,c,x,y,z,outlat,outlon);
        break;
      case 3: /* snyder input */
         *outlat=inlat;
         *outlon=inlon;
         break;
      case 4: /* conformal input */
          status = conftran_c(cc,cp,outlat,outlon,&inlat,&inlon,2);
         if (status == 1) return 1;
         break;
      case 5: /* authalic input */
         status = authtran_c(ac,ap,outlat,outlon,&inlat,&inlon,2);
         if (status == 1) return 1;
         break;
      default: /* Illegal input format */
         return -1;
      }
   break;
   
   case 2: /* detic output */
   
      switch (infmt)
      {
      case 1: /* centric input */
         centric_to_xyz_c(b,c,inlat,inlon,&x,&y,&z);
         xyz_to_detic_c(b,c,x,y,z,outlat,outlon);
         break;
      case 2: /* detic input */
         *outlat=inlat;
         *outlon=inlon;
        break;
      case 3: /* snyder input */
         snyder_to_xyz_c(b,c,inlat,inlon,&x,&y,&z);
         xyz_to_detic_c(b,c,x,y,z,outlat,outlon);
         break;
      case 4: /* conformal input */
         status = conftran_c(cc,cp,&snylat,&snylon,&inlat,&inlon,2);
         if (status == 1) return 1;
         snyder_to_xyz_c(b,c,snylat,snylon,&x,&y,&z);
         xyz_to_detic_c(b,c,x,y,z,outlat,outlon);
         break;
      case 5: /* authalic input */
         status = authtran_c(ac,ap,&snylat,&snylon,&inlat,&inlon,2);
         if (status == 1) return 1;
         snyder_to_xyz_c(b,c,snylat,snylon,&x,&y,&z);
         xyz_to_detic_c(b,c,x,y,z,outlat,outlon);
         break;
      default: /* Illegal input format */
         return -1;
      }
   break;
   
   case 1: /* centric output */
   
      switch (infmt)
      {
      case 1: /* centric input */
         *outlat=inlat;
         *outlon=inlon;
         break;
      case 2: /* detic input */
         detic_to_xyz_c(b,c,inlat,inlon,&x,&y,&z);
         xyz_to_centric_c(b,c,x,y,z,outlat,outlon);
        break;
      case 3: /* snyder input */
         snyder_to_xyz_c(b,c,inlat,inlon,&x,&y,&z);
         xyz_to_centric_c(b,c,x,y,z,outlat,outlon);
         break;
      case 4: /* conformal input */
         status = conftran_c(cc,cp,&snylat,&snylon,&inlat,&inlon,2);
         snyder_to_xyz_c(b,c,snylat,snylon,&x,&y,&z);
         xyz_to_centric_c(b,c,x,y,z,outlat,outlon);
         break;
      case 5: /* authalic input */
         status = authtran_c(ac,ap,&snylat,&snylon,&inlat,&inlon,2);
         if (status == 1) return 1;
         snyder_to_xyz_c(b,c,snylat,snylon,&x,&y,&z);
         xyz_to_centric_c(b,c,x,y,z,outlat,outlon);
         break;
      default: /* Illegal input format */
         return -1;
      }
   break;
   
   default: /* Illegal output format */
      return -2;
   }

return 0;
}

