/* Fortran routine pproj_mp rewritten in C,
   Thomas Roatsch, DLR 7-Mar-2001 */
 
/*  DATA = standard 40-word geometry buffer (see subroutine CONVEV) for 
        Perspective case, except that word 37 = equatorial semi-minor axis
 
  LINE,SAMP = object space location of a point
 
  LAT,LON = planetary coordinates of point
            (radians, planetocentri  Lat., West Lon.)
 
  MODE:  1 = (LAT,LON) to (LINE,SAMP)   2 = (LINE,SAMP) to (LAT,LON) 
 
  IND = return indicator.  0=success, 1=failure. (to coincide w/mp_routines)
 
   31oct93  lwk  implemented tri-axial ellipsoid model with extra radius
                in word 31 of DATA
   15may94  lwk  fixed BOP test for triaxial ellipsoid using NAIF calls
   jun96  pxa  cleaned up for use with MP routines, changed failure indicator 
                from 0 to 1, 3rd radius from word 31 to 37 in DATA
   jul96  pxa  replaced naif/spice calls to 'surfnm','vsep',vnorm,and 'halfpi'
		 with relevant in-line code, to remove dependency of naif/
		 spice routines in mp_routines 
   29aug01  lwk  fixed bug in replacement of vnorm code */

#include <math.h>
#include "mp_private.h"

#define eps 0.000001

double mymax(double a, double b, double c)
{
double x;
if (a > b) x=a;
else x=b;
if (c > x) x=c;
return x;
}

int pproj_mp_c(float *data, float *line, float *samp,
               float *lat, float *lon, int imode)

{

int    ibop,i,j;
double cp[3],op[3],cpp[3],rs[3],xnorm[3],m;
double u1[3],u2[3],vtemp[3];
double fl,oal,oas,scale,rp,ra,rb,e1,e2;
double *help = (double *)data;
double om[3][3];
double rlat,rlon,slat,slon,clat,clon;
double d1,rb2,ra2,cln2,sln2,slt2,r;
double a1,b1,c1;
double vmax1,vmax2,vmax3,vmax4,norm1,norm2,norm3;
double vnorm1,vnorm2,vnorm3,vnorm4,vmag1,vmag2,vmag3,vsep1;
double vdot1,s;
double x,y,z,x1,y1,z1;
double a,b,c,d;

fl    = data[26];	/* camera focal length (mm) */
oal   = data[27];	/* optical axis line */
oas   = data[28];	/* optical axis sample */
scale = data[29];	/* camera scale (pix/mm) */
rp    = data[24];       /* polar radius */
ra    = data[25];       /* equatorial semi-major radius */
rb    = data[36];
/* check if rb is garbage, in which case assume oblate spheroid: */
if (rb < rp || rb > ra) rb = ra;
e1 = ra/rp;
e1 = e1*e1;
e2 = rb/rp;
e2 = e2*e2;
/*c OM-matrix (Camera-to-Body coordinates) and RS-vector (body center to camera)
c are stored in the first 24 words of the float buffer:*/
om[0][0] = help[0];
om[0][1] = help[1];
om[0][2] = help[2];
om[1][0] = help[3];
om[1][1] = help[4];
om[1][2] = help[5];
om[2][0] = help[6];
om[2][1] = help[7];
om[2][2] = help[8];

rs[0] = help[9];
rs[1] = help[10];
rs[2] = help[11];

if (imode == 1)
   { /*Here to convert (lat,lon) to (line,samp) */
   rlat = (double) (*lat);
   rlon = (double) (*lon);	
   clat = cos(rlat);
   slat = sin(rlat);
   clon = cos(rlon);
   slon = sin(rlon);
   
   /* COMPUTE GEOCENTRIC RADIUS */
   d1 = rp*rp*clat*clat;
   rb2 = rb*rb;
   ra2 = ra*ra;
   cln2 = clon*clon;
   sln2 = slon*slon;
   slt2 = slat*slat;
   r = (ra*rb*rp)/sqrt(d1*rb2*cln2+d1*ra2*sln2+ra2*rb2*slt2);
   cp[0] = r*clat*clon - rs[0];
   cp[1] = -r*clat*slon - rs[1];
   cp[2] = r*slat - rs[2];

/* *******************************************************************
   The following code replaces the functionality of 'surfnm','vsep',and
   halfpi() routines. This needed to be done to remove spice/naif
   dependencies from mp_routines
   ******************************************************************/

   op[0] = r*clat*clon; 
   op[1] = -r*clat*slon; 
   op[2] = r*slat;

   /* M=MIN(RA,RB,RP) */
   if (ra < rb) m = ra;
   else m = rb;
   if (m > rp) m = rp;
   a1=m/ra;
   b1=m/rb;
   c1=m/rp;

   xnorm[0] = op[0]*(a1*a1);
   xnorm[1] = op[1]*(b1*b1);
   xnorm[2] = op[2]*(c1*c1);

   vmax1 = mymax(fabs(xnorm[0]),fabs(xnorm[1]),fabs(xnorm[2]));

   if (vmax1 == 0) vnorm1 = 0;
   else 
      {
      norm1 = xnorm[0] / vmax1;
      norm2 = xnorm[1] / vmax1;
      norm3 = xnorm[2] / vmax1;
      vnorm1 = vmax1*sqrt(norm1*norm1 + norm2*norm2 + norm3*norm3);
      }
   vmag1 = vnorm1;
   if (vmag1 > 0)
      {
      xnorm[0] = xnorm[0]/vmag1;
      xnorm[1] = xnorm[1]/vmag1;
      xnorm[2] = xnorm[2]/vmag1;
      }
   else
      {
      xnorm[0] = 0;
      xnorm[1] = 0;
      xnorm[2] = 0;
      }
   ibop = 0;

/* Now compute vsep(cp,xnorm), and assign it a value */
   vmax2 = mymax(fabs(cp[0]),fabs(cp[1]),fabs(cp[2]));
  if (vmax2 == 0) vnorm2 = 0;
  else
     {
     norm1 = cp[0] / vmax2;
     norm2 = cp[1] / vmax2;
     norm3 = cp[2] / vmax2;
     vnorm2 = vmax2*sqrt(norm1*norm1 + norm2*norm2 + norm3*norm3);
     }
   vmag2 = vnorm2;
   if (vmag2 > 0)
      {
      u1[0] = cp[0]/vmag2;
      u1[1] = cp[1]/vmag2;
      u1[2] = cp[2]/vmag2;
      }
   else
      {
      u1[0] = 0;
      u1[1] = 0;
      u1[2] = 0;
      }
   if (vmag2 == 0) vsep1 = 0;

   vmax3 = mymax(fabs(xnorm[0]),fabs(xnorm[1]),fabs(xnorm[2]));
   if (vmax3 == 0) vnorm3 = 0;
   else
      {
      norm1 = xnorm[0] / vmax3;
      norm2 = xnorm[1] / vmax3;
      norm3 = xnorm[2] / vmax3;
      vnorm3 = vmax3*sqrt(norm1*norm1 + norm2*norm2 + norm3*norm3);
      }
   vmag3 = vnorm3;
   if (vmag3 > 0)
      {
      u2[0] = xnorm[0]/vmag3;
      u2[1] = xnorm[1]/vmag3;
      u2[2] = xnorm[2]/vmag3;
      } 
      else
         {
	 u2[0] = 0;
	 u2[1] = 0;
	 u2[2] = 0;
         }
   if (vmag3 == 0) vsep1 = 0;
   vdot1 = u1[0]*u2[0] + u1[1]*u2[1] + u1[2]*u2[2];
   if (vdot1 == 0.0) vsep1 = PI_OVER_2;
   else 
      {
      if (vdot1 > 0.0) {
        vtemp[0] = u1[0] - u2[0];
        vtemp[1] = u1[1] - u2[1];
        vtemp[2] = u1[2] - u2[2];
      }
      else {
        vtemp[0] = u1[0] + u2[0];
        vtemp[1] = u1[1] + u2[1];
        vtemp[2] = u1[2] + u2[2];
      }
      vmax4 = mymax(fabs(vtemp[0]),fabs(vtemp[1]),fabs(vtemp[2]));
      if (vmax4 == 0) vnorm4 = 0;
      else
         {
         norm1 = vtemp[0] / vmax4;
         norm2 = vtemp[1] / vmax4;
         norm3 = vtemp[2] / vmax4;
	 vnorm4 = vmax4*sqrt(norm1*norm1 + norm2*norm2 + norm3*norm3);
         }
      }
   if (vdot1 > 0.0) vsep1 = 2.0 * sin(0.5 * vnorm4);
   else vsep1 = PI - 2.0*sin(0.5*vnorm4);
   if (vsep1 < PI_OVER_2) ibop=1; 

   for (i=0; i<3; i++)
      {
      d1 = 0;
      for (j=0; j<3; j++)
         d1 = d1 + om[j][i]*cp[j];
      cpp[i] = d1;
      }
   s = fl*scale/cpp[2];
   *line = (float) (oal + s*cpp[1]);
   *samp = (float) (oas + s*cpp[0]);
   return ibop;

   }


/* ....Here to convert (line,samp) to lat,lon) */
x = (double) (*samp) - oas;
y = (double) (*line) - oal;
z = fl*scale;

for (i=0; i<3; i++)
   cp[i] = om[i][0]*x + om[i][1]*y + om[i][2]*z;

a = e2*cp[0]*cp[0] + e1*cp[1]*cp[1] + e1*e2*cp[2]*cp[2];
b = e2*cp[0]*rs[0] + e1*cp[1]*rs[1] + e1*e2*cp[2]*rs[2];
c = e2*rs[0]*rs[0] + e1*rs[1]*rs[1] + e1*e2*rs[2]*rs[2] - e1*e2*rp*rp;
d = b*b - a*c;
if (d < 0) return 1;

s = (-b-sqrt(d))/a;
for (i=0; i<3; i++)
   op[i] = s*cp[i] + rs[i];

x = op[0];
y = op[1];
z = op[2];
x1 = fabs(x);
y1 = fabs(y);
z1 = fabs(z);
d = sqrt(x*x+y*y);
if(d >= z1*eps) 
   {
   *lat = atan(z/d);         /* geocentric lat.*/
   if(y1 < x1*eps) 
      {
      *lon = 0;
      if(x < 0.) *lon=(float)PI;
      return 0;
      }

   if(x1 < y1*eps)
      {
      *lon = RETURN_RADIANS(270.0);
      if(y < 0) *lon= (float)PI_OVER_2;
      return 0;
      }

   *lon = (float) (TWO_PI - atan2(y,x)) ;
   if(*lon > TWO_PI) *lon=*lon - (float) TWO_PI;
   return 0;
   }

*lat = (float) PI_OVER_2;
if(z < 0) *lat = - (*lat);
*lon = 0;
return 0;
      
}
