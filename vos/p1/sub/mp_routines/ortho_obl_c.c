/* Fortran routine ortho_obl rewritten in C,
   Thomas Roatsch, DLR 7-Mar-2001 

 CONVERT L,S TO LAT LONG OR LAT,LONG TO L,S FOR THE ORTHOGRAPHIC
 PROJECTION FOR AN OBLATE SPHEROID

 11SEP96 -LWK-  CODE ADAPTED FROM SUBR. TRANV, FOR USE BY MP_ROUTINES
 23Oct97 -Scholten- added check for THR near 90 deg. 
  return  0=O.K.  1=POINT OFF PLANET
  M    1=DIRECT  2=INVERSE

   
DATA
 1    X SPECIAL SAMPLE POINT
 2    Z SPECIAL LINE POINT
 3    TH  SPECIAL LATITUDE
 4    TH1  LATITUDE OF SPECIAL PARALLEL OR SPECIAL OBLIQUE LONGITUDE
 5    TH2  LATITUDE OF SPECIAL PARALLEL
 6    LAM SPECIAL LONGITUDE    WEST
 7    F  SCALE  (KM/PIXEL)
 8    CAS  +1 IF VISIBLE POLE IS N.   -1 IF VISIBLE POLE IS S.
      M  M=2  LINE,SAMPLE TO LAT,LONG   (INVERSE)
      M  M=1  LAT,LONG TO LINE,SAMP  (DIRECT)
 25   RP  POLAR RADIUS  (KM)
 26   RE  EQUATORIAL RADIUS  (KM)
 9    PSI   NORTH ANGLE
 ******  ANGLES IN DATA() IN DEGREES  ******
 ******  LAT,LONG IN RADIANS          ******
 ******  ALL LATITUDES PLANETOCENTRI******
 ******  ALL LONGITUDES WEST          ****** */

#include <math.h>
#include "mp_private.h"

#define SMALL 1e-8

/* special functions */

/* GEOCENTRIC RADIUS */
double gcr (double rpp, double rep, double thr)
{
double help1,help2,help3;

help1 = rpp*cos(thr);
help2 = rep*sin(thr);
help3 = rpp * rep/sqrt(help1*help1 + help2*help2);

return help3;
}

/* GEODETIC LATITUDE */
double phig (double rpp, double rep, double thr)
{
double help;

help = PI_OVER_2 - fabs(atan(-rpp*rpp/(rep*rep)*1.0/tan(thr)));

return help;
}

/* dsign from FORTRAN */
double orthodsign (double a, double b)
{
double dhelp;

if (b < 0) dhelp = - fabs(a);
else       dhelp =    fabs(a);

return dhelp;
}

/* dmod from FORTRAN */
double orthodmod (double a, double b)
{
double dhelp;

dhelp = a - ((int)(a/b)) * b;

return dhelp;
}

int ortho_obl_c(int m, float *data, float *line,
                 float *sample, float *lat, float *longi)
                 
{

double lam,lamr,latr,longr;
double k1,k2,k3,k3sqrt,dif1[3],dif2[3],lambar,north,l;
double lat8,long8;
double xc,zc,th,thr,thr0,f,psi,psir,rp,rpp,re,rep;
double r,phi,x11,z11,x1,y1,z1,x2,y2,z2;
double c1,c2,c3,c4,ca,ce,co,sa,so;
double rlat,rlon,cphi,cpsi;
double sinlat,coslat,sinlon,coslon,sinnor,cosnor,fl;
double req,slccpc,slcspc,scpcsl,scpccl,clcc2p,clcs2p,slcc2p;
double rpsq,resq,sinlam,coslam,sinl,cosl;
double rcp,delx,xdel,delz,zdel,apoiup;
double adel,sindel,cosdel;
double dd,a1,b1,d1,a2,b2;
double alpha,beta,gamma,delta;
double alphsq,betasq,gammsq,deltsq,d1sq,c2sq,b2sq,gresq,drpsq;
double pcpx,pcpy,pcpz,rad1,rad2;

/*  CONVERT ANGLES AND DIMENSIONS TO RADIANS AND PIXELS RESPECTIVELY
    AND float DATA ITEMS TO double */

xc  = (double) data[0];
zc  = (double) data[1];
th  = (double) data[2];
thr = RETURN_RADIANS(th);
if(thr == 0) thr=SMALL;		/* in case center_lat=0 */ 
thr0 = thr;
lam  = (double) data[5];
lamr = RETURN_RADIANS(lam);
f    = (double) data[6];
psi  = (double) data[8];
psir = RETURN_RADIANS(psi);
rp   = (double) data[24];
rpp  = rp / f;
re   = (double) data[25];
rep  = re / f;

if (m == 1 )
   { /* DIRECT */
   lat8  = (double) *lat;
   long8 = (double) *longi;
   latr  = lat8;
   longr = long8;
   r   = gcr(rpp,rep,latr);
   phi = phig(rpp,rep,thr);
   phi = orthodsign(phi,thr);
   x11 = -r*cos(latr)*sin(longr-lamr);
   z11 = r*(sin(phi)*cos(latr)*cos(longr-lamr)-cos(phi)*sin(latr));
   x1  = x11;
   z1  = z11-gcr(rpp,rep,thr)*sin(phi-thr);
   *sample = x1*cos(psir)-z1*sin(psir)+xc;
   *line   = x1*sin(psir)+z1*cos(psir)+zc;

/* BACK-OF-PLANET TEST */
   c1 = cos(thr);
   c2 = cos(TWO_PI-lamr);
   c3 = sin(thr);
   c4 = sin(TWO_PI-lamr);
   ca = cos(latr);
   co = cos(TWO_PI-longr);
   sa = sin(latr);
   so = sin(TWO_PI-longr);
   ce = ca*co*c1*c2+ca*so*c1*c4+sa*c3;	/* COSINE EMISSION ANGLE */
/*  RETURNS .TRUE. IF POINT LAT,LON IS ON BACK OF PLANET W.R.T. TH,LAM */
   if(ce < 0) return 1;
   else return 0;
   }

/*  INVERSE */
rlat = *sample-xc;
rlon = *line-zc;
if( (rlat == 0) && (rlon ==0) )
   {
   *lat   = thr;
   *longi = lamr;
   return 0;
   }

cphi  = th;
cpsi  = lam;
north = psi;
if (fabs(thr) > RETURN_RADIANS(90.0-SMALL))
    thr = orthodsign(RETURN_RADIANS(90.0-SMALL),thr);
sinlat=sin(thr);
coslat=cos(thr);
sinlon=sin(lamr);
coslon=cos(lamr);
sinnor=sin(psir);
cosnor=cos(psir);
fl=rp;
req=re;
slccpc=sinlat*coslon;
slcspc=sinlat*sinlon;
scpcsl=sinlon*coslon*sinlat;
scpccl=sinlon*coslon*coslat;
clcc2p=coslat*coslon*coslon;
clcs2p=coslat*sinlon*sinlon;
slcc2p=sinlat*coslon*coslon;

/* CALC ANGLE LAMBDA BAR */
rpsq=fl;
rpsq=rpsq*rpsq;
resq=req;
resq=resq*resq;
lambar=((coslat*coslat/resq+sinlat*sinlat/rpsq)/
        sqrt((coslat*coslat/(resq*resq)+sinlat*sinlat/(rpsq*rpsq))));
if(lambar > 1) lambar=1;
lambar=acos(lambar);
lambar=RETURN_RADIANS(cphi)+lambar;
sinlam=sin(lambar);
coslam=cos(lambar);
l=RETURN_RADIANS(cphi)-lambar;
sinl=sin(l);
cosl=cos(l);

/* GET RADIUS OF PLANET AT C.P. */
rcp= gcr(rpp,rep,thr);

/* CONVERT FROM PIXELS TO KM */
rcp = f*rcp;

/* CALC.ANGLE BETWEEN UP AND POINT OF INTEREST
   IN PLANE OF PROJECTION SUBTENDED AT CENTER OF PROJECTION */
delx=rlat;
xdel=delx;
delz=rlon;
zdel=delz;
apoiup=atan2(-xdel,-zdel);

/* CALC.SIN AND COS OF THE ANGLE BETWEEN THE DIRECTION OF
   NORTH IN THE IMAGE PLANE AND THE POINT OF INTEREST SUBTENDED AT
   THE CENTER OF PROJECTION */
adel=RETURN_RADIANS(north) + apoiup;
sindel=sin(adel);
cosdel=cos(adel);
if(sindel ==  1) cosdel=0.0;
if(sindel == -1) cosdel=0.0;

/* CALC.DISTANCE OF POINT OF INTEREST FROM
   CENTER OF PROJECTION IN PLANE OF PROJECTION
   AT TRUE SCALE */
dd=f * sqrt( (xdel*xdel) + (zdel*zdel) );

/* CHECK WHETHER POINT OF INTEREST IS OFF PLANET */
if(req < dd) return 1;

/* CALC.COEFFIEIENTS FOR TWO PLANES NORMAL
   TO PLANE OF PROJECTION.

   PLANE 1 - NORMAL TO LINE CONNECTION CENTER OF PROJECTION
   AND POINT OF INTEREST
   PLANE 2 - CONTAINS LINE CONNECTION CENTER OF
   PROJECTION AND POINT OF INTEREST

   PLANE 1 A1*X+B1*Y+C1*Z+D1=0
   PLANE 2 A2*X+B2*Y+C2*Z=0 */

a1=-sindel*sinlon-cosdel*coslon*sinlam;
b1=-sindel*coslon+cosdel*sinlon*sinlam;
c1=cosdel*coslam;
d1=-dd*sindel*sindel+rcp*cosdel*sinlam*coslat
   -rcp*sinlat*coslam*cosdel-dd*cosdel*cosdel*slcc2p*sinlam
   -dd*cosdel*cosdel*coslam*coslam
   -dd*sinlam*sinlam*cosdel*cosdel*sinlon*sinlon;
a2=-cosdel*sinlon*cosl+sindel*slccpc;
b2=-cosdel*coslon*cosl-sindel*slcspc;
c2=-coslat*sindel;

/* CALCULATE PARAMETRIC VARIABLES IN
   SIMULTANEOUS SOLN.OF PLANE 1,PLANE 2,AND SPHEROID */

alpha=a2*c1-a1*c2;
beta=a2*b1-a1*b2;
gamma=b1*c2-b2*c1;
delta=c1*b2-b1*c2;

/* CALCULATE X COORDINATE

   EQUATION IS X=K1+OR-K2*SQRT(K3) */

alphsq=alpha*alpha;
betasq=beta*beta;
gammsq=gamma*gamma;
deltsq=delta*delta;
d1sq=d1*d1;
c2sq=c2*c2;
b2sq=b2*b2;
gresq=gammsq*resq;
drpsq=deltsq*rpsq;
z1=drpsq*(alphsq+gammsq)+betasq*gresq;
k1=((alpha*c2*d1*drpsq)+(beta*b2*d1*gresq))/z1;
k2=(gamma*delta*fl)/z1;
k3=2.0*alpha*c2*beta*b2*resq;
k3=k3+(-c2sq*drpsq-b2sq*gresq-alphsq*b2sq*resq-betasq*resq*c2sq);
k3=k3*d1sq;
k3=k3+(gresq*drpsq+drpsq*resq*alphsq+resq*betasq*gresq);
if(k3 < 0) return 1;
k3sqrt=sqrt(k3);
z1=k2*k3sqrt;
x1=k1+z1;
x2=k1-z1;

/* MAKE THE BACK OF PLANET TEST */

y1=-d1*c2;
y2=y1;
y1=(y1+alpha*x1)/gamma;
y2=(y2+alpha*x2)/gamma;
z1=(-b2*d1+beta*x1)/delta;
z2=(-b2*d1+beta*x2)/delta;

/* (X1,Y1,Z1) IS VECTOR P01
   (X2,Y2,Z2) IS VECTOR P02
   PCP IS VECTOR FROM PLANET CENTER TO CENTER OF PROJECTION
   FIND WHICH VECTOR HAS MINIMUM LENGTH, P01-PCP  OR  P02-PCP */

pcpx=rcp*coslat*coslon;
pcpy=-rcp*coslat*sinlon;
pcpz=rcp*sinlat;
dif1[0]=x1-pcpx;
dif1[1]=y1-pcpy;
dif1[2]=z1-pcpz;
dif2[0]=x2-pcpx;
dif2[1]=y2-pcpy;
dif2[2]=z2-pcpz;
rad1=dif1[0]*dif1[0]+dif1[1]*dif1[1]+dif1[2]*dif1[2];
rad2=dif2[0]*dif2[0]+dif2[1]*dif2[1]+dif2[2]*dif2[2];
if(rad1 <= rad2)
   {
   /* POINT 1 IS VALID */
   rlon=TWO_PI-atan2(y1,x1);
   rlon=orthodmod(rlon+TWO_PI,TWO_PI);
   rlat=(atan(fabs(z1)/sqrt(x1*x1+y1*y1)));
   rlat=orthodsign(rlat,z1);
   *lat=rlat;
   *longi=rlon;
   return 0;
   }
/* POINT 2 IS VALID */
rlon=TWO_PI-atan2(y2,x2);
rlon=orthodmod(rlon+TWO_PI,TWO_PI);
rlat=(atan(fabs(z2)/sqrt(x2*x2+y2*y2)));
rlat=orthodsign(rlat,z2);
*lat=rlat;
*longi=rlon;
return 0;

}
