$!****************************************************************************
$!
$! Build proc for MIPL module triaxtran
$! VPACK Version 1.9, Thursday, June 28, 2001, 08:06:58
$!
$! Execute by entering:		$ @triaxtran
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module triaxtran ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Create_Other .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to triaxtran.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("triaxtran.imake") .nes. ""
$   then
$      vimake triaxtran
$      purge triaxtran.bld
$   else
$      if F$SEARCH("triaxtran.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake triaxtran
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @triaxtran.bld "STD"
$   else
$      @triaxtran.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create triaxtran.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack triaxtran.com -mixed -
	-s triaxtran_c.c triaxtran.f -
	-i triaxtran.imake -
	-o triaxtran.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create triaxtran_c.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create triaxtran.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      subroutine triaxtran(ain,bin,cin,cc,cp,ac,ap,nlimit,
     +     klimit,mlimit,inlat,inlon,infmt,
     +     outlat,outlon,outfmt,ind)

c Routine to convert between any type of lat/lon and any other type of 
c lat/lon on the triaxial ellipsoid.
c Reference: Snyder, Survey Review Vol28, 217, July 1985

c A = major axis radius of the triaxial ellipsoid 
c     at lat=0 eastlon=0.                              input real*8
c     Normalized to A=1, B=B/A, C=C/A
c B = next major axis radius of the triaxial ellipsoid 
c     at lat=0 eastlon=90.                             input real*8
c     Normalized to A=1, B=B/A, C=C/A
c C = polar axis radius of the triaxial ellipsoid.     input real*8
c     Normalized to A=1, B=B/A, C=C/A
c CC = C matrix of coefficients             input      real*8
c      for computing conformal longitude
c      CC is dimensioned cc(nlimit,mlimit)
c CP = C primed matrix of coefficients      input      real*8
c      for computing conformal latitude
c      CP is dimensioned cp(nlimit,mlimit)
c AC = COEF matrix of coefficients          input      real*8
c      for computing authalic longitude
c      AC is dimensioned AC(0:mlimit,0:klimit)
c AP = COEFP primed array of coefficients   input      real*8
c      for computing authalic latitude
c      AP is dimensioned AP(nlimit)
c nlimit= n dimension limit                 input      integer*4
c klimit= k dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c inlat= input latitude in degrees          input      real*8
c inlon= input longitude in degrees east    input      real*8
c infmt= the input lat/lon type             input      integer*4
c outlat= output latitude in degrees        input      real*8
c outlon= output longitude in degrees east  input      real*8
c outfmt= the output lat/lon type           input      integer*4
c ind=0 normal, ind=1 abnormal status       output     integer*4
c The infmt & outfmt codes are:
c  1 means planetocentric latitude & longitude
c  2 means planetodetic latitude & longitude
c  3 means Snyder latitude & longitude
c  4 means conformal latitude & longitude
c  5 means authalic latitude & longitude
c For example, to convert from centric to conformal, infmt=1 & outfmt=4

      implicit real*8 (a-h,o-z)
      real*8 cc(nlimit,mlimit),cp(nlimit,mlimit)
      real*8 ac(0:mlimit,0:klimit),ap(nlimit)
      real*8 inlat,inlon,outlat,outlon
      integer*4 infmt,outfmt

      ind=0
      degtorad=datan(1.d0)/45.d0

      if(ain.eq.1.d0)then
        a=ain
        b=bin
        c=cin
      else
        a=1.d0
        b=bin/ain
        c=cin/ain
      endif
      
      if(outfmt.eq.5)then            ! authalic output

         if(infmt.eq.1)then          ! centric input
            call centric_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_snyder(degtorad,a,b,c,x,y,z,snylat,snylon)
            call authtran(ac,ap,nlimit,klimit,mlimit,snylat,snylon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.2)then     ! detic input
            call detic_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_snyder(degtorad,a,b,c,x,y,z,snylat,snylon)
            call authtran(ac,ap,nlimit,klimit,mlimit,snylat,snylon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.3)then     ! snyder input
            call authtran(ac,ap,nlimit,klimit,mlimit,inlat,inlon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.4)then     ! conformal input
            call conftran(cc,cp,nlimit,mlimit,snylat,snylon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
            call authtran(ac,ap,nlimit,klimit,mlimit,snylat,snylon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.5)then     ! authalic input
            outlat=inlat
            outlon=inlon
         else
           call xvmessage('TRIAXTRAN: Illegal input format.',' ')
           ind=1
         endif

      else if(outfmt.eq.4)then            ! conformal output

         if(infmt.eq.1)then          ! centric input
            call centric_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_snyder(degtorad,a,b,c,x,y,z,snylat,snylon)
            call conftran(cc,cp,nlimit,mlimit,snylat,snylon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.2)then     ! detic input
            call detic_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_snyder(degtorad,a,b,c,x,y,z,snylat,snylon)
            call conftran(cc,cp,nlimit,mlimit,snylat,snylon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.3)then     ! snyder input
            call conftran(cc,cp,nlimit,mlimit,inlat,inlon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else if(infmt.eq.4)then     ! conformal input
            outlat=inlat
            outlon=inlon
         else if(infmt.eq.5)then     ! authalic input
            call authtran(ac,ap,nlimit,klimit,mlimit,snylat,snylon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
            call conftran(cc,cp,nlimit,mlimit,snylat,snylon,
     +                    outlat,outlon,1,ind)
            if(ind.eq.1)return
         else
           call xvmessage('TRIAXTRAN: Illegal input format.',' ')
           ind=1
         endif

      else if(outfmt.eq.3)then            ! snyder output

         if(infmt.eq.1)then          ! centric input
            call centric_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_snyder(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.2)then     ! detic input
            call detic_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_snyder(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.3)then     ! snyder input
            outlat=inlat
            outlon=inlon
         else if(infmt.eq.4)then     ! conformal input
            call conftran(cc,cp,nlimit,mlimit,outlat,outlon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
         else if(infmt.eq.5)then     ! authalic input
            call authtran(ac,ap,nlimit,klimit,mlimit,outlat,outlon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
         else
           call xvmessage('TRIAXTRAN: Illegal input format.',' ')
           ind=1
         endif

      else if(outfmt.eq.2)then            ! detic output

         if(infmt.eq.1)then          ! centric input
            call centric_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_detic(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.2)then     ! detic input
            outlat=inlat
            outlon=inlon
         else if(infmt.eq.3)then     ! snyder input
            call snyder_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_detic(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.4)then     ! conformal input
            call conftran(cc,cp,nlimit,mlimit,snylat,snylon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
            call snyder_to_xyz(degtorad,a,b,c,snylat,snylon,x,y,z)
            call xyz_to_detic(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.5)then     ! authalic input
            call authtran(ac,ap,nlimit,klimit,mlimit,snylat,snylon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
            call snyder_to_xyz(degtorad,a,b,c,snylat,snylon,x,y,z)
            call xyz_to_detic(degtorad,a,b,c,x,y,z,outlat,outlon)
         else
           call xvmessage('TRIAXTRAN: Illegal input format.',' ')
           ind=1
         endif

      else if(outfmt.eq.1)then            ! centric output

         if(infmt.eq.1)then          ! centric input
            outlat=inlat
            outlon=inlon
         else if(infmt.eq.2)then     ! detic input
            call detic_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_centric(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.3)then     ! snyder input
            call snyder_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
            call xyz_to_centric(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.4)then     ! conformal input
            call conftran(cc,cp,nlimit,mlimit,snylat,snylon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
            call snyder_to_xyz(degtorad,a,b,c,snylat,snylon,x,y,z)
            call xyz_to_centric(degtorad,a,b,c,x,y,z,outlat,outlon)
         else if(infmt.eq.5)then     ! authalic input
            call authtran(ac,ap,nlimit,klimit,mlimit,snylat,snylon,
     +                    inlat,inlon,2,ind)
            if(ind.eq.1)return
            call snyder_to_xyz(degtorad,a,b,c,snylat,snylon,x,y,z)
            call xyz_to_centric(degtorad,a,b,c,x,y,z,outlat,outlon)
         else
           call xvmessage('TRIAXTRAN: Illegal input format.',' ')
           ind=1
         endif

      else
        call xvmessage('TRIAXTRAN: Illegal output format.',' ')
        ind=1
      endif

      return
      end


c ********************************************************************
      subroutine xyz_to_centric(degtorad,a,b,c,x,y,z,outlat,outlon)
c converts from xyz on the ellipsoid to planetocentric lat and lon.
c All arguments are real*8
c degtorad = ratio of radians/degrees.
c a= major radius
c b= middle radius
c c= minor radius
c x= ellipsoid x value
c y= ellipsoid y value
c z= ellipsoid z value
c outlat= planetocentric latitude in degrees
c outlon= planetocentric longitude in degrees east

      implicit real*8 (a-z)
c     r=dsqrt(x*x+y*y+z*z)

      if(x.eq.0.d0.and.y.eq.0.d0)then
        if(z.gt.0.d0)then
          outlat=90.d0
          outlon=0.d0
        else
          outlat=-90.d0
          outlon=0.d0
        endif
        return
      endif

      outlat=datan2(z,dsqrt(x*x+y*y))/degtorad
      outlon=datan2(y,x)/degtorad
      if(outlon.lt.0.d0) outlon=360.d0+outlon
      return
      end

c ********************************************************************
      subroutine xyz_to_detic(degtorad,a,b,c,x,y,z,outlat,outlon)
c converts from xyz on the ellipsoid to planetodetic lat and lon.
c All arguments are real*8
c degtorad = ratio of radians/degrees.
c a= major radius
c b= middle radius
c c= minor radius
c x= ellipsoid x value
c y= ellipsoid y value
c z= ellipsoid z value
c outlat= planetodetic latitude in degrees
c outlon= planetodetic longitude in degrees east

      implicit real*8 (a-z)

      if(x.eq.0.d0.and.y.eq.0.d0)then
        if(z.gt.0.d0)then
          outlat=90.d0
          outlon=0.d0
        else
          outlat=-90.d0
          outlon=0.d0
        endif
        return
      endif

      cosphi=(b*b*a*a*z)/
     +            dsqrt(c**4*b**4*x*x+a**4*c**4*y*y+b**4*a**4*z*z)
      if(cosphi.gt.1.d0) cosphi=1.d0
      if(cosphi.lt.-1.d0) cosphi=-1.d0
      phi=dacos(cosphi)/degtorad
      outlat=90.d0-phi
      coslon=b*b*x/dsqrt(b**4*x*x+y*y*a**4)
      if(coslon.gt.1.d0) coslon=1.d0
      if(coslon.lt.-1.d0) coslon=-1.d0
      outlon=dacos(coslon)/degtorad
      if(y.lt.0.d0)outlon=-outlon
      if(outlon.lt.0.d0) outlon=360.d0+outlon

      return
      end

c ********************************************************************
      subroutine xyz_to_snyder(degtorad,a,b,c,x,y,z,outlat,outlon)
c converts from xyz on the ellipsoid to snyder lat and lon.
c All arguments are real*8
c degtorad = ratio of radians/degrees.
c a= major radius
c b= middle radius
c c= minor radius
c x= ellipsoid x value
c y= ellipsoid y value
c z= ellipsoid z value
c outlat= snyder latitude in degrees
c outlon= snyder longitude in degrees east

      implicit real*8 (a-z)

      if(x.eq.0.d0.and.y.eq.0.d0)then
        if(z.gt.0.d0)then
          outlat=90.d0
          outlon=0.d0
        else
          outlat=-90.d0
          outlon=0.d0
        endif
        return
      endif

      outlat=datan2(z,b*dsqrt(1.d0-(z*z)/(c*c)))/degtorad
      outlon=datan2(y,x)/degtorad
      if(outlon.lt.0.d0) outlon=360.d0+outlon
      return
      end

c ********************************************************************
      subroutine snyder_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
c converts from snyder lat/lon to xyz on the triaxial ellipsoid
c All arguments are real*8
c degtorad = ratio of radians/degrees.
c a= major radius
c b= middle radius
c c= minor radius
c inlat= planetocentric latitude in degrees
c inlon= planetocentric longitude in degrees east
c x= ellipsoid x value
c y= ellipsoid y value
c z= ellipsoid z value

      implicit real*8 (a-z)

      if(inlat.ge.90.d0)then
        x=0.d0
        y=0.d0
        z=c
        return
      else if(inlat.le.-90.d0)then
        x=0.d0
        y=0.d0
        z=-c
        return
      endif

      coslat=dcos(inlat*degtorad)
      sinlat=dsin(inlat*degtorad)
      coslon=dcos(inlon*degtorad)
      sinlon=dsin(inlon*degtorad)
      sqrtflon=dsqrt(sinlon**2+(b**2/a**2)*coslon**2)   ! equation 9
      sqrtflat=dsqrt(sinlat**2+(c**2/b**2)*coslat**2)   ! equation 10
      z=c*sinlat/sqrtflat                               ! equation 6
      y=c*coslat*sinlon/(sqrtflon*sqrtflat)             ! equation 11
      x=c*coslat*coslon/(sqrtflon*sqrtflat)             ! equation 8

      return
      end

c ********************************************************************
      subroutine centric_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
c converts from centric lat/lon to xyz on the triaxial ellipsoid
c All arguments are real*8
c degtorad = ratio of radians/degrees.
c a= major radius
c b= middle radius
c c= minor radius
c inlat= planetocentric latitude in degrees
c inlon= planetocentric longitude in degrees east
c x= ellipsoid x value
c y= ellipsoid y value
c z= ellipsoid z value

      implicit real*8 (a-z)

      if(inlat.ge.90.d0)then
        x=0.d0
        y=0.d0
        z=c
        return
      else if(inlat.le.-90.d0)then
        x=0.d0
        y=0.d0
        z=-c
        return
      endif

      lon=inlon
      if(lon.lt.0.d0) lon=360+lon
      tanlat=dtan(inlat*degtorad)
      if(lon.eq.90.d0.or.lon.eq.270.d0)then
        x=0.d0
        y=dsqrt(c*c*b*b/(b*b*tanlat*tanlat+c*c))
        z=tanlat*y
        if(lon.gt.180.d0) y=-y
      else
        tanlon=dtan(inlon*degtorad)
        x=dsqrt(1.d0/(1.d0/a**2 + tanlon**2/b**2 + tanlat**2/c**2 +
     +              tanlon**2*tanlat**2/c**2) )
        if(lon.gt.90.d0.and.lon.lt.270.d0) x=-x
        y=x*tanlon
        z=tanlat*dsqrt(x*x+y*y)      
      endif

      return
      end

c ********************************************************************
      subroutine detic_to_xyz(degtorad,a,b,c,inlat,inlon,x,y,z)
c converts from detic lat/lon to xyz on the triaxial ellipsoid
c All arguments are real*8
c degtorad = ratio of radians/degrees.
c a= major radius
c b= middle radius
c c= minor radius
c inlat= planetodetic latitude in degrees
c inlon= planetodetic longitude in degrees east
c x= ellipsoid x value
c y= ellipsoid y value
c z= ellipsoid z value

      implicit real*8 (a-z)

      if(inlat.ge.90.d0)then
        x=0.d0
        y=0.d0
        z=c
        return
      else if(inlat.le.-90.d0)then
        x=0.d0
        y=0.d0
        z=-c
        return
      endif

      cosang2=(dcos((90.d0-inlat)*degtorad))**2
      coslon2=(dcos(inlon*degtorad))**2
      if(coslon2.ne.1.d0)then                           ! normal
        k1=cosang2*c*c*(1.d0-coslon2/(coslon2-1.d0))
        k2=(1.d0-cosang2)*( (coslon2*a*a)/(coslon2-1.d0)-b*b)
        y2=(1.d0-cosang2)*b**4/(k1-k2)
        if (y2.le.0.d0) then
            y=0.d0     
        else 
            y=dsqrt(y2)
        endif
        x2=(-coslon2*y*y*a**4)/(b**4*(coslon2-1.d0))
      else                                    ! longitude=0 or 180
        y2=0.d0
        y=0.d0
        x2=(a**4) * (1.d0-cosang2)/(cosang2*c*c-cosang2*a*a+a*a)
      endif
      if (x2.le.0.d0) then
           x=0.d0
      else 
           x=dsqrt(x2)
      endif
      x=dsqrt(x2)
      z2=c*c*(1.d0-x2/(a*a)-y2/(b*b))
      if (z2.le.0.d0) then
           z=0.d0
      else 
           z=dsqrt(z2)
      endif
      lon=inlon
      if(lon.lt.0.d0) lon=360+lon
      if(lon.gt.90.d0.and.lon.lt.270.d0) x=-x
      if(lon.gt.180.d0) y=-y
      if(inlat.lt.0.d0)z=-z
      return
      end

c ********************************************************************
      subroutine authtran1(coef,coefp,nlimit,klimit,mlimit,
     +                     lat,lon,alat,alon,ind)

c Routine to convert from Snyder lat/lon to authalic lat/lon.
c Reference: Snyder, Survey Review Vol28, 217, July 1985
c Is called by authtran.

c coef = longitude matrix of coefficients              input      real*8
c coefp= latitude primed array of coefficients        input      real*8
c nlimit= n dimension limit                 input      integer*4
c klimit= k dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c lat= snyder latitude in degrees           input      real*8
c lon= snyder longitude in degrees east     input      real*8
c alat= authalic latitude in degrees        output     real*8
c alon= authalic longitude in degrees east  output     real*8
c ind= return status.  0 is OK, 1 is error condition.  integer

      implicit real*8 (a-h,o-z)
      real*8 coef(0:mlimit,0:klimit),coefp(nlimit)
      real*8 lat,lon,latitude,longitude

      ind=0
      degtorad=datan(1.d0)/45.d0
      if(coef(1,1).eq.0.d0.or.coefp(1).eq.0.d0)then
         call xvmessage('AUTHTRAN1: COEF or COEFP buffer is empty',' ')
         ind=1
         return
      endif

      if(lat.eq.90.d0.or.lat.eq.-90.d0)then
         clat=lat
         clon=lon
         return
      endif
         
      latitude=lat*degtorad
      longitude=lon*degtorad

c     Compute longitude
      alon=longitude
      do m=0,mlimit
         alonn=0.d0
         do k=0,klimit
            alonn=alonn+coef(m,k)*dcos(2*k*latitude)
         enddo
         alon=alon+alonn*dsin(2*m*longitude)
      enddo
      alon=alon/degtorad

c     Compute latitude
      alat=latitude
      do n=1,nlimit
         alat=alat+coefp(n)*dsin(2*n*latitude)
      enddo
      alat=alat/degtorad      

      return
      end

c ********************************************************************
      subroutine conftran1(c,cp,nlimit,mlimit,lat,lon,clat,clon,ind)

c Routine to convert from Snyder lat/lon to conformal lat/lon.
c Reference: Snyder, Survey Review Vol28, 217, July 1985
c Is called by conftran.

c C = C matrix of coefficients              input      real*8
c CP = C primed matrix of coefficients      input      real*8
c nlimit= n dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c lat= snyder latitude in degrees           input      real*8
c lon= snyder longitude in degrees east     input      real*8
c clat= conformal latitude in degrees          output  real*8
c clon= conformal longitude in degrees east    output  real*8
c ind= return status.  0 is OK, 1 is error condition.  integer

      implicit real*8 (a-h,o-z)
      real*8 c(nlimit,mlimit),cp(nlimit,mlimit)
      real*8 lat,lon,latitude,longitude

      ind=0
      degtorad=datan(1.d0)/45.d0
      if(c(1,1).eq.0.d0.or.cp(1,1).eq.0.d0)then
         call xvmessage('CONFTRAN1: C or CP buffer is empty',' ')
         ind=1
         return
      endif

      if(lat.eq.90.d0.or.lat.eq.-90.d0)then
         clat=lat
         clon=lon
         return
      endif
         
      latitude=lat*degtorad
      longitude=lon*degtorad

c        Compute latitude
         t=0.d0
         do m=0,mlimit-1
            t1=0.d0
            do n=1,nlimit
               t1=t1+cp(n,m+1)*dsin(n*latitude)
            enddo
            t=t+t1*dcos(2*m*longitude)
         enddo
         q=dexp(t)*dtan(45.d0*degtorad+latitude/2.d0)
         clat=2.d0*datan(q)/degtorad - 90.d0

c        Compute longitude
         t=0.d0
         do m=1,mlimit
            t1=0.d0
            do n=0,nlimit-1
               t1=t1+c(n+1,m)*dcos(n*latitude)
            enddo
            t=t+t1*dsin(2*m*longitude)
         enddo
         clon=(longitude+t)/degtorad

      return
      end


c ********************************************************************
      subroutine authtran(c,cp,nlimit,klimit,mlimit,slat,slon,
     +                    alat,alon,mode,ind)

c Routine to convert between Snyder lat/lon and authalic lat/lon on
c the triaxial ellipsoid (both directions). 
c Note in that authtran1 only goes from snyder to authalic. To get the
c reverse we fit a polynomial to 3 points near alat & alon and compute
c the reverse from the coefficients.
c Reference: Snyder, Survey Review Vol28, 217, July 1985

c C = C matrix of coefficients              input      real*8
c CP = C primed array of coefficients       input      real*8
c nlimit= n dimension limit                 input      integer*4
c klimit= k dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c slat= snyder latitude in degrees           input/output real*8
c slon= snyder longitude in degrees east     input/output real*8
c alat= authalic latitude in degrees           input/output real*8
c alon= authalic longitude in degrees east     input/output real*8
c mode=1 for snyder to authalic , mode=2 for authalic to snyder
c        input integer*4
c ind=0 normal, ind=1 abnormal status       output     integer*4

      implicit real*8 (a-h,o-z)
      real*8 c(0:mlimit,0:klimit),cp(nlimit)
      real*8 a(3,3),b(3),aa(3,3),bb(3)
      real*8 rlat(3),rlon(3),ala(3),alo(3)

      ind=0
      ipass=0

      if(mode.eq.1)then         ! snyder to authalic

         if(slat.ge.90.d0)then
           alat=90.d0
           alon=slon
           return
         else if(slat.le.-90.d0)then
           alat=-90.d0
           alon=slon
           return
         endif

         call authtran1(c,cp,nlimit,klimit,mlimit,slat,slon,
     +                  alat,alon,ind)

      else if(mode.eq.2)then      ! authalic to snyder

         if(alat.ge.90.d0)then
           slat=90.d0
           slon=alon
           return
         else if(alat.le.-90.d0)then
           slat=-90.d0
           slon=alon
           return
         endif

100      ipass=ipass+1
         if(ipass.eq.1)then
           rlat(1)=alat           ! take authalic as snyder input
           rlon(1)=alon
           delta=.1               ! increment in degrees
         else
           rlat(1)=slat           ! take first iterated snyder as input
           rlon(1)=slon
           delta=.001             ! increment in degrees
         endif

         call authtran1(c,cp,nlimit,klimit,mlimit,rlat(1),rlon(1),
     +                  ala(1),alo(1),ind)
         rlat(2)=rlat(1)-sign(delta,rlat(1))
         rlon(2)=rlon(1)
         call authtran1(c,cp,nlimit,klimit,mlimit,rlat(2),rlon(2),
     +                  ala(2),alo(2),ind)
         rlat(3)=rlat(1)
         rlon(3)=rlon(1)+delta
         if(rlon(3).gt.360.d0) rlon(3)=rlon(1)-delta
         call authtran1(c,cp,nlimit,klimit,mlimit,rlat(3),rlon(3),
     +                  ala(3),alo(3),ind)
         do i=1,3
            a(i,1)=alo(i)
            a(i,2)=ala(i)
            a(i,3)=1.d0
            b(i)=rlat(i)
            aa(i,1)=a(i,1)
            aa(i,2)=a(i,2)
            aa(i,3)=a(i,3)
         enddo
         call dsimq_solution(a,b,3,ind)      ! solve latitude eqn
         if(ind.ne.0) then
           call xvmessage('AUTHTRAN: singular solution on inverse',' ')
           return
         endif
         do i=1,3
            bb(i)=rlon(i)
         enddo
         call dsimq_solution(aa,bb,3,ind)      ! solve longitude eqn
         if(ind.ne.0) then
           call xvmessage('AUTHTRAN: singular solution on inverse',' ')
           return
         endif
         slat=b(1)*alon+b(2)*alat+b(3)
         slon=bb(1)*alon+bb(2)*alat+bb(3)

         if(ipass.eq.1) goto 100

      else
         call xvmessage('AUTHTRAN: Invalid argument for mode',' ')
         ind=1
         return
      endif

      return
      end

c ********************************************************************
      subroutine conftran(c,cp,nlimit,mlimit,slat,slon,clat,clon,mode,
     +     ind)

c Routine to convert between Snyder lat/lon and conformal lat/lon on
c the triaxial ellipsoid (both directions). 
c Note in that conftran1 only goes from snyder to conformal. To get the
c reverse we fit a polynomial to 3 points near clat & clon and compute
c the reverse from the coefficients.
c Reference: Snyder, Survey Review Vol28, 217, July 1985

c C = C matrix of coefficients              input      real*8
c CP = C primed matrix of coefficients      input      real*8
c nlimit= n dimension limit                 input      integer*4
c mlimit= m dimension limit                 input      integer*4
c slat= snyder latitude in degrees           input/output real*8
c slon= snyder longitude in degrees east     input/output real*8
c clat= conformal latitude in degrees           input/output real*8
c clon= conformal longitude in degrees east     input/output real*8
c mode=1 for snyder to conformal, mode=2 for conformal to snyder
c        input integer*4
c ind=0 normal, ind=1 abnormal status       output     integer*4

      implicit real*8 (a-h,o-z)
      real*8 c(nlimit,mlimit),cp(nlimit,mlimit)
      real*8 a(3,3),b(3),aa(3,3),bb(3)
      real*8 rlat(3),rlon(3),cla(3),clo(3)

      ind=0
      ipass=0

      if(mode.eq.1)then         ! snyder to conformal

         if(slat.ge.90.d0)then
           clat=90.d0
           clon=slon
           return
         else if(slat.le.-90.d0)then
           clat=-90.d0
           clon=slon
           return
         endif

         call conftran1(c,cp,nlimit,mlimit,slat,slon,clat,clon,ind)

      else if(mode.eq.2)then      ! conformal to snyder

         if(clat.ge.90.d0)then
           slat=90.d0
           slon=clon
           return
         else if(clat.le.-90.d0)then
           slat=-90.d0
           slon=clon
           return
         endif

100      ipass=ipass+1
         if(ipass.eq.1)then
           rlat(1)=clat           ! take authalic as snyder input
           rlon(1)=clon
           delta=.1               ! increment in degrees
         else
           rlat(1)=slat           ! take first iterated snyder as input
           rlon(1)=slon
           delta=.001             ! increment in degrees
         endif

         call conftran1(c,cp,nlimit,mlimit,rlat(1),rlon(1),
     +                  cla(1),clo(1),ind)
         rlat(2)=rlat(1)-sign(delta,rlat(1))
         rlon(2)=rlon(1)
         call conftran1(c,cp,nlimit,mlimit,rlat(2),rlon(2),
     +                  cla(2),clo(2),ind)
         rlat(3)=rlat(1)
         rlon(3)=rlon(1)+delta
         if(rlon(3).gt.360.d0) rlon(3)=rlon(1)-delta
         call conftran1(c,cp,nlimit,mlimit,rlat(3),rlon(3),
     +                  cla(3),clo(3),ind)
         do i=1,3
            a(i,1)=clo(i)
            a(i,2)=cla(i)
            a(i,3)=1.d0
            b(i)=rlat(i)
            aa(i,1)=a(i,1)
            aa(i,2)=a(i,2)
            aa(i,3)=a(i,3)
         enddo
         call dsimq_solution(a,b,3,ind)      ! solve latitude eqn
         if(ind.ne.0) then
           call xvmessage('CONFTRAN: singular solution on inverse',' ')
           return
         endif
         do i=1,3
            bb(i)=rlon(i)
         enddo
         call dsimq_solution(aa,bb,3,ind)      ! solve longitude eqn
         if(ind.ne.0) then
           call xvmessage('CONFTRAN: singular solution on inverse',' ')
           return
         endif
         slat=b(1)*clon+b(2)*clat+b(3)
         slon=bb(1)*clon+bb(2)*clat+bb(3)

         if(ipass.eq.1) goto 100

      else
         call xvmessage('CONFTRAN: Invalid argument for mode',' ')
         ind=1
         return
      endif

      return
      end

C*********************************************************************
      SUBROUTINE DSIMQ_SOLUTION(A,B,N,KS)
C        PURPOSE
C           OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS,
C           AX=B
C
C        USAGE
C           CALL DSIMQ_SOLUTION(A,B,N,KS)
C
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS
      real*8 A(1),B(1),biga,save,tol
C
C        FORWARD SOLUTION
C
      TOL=0.d0
      KS=0
      JJ=-N
      DO 65 J=1,N
      JY=J+1
      JJ=JJ+N+1
      BIGA=0.d0
      IT=JJ-J
      DO 30 I=J,N
C
C        SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN
C
      IJ=IT+I
      IF(dabs(BIGA)-dabs(A(IJ))) 20,30,30
   20 BIGA=A(IJ)
      IMAX=I
   30 CONTINUE
C
C        TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX)
C
      IF(dabs(BIGA)-TOL) 35,35,40
   35 KS=1
      RETURN
C
C        INTERCHANGE ROWS IF NECESSARY
C
   40 I1=J+N*(J-2)
      IT=IMAX-J
      DO 50 K=J,N
      I1=I1+N
      I2=I1+IT
      SAVE=A(I1)
      A(I1)=A(I2)
      A(I2)=SAVE
C
C        DIVIDE EQUATION BY LEADING COEFFICIENT
C
   50 A(I1)=A(I1)/BIGA
      SAVE=B(IMAX)
      B(IMAX)=B(J)
      B(J)=SAVE/BIGA
C
C        ELIMINATE NEXT VARIABLE
C
      IF(J-N) 55,70,55
   55 IQS=N*(J-1)
      DO 65 IX=JY,N
      IXJ=IQS+IX
      IT=J-IX
      DO 60 JX=JY,N
      IXJX=N*(JX-1)+IX
      JJX=IXJX+IT
   60 A(IXJX)=A(IXJX)-(A(IXJ)*A(JJX))
   65 B(IX)=B(IX)-(B(J)*A(IXJ))
C
C        BACK SOLUTION
C
   70 NY=N-1
      IT=N*N
      DO 80 J=1,NY
      IA=IT-J
      IB=N-J
      IC=N
      DO 80 K=1,J
      B(IB)=B(IB)-A(IA)*B(IC)
      IA=IA-N
   80 IC=IC-1
      RETURN
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create triaxtran.imake
/* Imake file for VICAR subroutine TRIAXTRAN */

#define SUBROUTINE triaxtran

#define MODULE_LIST triaxtran_c.c triaxtran.f

#define P1_SUBLIB

#define USES_C
#define USES_FORTRAN

#define LIB_LOCAL	/* for development, remove on delivery */ 
#define DEBUG		/* for development, remove on delivery */ 
$ Return
$!#############################################################################
$Other_File:
$ create triaxtran.hlp
1 TRIAXTRAN

        Routine to convert from one type of latitude & longitude to another
        type of latitude & longitude on the triaxial ellipsoid.
        Reference: Snyder, Survey Review Vol28, 217, July 1985

  FORTRAN Calling Sequence and arguments:  

      call triaxtran(a,b,c,cc,cp,ac,ap,nlimit,klimit,
     +     mlimit,inlat,inlon,infmt,
     +     outlat,outlon,outfmt,ind)

 A = major axis radius of the triaxial ellipsoid 
     at lat=0 eastlon=0.                              input real*8
 B = next major axis radius of the triaxial ellipsoid 
     at lat=0 eastlon=90.                             input real*8
 C = polar axis radius of the triaxial ellipsoid.     input real*8
 CC = C matrix of coefficients             input      real*8
 CP = C primed matrix of coefficients      input      real*8
 AC = COEF matrix of coefficients          input      real*8
 AP = COEFP array of coefficients          input      real*8
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
  1 means planetocentric latitude & longitude
  2 means planetodetic latitude & longitude
  3 means Snyder latitude & longitude
  4 means conformal latitude & longitude
  5 means authalic latitude & longitude

 For example, to convert from centric to conformal, infmt=1 & outfmt=4

 The CC,CP,AC,AP,nlimit,klimit and mlimit arrays and values come 
 from data stored
 in a file by program TRICOEF. These data are used only for transformations
 involving conformal and authalic lat/lon types (infmt & outfmt types 4 & 5).
 For other formats these data are ignored.


  C Calling Sequence and arguments:  
        
      ztriaxtran(a,b,c,cc,cp,ac,ap,nlimit,klimit,mlimit,
          inlat,inlon,infmt,
          outlat,outlon,outfmt,ind)

 A = major axis radius of the triaxial ellipsoid 
     at lat=0 eastlon=0.                              input double
 B = next major axis radius of the triaxial ellipsoid 
     at lat=0 eastlon=90.                             input double
 C = polar axis radius of the triaxial ellipsoid.     input double
 CC = C matrix of coefficients             input      double
 CP = C primed matrix of coefficients      input      double
 AC = COEF matrix of coefficients          input      real*8
 AP = COEFP array of coefficients          input      real*8
 nlimit= n dimension limit                 input      int
 klimit= k dimension limit                 input      integer*4
 mlimit= m dimension limit                 input      int
 inlat= input latitude in degrees          input      double
 inlon= input longitude in degrees east    input      double
 infmt= the input lat/lon type             input      int
 outlat= output latitude in degrees        input      double
 outlon= output longitude in degrees east  input      double
 outfmt= the output lat/lon type           input      int
 ind=0 normal, ind=1 abnormal status       output     int


2 History

  Original Programmer: J Lorre 9/93
  Source Language: Fortran

2 Operation

  TRIAXTRAN usually requires a set of coefficients written to an IBIS  file
  by program TRICOEF. You must first access this file and read the column 
  appropriate for the planet in question before calling the subroutine.
  A subroutine is available to read the IBIS archive file. It is 
  called GET_ELLIPSOID and is contained inline in program TRICOEF.

2 Arguments

 A = major axis radius of the triaxial ellipsoid 
 at lat=0 eastlon=0.                           
 Radii should be expressed as normalized numbers. Thus A should be
 1.0, B should be B/A, and C should be C/A.
 If you don't normalize the code will do it for you.

 B = next major axis radius of the triaxial ellipsoid 
 at lat=0 eastlon=90.                          
 radii should be expressed as normalized numbers. Thus A should be
 1.0, B should be B/A, and C should be C/A.
 If you don't normalize the code will do it for you.

 C = polar axis radius of the triaxial ellipsoid.  
 radii should be expressed as normalized numbers. Thus A should be
 1.0, B should be B/A, and C should be C/A.
 If you don't normalize the code will do it for you.

 CC = C matrix of coefficients as read from the TRICOEF IBIS file.
 Only used for conformal projections.
 CC is dimensioned cc(nlimit,mlimit)

 CP = C primed matrix of coefficients as read from the TRICOEF IBIS file.
 Only used for conformal projections.
 CP is dimensioned cp(nlimit,mlimit)

 AC = COEF matrix of coefficients as read from the TRICOEF IBIS file.
 Only used for authalic projections.
 AC is dimensioned ac(0:mlimit,0:klimit)

 AP = COEFP matrix of coefficients as read from the TRICOEF IBIS file.
 Only used for authalic projections.
 AP is dimensioned ap(nlimit)

 nlimit= n dimension limit               
 As read from the TRICOEF IBIS file.
 Only used for conformal and authalic projections.
 If you are not requesting either conformal or authalic then this value
 should be 1  .

 klimit= k dimension limit               
 As read from the TRICOEF IBIS file.
 Only used for conformal and authalic projections.
 If you are not requesting either conformal or authalic then this value
 should be 1  .

 mlimit= m dimension limit               
 As read from the TRICOEF IBIS file.
 Only used for conformal and authalic projections.
 If you are not requesting either conformal or authalic then this value
 should be 1  .

 inlat= input latitude in degrees        

 inlon= input longitude in degrees east  

 infmt= the input lat/lon type           
 The infmt & outfmt codes are:
  1 means planetocentric latitude & longitude
  2 means planetodetic latitude & longitude
  3 means Snyder latitude & longitude
  4 means conformal latitude & longitude
  5 means authalic latitude & longitude
 For example, to convert from centric to conformal, infmt=1 & outfmt=4

 outlat= output latitude in degrees    ( RETURNED )
 Outputs are always from -90 to +90

 outlon= output longitude in degrees east  ( RETURNED )
 Outputs are always from 0 to 360

 outfmt= the output lat/lon type         
 The infmt & outfmt codes are:
  1 means planetocentric latitude & longitude
  2 means planetodetic latitude & longitude
  3 means Snyder latitude & longitude
  4 means conformal latitude & longitude
  5 means authalic latitude & longitude
 For example, to convert from centric to conformal, infmt=1 & outfmt=4

 IND is the returned indicator signifying the status of the call.
 ind=0 is normal, ind=1 is abnormal status     



$ Return
$!#############################################################################
