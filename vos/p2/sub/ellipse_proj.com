$!****************************************************************************
$!
$! Build proc for MIPL module ellipse_proj
$! VPACK Version 1.9, Monday, December 07, 2009, 16:16:38
$!
$! Execute by entering:		$ @ellipse_proj
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
$!   TEST        Only the test files are created.
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
$ write sys$output "*** module ellipse_proj ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to ellipse_proj.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
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
$   if F$SEARCH("ellipse_proj.imake") .nes. ""
$   then
$      vimake ellipse_proj
$      purge ellipse_proj.bld
$   else
$      if F$SEARCH("ellipse_proj.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ellipse_proj
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ellipse_proj.bld "STD"
$   else
$      @ellipse_proj.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ellipse_proj.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ellipse_proj.com -mixed -
	-s ellipse_proj.f ellipse_inv.f ellipse_radius.f azimuth.f -
	   zellipse_proj.c zellipse_inv.c zellipse_radius.c zazimuth.c -
	-i ellipse_proj.imake -
	-t tellipse_proj.c tellipse_proj.pdf tellipse_proj.imake -
	   tstellipse_proj.pdf -
	-o ellipse_proj.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ellipse_proj.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Given the vector from target center to surface point, compute the
C (line,sample) location of the image of the surface point in a perspective
C projection camera system.
C
      SUBROUTINE ELLIPSE_PROJ(OM,VSC,FL,OAL,OAS,SCALE,RA,RB,RC,
     &		V,rline,rsamp,ind)
      IMPLICIT NONE
      REAL*8 OM(3,3)		!Camera to planet transformation matrix
      REAL*8 VSC(3)		!Vector from target center to spacecraft (RS)
      REAL*4 FL			!Camera focal length in mm 
      REAL*4 OAL,OAS		!Optical axis intercept line-sample
      REAL*4 SCALE 		!Picture scale in pixels/mm
      REAL*8 RA,RB,RC		!Target radii
      REAL*8 V(3)		!Vector from target center to surface point
      REAL*8 RLINE,RSAMP 	!Output image coordinates
      INTEGER*4 IND		! =1 if RLINE,RSAMP are valid, =0 otherwise

      REAL*8 x0,y0,z0,xc,yc,zc,S,DOT

C     ...Compute vector from camera to surface point
      xc = V(1) - VSC(1)
      yc = V(2) - VSC(2)
      zc = V(3) - VSC(3)

C     ...Compute dot product of vector with surface normal
      DOT = xc*(v(1)/ra) + yc*(v(2)/rb) + zc*(v(3)/rc)
      IF (DOT .GT. 0.) THEN	!Point is on back side of target
	IND = 0
        RETURN
      ENDIF

C     ...Rotate vector into camera coordinates
      x0 = OM(1,1)*xc + OM(1,2)*yc + OM(1,3)*zc
      y0 = OM(2,1)*xc + OM(2,2)*yc + OM(2,3)*zc
      z0 = OM(3,1)*xc + OM(3,2)*yc + OM(3,3)*zc

C     ...Scale vector into pixels
      S = FL*SCALE/z0			!pixels/km
      RLINE = S*y0 + OAL
      RSAMP = S*x0 + OAS
      IND = 1
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ellipse_inv.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given the (line,sample) coordinates of a point on an ellipsoid target
C surface, compute the vector from target center to surface point.
C The line-sample coordinates are assumed to be in object space.
C
      SUBROUTINE ELLIPSE_INV(OM,VSC,FL,OAL,OAS,SCALE,RA,RB,RC,
     &		RLINE,RSAMP,v,ind)
      IMPLICIT NONE
      REAL*8 OM(3,3)		!Camera to planet transformation matrix
      REAL*8 VSC(3)		!Vector from target center to spacecraft (RS)
      REAL*4 FL			!Camera focal length in mm 
      REAL*4 OAL,OAS		!Optical axis intercept line-sample
      REAL*4 SCALE 		!Picture scale in pixels/mm
      REAL*8 RA,RB,RC		!Target radii
      REAL*8 RLINE,RSAMP 	!Input image coordinates
      REAL*8 V(3)		!Vector from target center to surface point
      INTEGER*4 IND		! =0 if is not on the target, =1 success

      real*8 x0,y0,z0,xc,yc,zc,s,a,b,c,d
      real*8 x1,y1,z1

C     ...Vector from spacecraft to surface point in camera coordinate system
      x0 = rsamp - oas
      y0 = rline - oal
      z0 = fl*scale

C     ...Convert vector to target-centered coordinates
      xc = OM(1,1)*x0 + OM(2,1)*y0 + OM(3,1)*z0
      yc = OM(1,2)*x0 + OM(2,2)*y0 + OM(3,2)*z0
      zc = OM(1,3)*x0 + OM(2,3)*y0 + OM(3,3)*z0

c Solve for the scale factor s which extends the vector to where it intersects
c the target surface...

      x0 = xc/ra
      y0 = yc/rb
      z0 = zc/rc

      x1 = vsc(1)/ra
      y1 = vsc(2)/rb
      z1 = vsc(3)/rc

      a = x0*x0 + y0*y0 + z0*z0
      b = x0*x1 + y0*y1 + z0*z1
      c = x1*x1 + y1*y1 + z1*z1 - 1.
      d = b*b - a*c

      ind = 0
      if (d .lt. 0.) return       !The vector does not intercept the target

C There are two intercepts (+b and -b).  The shorter one is visible,
C the other is on the back side of the target.

      s = (-b-dsqrt(d))/a      !s*cp is the vector from s/c to surface point
      v(1) = s*xc + vsc(1)
      v(2) = s*yc + vsc(2)
      v(3) = s*zc + vsc(3)
      ind = 1
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ellipse_radius.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Return radius for a given lat,lon.
C Longitudes may be either east or west.
C
      subroutine ellipse_radius(lat,lon,ra,rb,rc,lora,gcr)
      implicit none
      real*8 lat,lon		!Input lat,lon coordinates
      real*8 ra,rb,rc		!Input target body radii
      real*8 lora		!Longitude of semi-major axis
      real*8 gcr		!geocentric radius
      real*8 rlat,rlon,clat,slat,clon,slon,dtor,PI

      PI = 3.141592653589793D0
      dtor = PI/180.d0		!degrees to radians
      rlat = lat*dtor		!convert to radians
      rlon = (lon-lora)*dtor
      clat = dcos(rlat)
      slat = dsin(rlat)
      clon = dcos(rlon)
      slon = dsin(rlon)
      gcr = 1.d0/dsqrt((clat*clon/ra)**2+(clat*slon/rb)**2+(slat/rc)**2)
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create azimuth.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Compute north azimuth, spacecraft azimuth, and solar azimuth angles.
C The azimuth angles are measured at the surface point defined by the vector V.
C To compute the solar azimuth, the solar vector is (1) projected into the
C plane tangent to the surface point, and (2) this projected vector is then
C projected into the image plane.  The solar azimuth is the angle between a
C horizontal line pointing to the right in the image and this projected vector,
C measured in a clockwise direction.  The other azimuths are defined similarly.
C
      subroutine azimuth(om,op,vsc,vsun,ra,rb,rc,flg,noraz,scaz,sunaz)
      implicit none
      real*8 om(3,3)		!Target to camera transformation (OM matrix)
      real*8 op(3)		!Vector from target center to surface point
      real*8 vsc(3)		!Vector from target center to S/C (RS vector)
      real*8 vsun(3)		!Vector from target center to sun
      real*8 ra,rb,rc		!Target radii
      integer*4 flg		!=1 if surface pt is target center, =0 otherwise
      real*8 noraz		!Output north azimuth (degrees)
      real*8 scaz		!Output spacecraft azimuth (degrees)
      real*8 sunaz		!Output solar azimuth (degrees)

      real*8 n(3),s(3),c(3),v(3),m(3,3),r,rlat,rlon,halfpi,dpr
      integer i

C     ...Compute vector from target center to north pole
      n(1) = 0.
      n(2) = 0.
      n(3) = rc

C     ...Compute the three vectors of interest
      do i=1,3
         c(i) = vsc(i) - op(i)		!Vector from surface point to spacecraft
         s(i) = vsun(i) - op(i)		!Vector from surface point to sun
         n(i) = n(i) - op(i)		!Vector from surface point to north pole
      enddo
C
C We need to project these vectors into the tangent plane.  To do this, we
C transform the vectors to a coordinate system where the z-axis is normal to the
C surface.  The projection into the tangent plane is then simply the x and y
C components of the vector.
C
C If the surface point is the target center, the spacecraft vector projects
C to a single point and the azimuth is undefined.  For this case, the S/C
C vector is projected directly onto the image plane.
C
C The tangent plane is defined by the surface normal vector v:
C
      v(1) = op(1)*(rc/ra)**2
      v(2) = op(2)*(rc/ra)**2
      v(3) = op(3)

C Convert to lat-lon and compute transformation matrix m

      call reclat(v,r,rlon,rlat)
      call eul2m(0.d0,halfpi()-rlat,halfpi()+rlon,3,1,3,m)

C Transform the vectors to "tangent plane coordinate system"

      call mxv(m,n,n)
      if (flg .ne. 1) call mxv(m,c,c)
      call mxv(m,s,s)

C Project vectors into plane

      n(3) = 0.
      if (flg .ne. 1) c(3) = 0.
      s(3) = 0.

C Now project the projected vectors into the image plane by first transforming
C the vectors back into planet coordinates, and then using to OM matrix to
C tranform them into image plane coordinates.

      call mtxv(m,n,n)
      if (flg .ne. 1) call mtxv(m,c,c)
      call mtxv(m,s,s)

      call mxv(om,n,n)
      call mxv(om,c,c)
      call mxv(om,s,s)

      noraz = datan2(n(2),n(1))*dpr()
      if (noraz .lt. 0.) noraz=noraz+360.

      scaz = datan2(c(2),c(1))*dpr()
      if (scaz .lt. 0.) scaz=scaz+360.

      sunaz = datan2(s(2),s(1))*dpr()
      if (sunaz .lt. 0.) sunaz=sunaz+360.
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zellipse_proj.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zellipse_inv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/*  C-Callable Version zellipse_inv                                     */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

int zellipse_inv(om,vsc,fl,oal,oas,scale,ra,rb,rc,rline,rsamp,v)

double om[3][3];	/* Camera to planet transformation matrix */
double vsc[3];  	/* Vector from target center to spacecraft (RS) */
double fl;		/* Camera focal length in mm */
double oal,oas;		/* Optical axis intercept line-sample */
double scale;		/* Picture scale in pixels/mm */
double ra,rb,rc;	/* Target radii */
double rline,rsamp;	/* Input image coordinates */
double v[3];		/* Vector from target center to surface point */
{
int ind;		/* =0 if is not on the target, =1 success */
float flx,oalx,oasx,scalex;

flx = fl;
oalx = oal;
oasx = oas;
scalex = scale;
FTN_NAME2_(ellipse_inv, ELLIPSE_INV) (om,vsc,&flx,&oalx,&oasx,&scalex,
	&ra,&rb,&rc,&rline,&rsamp,v,&ind);
return(ind);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zellipse_radius.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/*  C-Callable Version zellipse_radius                                  */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

void zellipse_radius(lat,lon,ra,rb,rc,lora,gcr)
double lat,lon;		/* coordinates of surface point */
double ra,rb,rc;	/* three radii of target body */
double lora;		/* longitude of semi-major axis */
double *gcr;		/* geocentric radius */
{

FTN_NAME2_(ellipse_radius, ELLIPSE_RADIUS) (&lat,&lon,&ra,&rb,&rc,&lora,gcr);
return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zazimuth.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/*  C-Callable Version zazimuth                                         */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

void  zazimuth(om,op,vsc,vsun,ra,rb,rc,tc_flag,noraz,scaz,sunaz)
double om[3][3];	/* OM matrix */
double op[3];		/* vector from target center to surface point */
double vsc[3];		/* vector from target center to spacecraft */
double vsun[3];		/* vector from target center to sun */
double ra,rb,rc;	/* target vectors */
int    tc_flag;		/* =1 if surface pt is target center, =0 otherwise */
double *noraz;		/* north azimuth */
double *scaz;		/* spacecraft azimuth */
double *sunaz;		/* solar azimuth */
{
FTN_NAME2(azimuth,AZIMUTH)(om,op,vsc,vsun,&ra,&rb,&rc,&tc_flag,
							noraz,scaz,sunaz);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ellipse_proj.imake
/* Imake file for VICAR subroutine ELLIPSE_PROJ */

#define SUBROUTINE ellipse_proj
#define MODULE_LIST ellipse_proj.f ellipse_inv.f azimuth.f ellipse_radius.f \
	zellipse_proj.c zellipse_inv.c zazimuth.c zellipse_radius.c
#define P2_SUBLIB
#define USES_FORTRAN
#define USES_C
#define FTN_STRING
$ Return
$!#############################################################################
$Test_File:
$ create tellipse_proj.c
/* Program TELLIPSE_PROJ.C */

#include <math.h>
#include "vicmain_c"

void main44()
{

  double ra,rb,rc;	/* camera radii */
  double lora;		/* longitude of semi-major axis */
  double fl;		/* Camera focal length in mm */
  double oal,oas;    	/* Optical axis intercept line-sample */
  double scale;          /* Picture scale in pixels/mm */

  double om[3][3]={0.22529468, 0.33034190, 0.91657871,
		  -0.51654703, -0.75716293, 0.39985430,
		   0.82608805, -0.56354106, 0.00005235};

  double vsc[3]={-739030.7,-321741.5,-456.1};
  double vsun[3];	/* Vector from target center to sun */

  double v[3];		/* Vector from targe center to surface point */
  double lat,lon;	/* geocentric latitude, west longitude of surface pt */
  double sunlat,sunlon,sunrange;

  double scline,scsamp,angln;
  double sclat,sclon,scrange;
  double rline,rsamp;	/* line-sample image coordinates */
  double radius;	/* planetocentric radius */
  double noraz;		/* north azimuth */
  double scaz;		/* scaz azimuth */
  double sunaz;		/* solar azimuth */
  double gcr,rad;
  int status,i;
  char msg[80];

  zvmessage(" program TELLIPSE_PROJ", "");

  rad = 180./3.141592653589793;		/* degrees per radian */
  fl = 1500.19;
  scale = 84.8214;
  oal = 500.0;
  oas = 500.0;

  ra = 1829.4;
  rb = 1819.3;
  rc = 1815.7;

  scrange = 806030.;
  sclat = -0.032;
  sclon = 156.474;
  lora = 0.;

  angln = 55.70;
  scline = 541.84;
  scsamp = 607.65;

	/* convert from rline,rsamp to v... */
  status = zellipse_inv(om,vsc,fl,oal,oas,scale,ra,rb,rc,scline,scsamp,v);
	/* convert from v to rline,rsamp */
  status = zellipse_proj(om,vsc,fl,oal,oas,scale,ra,rb,rc,v,&rline,&rsamp);

  sprintf(msg,"Before: (line,samp)=(%12.7f,%12.7f)",scline,scsamp);
  zvmessage(msg,0);
  sprintf(msg,"After: (line,samp)=(%12.7f,%12.7f)",rline,rsamp);
  zvmessage(msg,0);

  for (i=0; i<360; i++) {
    lat = sclat/rad;
    lon = (360.-i)/rad;
    zellipse_radius(sclat,sclon,ra,rb,rc,lora,&radius);
    v[0] = radius*cos(lon)*cos(lat);
    v[1] = radius*sin(lon)*cos(lat);
    v[2] = radius*sin(lat);
    status = zellipse_proj(om,vsc,fl,oal,oas,scale,ra,rb,rc,v,&rline,&rsamp);
    if (status == 1) {
       sprintf(msg,"lon=%d (line,samp)=(%9.4f,%9.4f)",i,rline,rsamp);
       zvmessage(msg,0);
    }
  }

  gcr = sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]);
  zellipse_radius(sclat,sclon,ra,rb,rc,lora,&radius);
  sprintf(msg,"GCR=%7.1f radius=%7.1f",gcr,radius);
  zvmessage(msg,0);

  sunlat = 0.541/rad;
  sunlon = (360.-171.276)/rad;
  sunrange = 10000000.;

  vsun[0] = sunrange*cos(sunlon)*cos(sunlat);
  vsun[1] = sunrange*sin(sunlon)*cos(sunlat);
  vsun[2] = sunrange*sin(sunlat);
  zazimuth(om,v,vsc,vsun,ra,rb,rc,1,&noraz,&scaz,&sunaz);
  sprintf(msg,"noraz=%8.2f, scaz=%8.2f, sunaz=%8.2f",noraz,scaz,sunaz);
  zvmessage( msg,0);
}
$!-----------------------------------------------------------------------------
$ create tellipse_proj.pdf
PROCESS HELP=*
END-PROC

.TITLE
VICAR test program tellipse_proj
.HELP
Test of subroutines ellipse_proj, ellipse_inv, ellipse_radius, and azimuth.
.END
$!-----------------------------------------------------------------------------
$ create tellipse_proj.imake
#define PROGRAM tellipse_proj

#define MODULE_LIST tellipse_proj.c

#define MAIN_LANG_C
#define R2LIB

#define USES_ANSI_C

#define R2LIB
#define LIB_SPICE
#define LIB_MATH77
#define LIB_RTL
#define LIB_P2SUB
#define LIB_TAE

#define USES_FORTRAN
#define LIB_FORTRAN

#define DEBUG		/* for development, remove on delivery */
$!-----------------------------------------------------------------------------
$ create tstellipse_proj.pdf
!Test script for subroutine package ELLIPSE_PROJ
procedure help=*
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tellipse_proj
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create ellipse_proj.hlp
Documentation for subroutine package ELLIPSE_PROJ.COM:

ELLIPSE_PROJ.COM contains a set of subroutines which handle the projection and
lighting geometry for target bodies modeled as ellipsoids:

  ELLIPSE_PROJ   - Given the vector from target center to surface point, compute
                   the (line,sample) position of the surfact point in the image.
  ELLIPSE_INV    - Given the (line,sample) coordinates of a point on an
                   ellipsoid target surface, compute the vector from target
                   center to surface point.
  ELLIPSE_RADIUS - Compute the planetocentric radius for a given lat,lon
  AZIMUTH        - Compute north azimuth, spacecraft azimuth, and solar azimuth
                   angles.

The routines are applicable to images taken by a perspective projection camera
system (i.e. having a shutter and focal plane).  A west longitude,
planetocentric latitude system is used.  West longitudes are measured from the
prime meridian and increase toward the west.  The planetocentric latitude is
the angle between the vector from target center to surface point and the
equatorial plane.   The image line-sample coordinates are assumed to be for a
geometrically corrected image (object space).

FORTRAN calling sequences:

In the following FORTRAN calling sequences, input arguments appear first and
are in UPPER CASE, followed by output arguments in lower case.

The ellipsoid model is defined by the following parameters:

      REAL*8 RA			!Radius of the major equatorial axis (km)
      REAL*8 RB			!Radius of the minor equatorial axis (km)
      REAL*8 RC			!Radius of the polar axis (km)
      REAL*8 LORA		!Longitude of RA

The camera model is defined by the following parameters:

      REAL*4 FL                 !Camera focal length in mm 
      REAL*4 OAL,OAS            !Optical axis intercept line-sample
      REAL*4 SCALE              !Picture scale in pixels/mm

The relative positions of the spacecraft, sun, and surface point, and the
pointing direction of the camera are defined by the following parameters:

      REAL*8 OM(3,3)            !Camera to planet transformation matrix
      REAL*8 VSC(3)             !Vector from target center to spacecraft (RS)
      REAL*8 VSUN(3)		!Vector from target center to sun
      REAL*8 V(3)		!Vector from target center to surface point

The vectors V, VSC and VSUN must be expressed in the coordinate system of the
target body.


The following additional arguments are required by subroutines ELLIPSE_PROJ
and ELLIPSE_INV:

      REAL*8 RLINE,RSAMP        !Image coordinates
      INTEGER IND		!Return status (1=success, 0=failure)

      CALL ELLIPSE_PROJ(OM,VSC,FL,OAL,OAS,SCALE,RA,RB,RC,
     &          V,rline,rsamp,ind)
      CALL ELLIPSE_INV(OM,VSC,FL,OAL,OAS,SCALE,RA,RB,RC,
     &          RLINE,RSAMP,v,ind)

For ELLIPSE_PROJ, IND=0 if the surface point is behind the target (from the
camera's viewpoint).

For ELLIPSE_INV, IND=0 if the point is off the target.


The function ELLIPSE_RADIUS requires the following additional variables defined:

      REAL*8 LAT,LON		!Input lat,lon coordinates
      REAL*8 RADIUS		!Output planetocentric radius (km)

      CALL ELLIPSE_RADIUS(LAT,LON,RA,RB,RC,LORA,radius)

The planetocentric radius is the distance from target center to the given
surface point.


The following additional arguments are required by subroutine AZIMUTH:

      INTEGER*4 FLG	!=1 if surface pt is target center, =0 otherwise
      REAL*8 NORAZ	!Output north azimuth (degrees)
      REAL*8 SCAZ	!Output spacecraft azimuth (degrees)
      REAL*8 SUNAZ	!Output solar azimuth (degrees)

      CALL AZIMUTH(OM,V,VSC,VSUN,RA,RB,RC,FLG,noraz,scaz,sunaz)

The azimuth angles are measured at the surface point defined by the vector V.
To compute the solar azimuth, the solar vector is (1) projected into the
plane tangent to the surface point, and (2) this projected vector is then
projected into the image plane.  The solar azimuth is the angle between a
horizontal line pointing to the right in the image and this projected vector,
measured in a clockwise direction.  The other azimuths are defined similarly.

If the surface point is the target center, the spacecraft vector projects
to a single point and the azimuth is undefined.  For this case, the S/C
vector is projected directly onto the image plane.


C calling sequences:

In the following C calling sequences are input arguments appear first, followed
by the output arguments.

  double ra,rb,rc;	/* camera radii */
  double lora;		/* longitude of semi-major axis */
  double fl;		/* Camera focal length in mm */
  double oal,oas;    	/* Optical axis intercept line-sample */
  double scale;          /* Picture scale in pixels/mm */
  double om[3][3];	/* Planet-to-camera transformation */
  double vsc[3];	/* Vector from target center to spacecraft */
  double vsun[3];	/* Vector from target center to sun */
  double v[3];		/* Vector from targe center to surface point */
  double lat,lon;	/* geocentric latitude, west longitude of surface pt */


For zellipse_proj and zellipse_inv the following output arguments are required:

  double rline,rsamp;	/* line-sample image coordinates */
  int status;		/* Return status, 1=success, 0=failure */

  status = zellipse_proj(om,vsc,fl,oal,oas,scale,ra,rb,rc,v,&rline,&rsamp)
  status = zellipse_inv(om,vsc,fl,oal,oas,scale,ra,rb,rc,rline,rsamp,v)

For zellipse_proj, v is input and rline,rsamp are returned.
For zellipse_inv, rline,rsamp are input and v is returned.


For zellipse_radius, the following output argument is required:

  double radius;	/* planetocentric radius */

  zellipse_radius(lat,lon,ra,rb,rc,lora,&radius)


For zazimuth, the following output arguments are required:

  int flg;		/*=1 if surface pt is target center, =0 otherwise */
  double noraz;		/* north azimuth */
  double scaz;		/* scaz azimuth */
  double sunaz;		/* solar azimuth */
	
  zazimuth(om,v,vsc,vsun,ra,rb,rc,flg,&noraz,&scaz,&sunaz)

See FORTRAN calling sequence (above) for description of arguments.

2 HISTORY
 
Original Programmer: Gary Yagi, Sept 1, 1997
Current Cognizant Programmer: Gary Yagi
Source Language: FORTRAN
Revision history:

09 Oct 02  GMY  Fix back-of-planet test by calculating surface normal
01 Apr 98  GMY  Pass FL, OAL, OAS, SCALE as double in C-bridge
17 OCT 97  GMY  Correct azimuth calculation (FR 90511)
	   Add ELLIPSE_PROJ, ELLIPSE_INV, and all C bridges
$ Return
$!#############################################################################
