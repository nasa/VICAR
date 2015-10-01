$!****************************************************************************
$!
$! Build proc for MIPL module ring_proj
$! VPACK Version 1.9, Friday, June 26, 1998, 11:34:16
$!
$! Execute by entering:		$ @ring_proj
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
$ write sys$output "*** module ring_proj ***"
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
$ write sys$output "Invalid argument given to ring_proj.com file -- ", primary
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
$   if F$SEARCH("ring_proj.imake") .nes. ""
$   then
$      vimake ring_proj
$      purge ring_proj.bld
$   else
$      if F$SEARCH("ring_proj.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ring_proj
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ring_proj.bld "STD"
$   else
$      @ring_proj.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ring_proj.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ring_proj.com -
	-s ring_proj.f ring_inv.f ring_light.f ring_scale.f razimuth.f -
	   zring_proj.c zring_inv.c zring_light.c zring_scale.c zrazimuth.c -
	-i ring_proj.imake -
	-t tstring_proj.pdf tstring_proj_old.pdf -
	-o ring_proj.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ring_proj.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Convert from ring plane coordinates (radius,longitude) to camera coordinates
C (line,sample) in a perspective projection camera system.
C
      SUBROUTINE RING_PROJ(OM,VSC,FL,OAL,OAS,SCALE,RADIUS,RLON,
     &		rline,rsamp,ind)
      IMPLICIT NONE
      REAL*8 OM(3,3)		!Camera to planet transformation matrix
      REAL*8 VSC(3)		!Vector from target center to spacecraft (RS)
      REAL*4 FL			!Camera focal length in mm 
      REAL*4 OAL,OAS		!Optical axis intercept line-sample
      REAL*4 SCALE 		!Picture scale in pixels/mm
      REAL*8 RADIUS,RLON	!Input ring coordinates.
      REAL*8 RLINE,RSAMP 	!Output image coordinates
      INTEGER*4 IND		! =1 if RLINE,RSAMP are valid, =0 otherwise

      REAL*8 x0,y0,z0,xc,yc,zc,S,ELON,PI,RPD

      PI = 3.141592653589793D0
      RPD = PI/180.D0		!radians per degree
      ELON = 360.D0 - RLON	!East longitude

C     ...Compute vector from camera to point on ring
      xc =  RADIUS*DCOS(ELON*RPD) - VSC(1)
      yc =  RADIUS*DSIN(ELON*RPD) - VSC(2)
      zc =                        - VSC(3)
C     ...Rotate vector into camera coordinates
      x0 = OM(1,1)*xc + OM(1,2)*yc + OM(1,3)*zc
      y0 = OM(2,1)*xc + OM(2,2)*yc + OM(2,3)*zc
      z0 = OM(3,1)*xc + OM(3,2)*yc + OM(3,3)*zc
      IF (z0 .LE. 0.) THEN	!Camera is looking away from ring plane
        RLINE = -999.
        RSAMP = -999.
	IND = 0
        RETURN
      ENDIF

C     ...Scale vector into pixels
      S = FL*SCALE/z0			!pixels/km
      RLINE = S*y0 + OAL
      RSAMP = S*x0 + OAS
      IND = 1
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ring_inv.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Convert from camera coordinates (line,sample) to ring plane coordinates
C (radius,longitude) for a perspective projection camera system.  The
C line-sample coordinates are assumed to be in object space.
C
      SUBROUTINE RING_INV(OM,VSC,FL,OAL,OAS,SCALE,RLINE,RSAMP,
     &		radius,rlon,ind)
      IMPLICIT NONE
      REAL*8 OM(3,3)		!Camera to planet transformation matrix
      REAL*8 VSC(3)		!Vector from target center to spacecraft (RS)
      REAL*4 FL			!Camera focal length in mm 
      REAL*4 OAL,OAS		!Optical axis intercept line-sample
      REAL*4 SCALE 		!Picture scale in pixels/mm
      REAL*8 RLINE,RSAMP 	!Input image coordinates
      REAL*8 RADIUS,RLON	!Output ring coordinates.
      INTEGER*4 IND		! =1 If RADIUS,RLON are valid, =0 otherwise

      REAL*8 x0,y0,z0,xc,yc,zc,x,y,S
      REAL*8 PI/3.141592653589793D0/,DPR

      DPR = 180.D0/PI          !degrees per radian

C     ...Vector from spacecraft to point on ring in camera coordinate system
      x0 = RSAMP - OAS
      y0 = RLINE - OAL
      z0 = FL*SCALE
C     ...Convert vector to target-centered coordinates
      xc = OM(1,1)*x0 + OM(2,1)*y0 + OM(3,1)*z0
      yc = OM(1,2)*x0 + OM(2,2)*y0 + OM(3,2)*z0
      zc = OM(1,3)*x0 + OM(2,3)*y0 + OM(3,3)*z0

      S = -VSC(3)/zc		!Scale in km per pixel
      IF (S.LT.0.) THEN		!Camera is looking away from ring plane
	IND = 0
        RETURN
      ENDIF

      X = S*xc + VSC(1)
      Y = S*yc + VSC(2)
      RADIUS = DSQRT(X**2+Y**2)
      RLON = - DMOD(DATAN2(Y,X)*DPR,360.D0)	!West longitude
      IF (RLON .LT. 0.D0) RLON= RLON+360.D0
      IND = 1
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ring_light.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Compute the phase, incidence, and emission angles for a point on the ring.
C
      subroutine ring_light(vsc,vsun,r,lon,i,e,p)
      implicit none
      real*8 vsc(3)             !Vector from target center to S/C (RS vector)
      real*8 vsun(3)            !Vector from target center to sun
      real*8 r,lon		!Radius and longitude of ring point
      real*8 i,e,p		!Incidence, emission, and phase angles (deg)
 
      real*8 v(3),s(3),c(3),PI,rpd,dpr
      real*8 solrange,scrange,elon
      integer j
 
      PI = 3.141592653589793D0
      rpd = PI/180.d0		!Radians per degree
      dpr = 180.d0/PI           !Degrees per radian
      elon = 360.d0 - lon	!East longitude

C     ....Compute vector from planet center to ring point
      v(1) = r*dcos(elon*rpd)
      v(2) = r*dsin(elon*rpd)
      v(3) = 0.

      do j=1,3
         s(j) = vsun(j) - v(j)		!Vector from ring point to sun
         c(j) = vsc(j)  - v(j)		!Vector from ring point to S/C
      enddo

C     ....Convert to unit vectors
      solrange = dsqrt(s(1)**2+s(2)**2+s(3)**2)		!Sun range
      scrange  = dsqrt(c(1)**2+c(2)**2+c(3)**2)		!S/C range
      do j=1,3
         s(j) = s(j)/solrange
         c(j) = c(j)/scrange
      enddo
 
C     ....First assume the sun illuminates the northern half of the ring plane.
C     ....Unit normal to ring plane is n = (0,0,1)
C     ....Cosine of incidence angle is dot product of s and n
      i = dacos(s(3))*dpr
C     ....Cosine of emission angle is dot product of c and n
      e = dacos(c(3))*dpr
C     ....cosine of phase angle is dot product of s and c
      p = dacos(s(1)*c(1)+s(2)*c(2)+s(3)*c(3))*dpr
C     ....If sun illuminates southern half of ring plane, then
C     ....unit normal to ring plane is n = (0,0,-1)
      if (i .gt. 90.) then
         i = 180. - i
         e = 180. - e
      endif
      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ring_scale.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Compute the scale at a given ring radius in km/pixel.  The scale is computed
C at the point on the visible ring closest to the camera.
C
      SUBROUTINE RING_SCALE(OM,VSC,FL,OAL,OAS,SCALE,NL,NS,R,
     &		lon,srange,rline,rsamp,rscl,ascl,ind)
      IMPLICIT NONE
      REAL*8 OM(3,3)            !Camera to planet transformation matrix
      REAL*8 VSC(3)             !Vector from target center to spacecraft
      REAL*4 FL                 !Camera focal length in mm 
      REAL*4 OAL,OAS            !Optical axis intercept line-sample
      REAL*4 SCALE              !Picture scale in pixels/mm
      INTEGER NL,NS		!Number of lines and samples in image
      REAL*8 R			!Input ring radius
      REAL*8 LON		!Output longitude
      REAL*8 SRANGE		!Slant range
      REAL*8 RLINE,RSAMP        !Output image coordinates
      REAL*8 RSCL,ASCL		!Radial and azimuthal ring scale (km/pixel)
      INTEGER IND		! =1 if RSCL and ASCL are valid, =0 otherwise

      REAL*8 L,S,LON0,SR,V(3),PIXELS,KILOMETERS,PI,RPD
      INTEGER I

      PI = 3.141592653589793D0
      RPD = PI/180.D0          !radians per degree

      RSCL = -999.
      ASCL = -999.
      SRANGE = 2.D+9
C     ...Scan at 1 degree increments for the point on the ring closest
C     ...to the camera (minimum slant range).
      DO 20 I=1,360
      LON0 = 1.0*I
      CALL RING_PROJ(OM,VSC,FL,OAL,OAS,SCALE,R,LON0,l,s,ind)
      IF (IND.EQ.0) GOTO 20
      IF (L.LT.1. .OR. L.GT.NL) GOTO 20
      IF (S.LT.1. .OR. S.GT.NS) GOTO 20
      V(1) = R*DCOS(LON0*RPD) - VSC(1)     !Vector from camera to ring pt
      V(2) = -R*DSIN(LON0*RPD) - VSC(2)
      V(3) =                  - VSC(3)
      SR = DSQRT(V(1)**2 + V(2)**2 + V(3)**2)    !Distance to camera
      IF (SR .GE. SRANGE) GOTO 20
      SRANGE = SR
      LON = LON0
      RLINE = L
      RSAMP = S
   20 CONTINUE

      IF (SRANGE .LT. 1.D+9) GOTO 100

C     ...Repeat scanning for the point on the ring closest to the camera,
C     ...but without restricting it to points in the image.
      DO 30 I=1,360
      LON0 = 1.0*I
      CALL RING_PROJ(OM,VSC,FL,OAL,OAS,SCALE,R,LON0,l,s,ind)
      IF (IND.EQ.0) GOTO 30
      V(1) = R*DCOS(LON0*RPD) - VSC(1)     !Vector from camera to ring pt
      V(2) = -R*DSIN(LON0*RPD) - VSC(2)
      V(3) =                  - VSC(3)
      SR = DSQRT(V(1)**2 + V(2)**2 + V(3)**2)    !Distance to camera
      IF (SR .GE. SRANGE) GOTO 30
      SRANGE = SR
      LON = LON0
      RLINE = L
      RSAMP = S
   30 CONTINUE

      IF (SRANGE .GT. 1.D+9) THEN
         SRANGE = -999.
         IND = 0
         RETURN
      ENDIF

C     ...Compute the arc length making up 1 deg of the ring in pixels
  100 CALL RING_PROJ(OM,VSC,FL,OAL,OAS,SCALE,R,LON+1.,l,s,ind)
      PIXELS = DSQRT((RLINE-L)**2 + (RSAMP-S)**2)
C     ...Compute the same arc length in kilometers: S = R*DLON
      KILOMETERS = R*RPD
      ASCL = KILOMETERS/PIXELS		!Azimuthal scale
      
C     ...Compute the pixels representing 1 km radial distance
      CALL RING_PROJ(OM,VSC,FL,OAL,OAS,SCALE,R+1.,LON,l,s,ind)
      PIXELS = DSQRT((RLINE-L)**2 + (RSAMP-S)**2)
      RSCL = 1./PIXELS			!Radial scale
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create razimuth.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Compute north azimuth, spacecraft azimuth, and solar azimuth angles at a
C point on the ring plane.  The ring point is defined by the vector op.
C The solar vector is first projected onto the ring plane.  The projected
C vector is then projected into the image plane.  The solar azimuth is the
C angle between the P5-P6 direction and this projected vector, measured
C clockwise.  The other azimuths are defined similarly.
C
      subroutine razimuth(om,op,vsc,vsun,noraz,scaz,sunaz)
      implicit none
      real*8 om(3,3)		!Target to camera transformation (OM matrix)
      real*8 op(3)		!Vector from target center to ring point
      real*8 vsc(3)		!Vector from target center to S/C (RS vector)
      real*8 vsun(3)		!Vector from target center to sun
      real*8 noraz		!Output north azimuth (degrees)
      real*8 scaz		!Output spacecraft azimuth (degrees)
      real*8 sunaz		!Output solar azimuth (degrees)

      real*8 n(3),s(3),c(3),dpr
      integer i

C Note: For the ring plane, the north vector is replaced by the vector from
C ring point to target center.

C     ...Compute the three vectors of interest
      do i=1,3
         c(i) = vsc(i) - op(i)		!Vector from ring point to spacecraft
         s(i) = vsun(i) - op(i)		!Vector from ring point to sun
         n(i) = - op(i)			!Vector from ring point to center
      enddo

C Project vectors into the ring plane

      n(3) = 0.
      c(3) = 0.
      s(3) = 0.

C Project the vectors into the image plane

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
$ create zring_proj.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zring_inv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zring_light.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/*  C-Callable Version zring_light                                      */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

void  zring_light(vsc,vsun,r,lon,i,e,p)
double *vsc, *vsun;
double r;		/* ring radius */
double lon;		/* ring longitude */
double *i;		/* incidence angle */
double *e;		/* emission angle */ 
double *p;		/* phase angle */
{
FTN_NAME(ring_light) (vsc,vsun,&r,&lon,i,e,p);
}

















$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zring_scale.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zrazimuth.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/*  C-Callable Version zrazimuth                                        */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

void  zrazimuth(om,op,vsc,vsun,noraz,scaz,sunaz)
double om[3][3];	/* OM matrix */
double op[3];		/* vector from target center to surface point */
double vsc[3];		/* vector from target center to spacecraft */
double vsun[3];		/* vector from target center to sun */
double *noraz;		/* north azimuth */
double *scaz;		/* spacecraft azimuth */
double *sunaz;		/* solar azimuth */
{
FTN_NAME(razimuth) (om,op,vsc,vsun,noraz,scaz,sunaz);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ring_proj.imake
/* Imake file for VICAR subroutine RING_PROJ */

#define SUBROUTINE ring_proj
#define MODULE_LIST ring_proj.f ring_inv.f ring_light.f ring_scale.f \
	razimuth.f zring_proj.c zring_inv.c zring_light.c zring_scale.c \
	zrazimuth.c
#define P2_SUBLIB
#define USES_FORTRAN
#define USES_ANSI_C
#define FTN_STRING
$ Return
$!#############################################################################
$Test_File:
$ create tstring_proj.pdf
!Test script for subroutine package RING_PROJ
procedure help=*
refgbl $echo
refgbl $syschar
parm catusr type=string count=1
parm catsrv type=string count=1 default="miplTest"
parm catdb  type=string count=1 default="mipl_t"
body
let _onfail="continue"
let $echo="yes"
local path type=string init="wms_test_work:[testdata.gll]"
if ($syschar(1) = "UNIX")
   let path="/project/test_work/testdata/gll/"
end-if

catlabel inp=&"path"s0368991900.5 out=test.img +
   catusr=&catusr catsrv=&catsrv catdb=&catdb

gspice inp=&"path"s0368991900.5 target=jupiter 'ring

end-proc
.help
TSTRING_PROJ is the unit test for the subroutines in RING_PROJ.COM.
This test requires the presence of MIPS Galileo catalog, and access to the
Password Server.  The test consists of running program CATLABEL and GSPICE.
Both program calls subroutine RPICSCALE which calls RING_SCALE when a ring
image is inputed.
.end

$!-----------------------------------------------------------------------------
$ create tstring_proj_old.pdf
!Test script for subroutine package RING_PROJ
procedure help=*
refgbl $echo
refgbl $syschar
parm user   type=string default=""
parm server type=string default="MIPSDB1"
parm db     type=string default="devCat"
parm pw     type=string default=""
body
let _onfail="continue"
let $echo="yes"
local path type=string init="wms_test_work:[testdata.gll]"
if ($syschar(1) = "UNIX")
   let path="/project/test_work/testdata/gll/"
end-if
ssisnip &"path"ssi_snip.ringtest ekid=s001 +
 catsrv=&server catdb=&db catusr=&user catpw=&pw
end-proc
.help
TSTRING_PROJ is the unit test for the subroutines in RING_PROJ.COM.
This test requires the presence of MIPS Galileo catalog.  The test consists of
running program SSISNIP to load data into the ssioverview tables.  SSISNIP
calls all the subroutines in RING_PROJ.COM to compute the geometry data for
ring plane images.

To see the results from the test, enter dbView and type:

    select * from ssioverview where sclkstrtcnt=368991900

The following values in ssioverview are effected:

        sclkstrtcnt = 368991900
        centerlon = 200.000000
        slantdist = 2191966.500000
        resolution = 29.624897
        incidang = 88.742218
        emissang = 90.444962
        phsang = 178.854340
        sunaz = 175.272690
        noraz = 90.627388
        scaz = 301.257141
        ringradius = 180000.000000
        subscline = 92.767662
        subscsample = -4907.114746
.end
$ Return
$!#############################################################################
$Other_File:
$ create ring_proj.hlp
Documentation for subroutine package RING_PROJ.COM:

RING_PROJ.COM contains a set of subroutines which handle the projection and
lighting geometry for the ring plane:

  RING_PROJ  - Convert from ring plane coordinates (radius,longitude) to
               image coordinates (line,sample).
  RING_INV   - Convert from camera coordinates (line,sample) to ring plane
               coordinates (radius,longitude).
  RING_LIGHT - Compute the phase, incidence, and emission angles for given
               radius-longitude on the ring.
  RING_SCALE - Compute the scale at a given ring radius in km/pixel.
  RAZIMUTH   - Compute spacecraft, sun, and north azimuths on the ring plane

The routines are applicable to images taken by a perspective projection camera
system (i.e. having a shutter and focal plane).  The ring plane is assumed to
coincide with the equatorial plane of the planet.  The longitude system is the
same as that for the planet.  West longitudes are used.  The image line-sample
coordinates are assumed to be for a geometrically corrected image
(object space).

The routines make no check for ring points inside the planet or
behind the planet.  I.e. the planet does not exist in the computations.


FORTRAN calling sequences:

In the following FORTRAN calling sequences, input arguments appear first and
are in UPPER CASE, followed by output arguments in lower case.

Let the following variables be defined:

      REAL*8 OM(3,3)            !Camera to planet transformation matrix
      REAL*8 VSC(3)             !Vector from target center to spacecraft (RS)
      REAL*8 VSUN(3)		!Vector from target center to sun
      REAL*8 V(3)		!Vector from target center to ring point
      REAL*4 FL                 !Camera focal length in mm 
      REAL*4 OAL,OAS            !Optical axis intercept line-sample
      REAL*4 SCALE              !Picture scale in pixels/mm
      REAL*8 R,LON		!Ring radius and longitude coordinates
      REAL*8 LINE,SAMP		!Image line and sample coordinates
      INTEGER*4	IND		!Return status

The vectors V, VSC and VSUN must be expressed in the coordinate system of the
target body.

      CALL RING_PROJ(OM,VSC,FL,OAL,OAS,SCALE,R,LON,line,samp,ind)
      CALL RING_INV(OM,VSC,FL,OAL,OAS,SCALE,LINE,SAMP,r,lon,ind)

For RING_PROJ, (R,LON) are input and (LINE,SAMP) are returned. IND=1 if
the returned line-sample coordinates are valid.  IND=0 otherwise (i.e. the
camera is pointing away from the ring plane).

For RING_INV, (LINE,SAMP) are input and (R,LON) are returned. IND=1 if the
returned radius-longitude coordinates are valid.  IND=0 otherwise (i.e. the
camera is pointing away from the ring plane).

The subroutine RING_LIGHT requires the following additional variables defined:

      REAL*8 I,E,P              !Incidence, emission, and phase angles (deg)

      CALL RING_LIGHT(VSC,VSUN,R,LON,i,e,p)

For the input ring point (R,LON), RING_LIGHT will output the solar incidence,
emission, and phase angles.  Let the surface normal vector, the solar vector,
and the spacecraft vector be computed at the surface point (R,LON).  The solar
incidence angle is the angle between the solar vector and the surface normal.
The emission angle is the angle between the spacecraft vector and the surface
normal.  The phase angle is the angle between the solar vector and the
spacecraft vector.

RING_LIGHT will compute the solar incidence, emission, and phase angles at the
ring point given by coordinates (R,LON).  Let Vn be a vector normal to the
ring plane.  Let Vs and Vc be the vectors from the ring point to the Sun and
spacecraft, respectively.  The solar incidence angle is the angle between Vn
and Vs.  The emission angle is the angle between Vn and Vc.  The phase angle
is the angle between Vs and Vc.

Note:  The vector normal to the ring plane is parallel to the planet's spin
axis.  It points to the north if the Sun illuminates the northern side of the
ring plane.  Otherwise it points to the south.

The subroutine RING_SCALE requires the following additional variables defined:

      INTEGER NL,NS		!Number of lines and samples in image
      REAL*8 SRANGE		!Distance from ring pt to camera (Slant range)
      REAL*8 RSCL,LSCL		!Radial and longitudinal ring scale (km/pixel)

      CALL RING_SCALE(OM,VSC,FL,OAL,OAS,SCALE,NL,NS,R,
		lon,srange,line,samp,rscl,lscl,ind)

The ring radius R is input and the radial and longitudinal ring scales are
output.  The scale is computed at the point on the visible ring at the given
radius closest to the camera.  Also output are the ring longitude and image
line-sample coordinates for the ring point. IND=1 if RSCL and LSCL are valid.
IND=0 otherwise (the given ring radius is not visible within the image).

If the ring is not visible within the image, a second attempt is made, searching
for the point at the given ring radius that has the highest resolution, this
time without the condition that the point be visible in the image.

IND=1 if the returned values are valid.  IND=0 otherwise (i.e. the camera is
pointing away from the ring plane).

For the routine RAZIMUTH, the following output variables must be defined:

      REAL*8 NORAZ              !Output north azimuth (degrees)
      REAL*8 SCAZ               !Output spacecraft azimuth (degrees)
      REAL*8 SUNAZ              !Output solar azimuth (degrees)
 
      CALL RAZIMUTH(OM,V,VSC,VSUN,noraz,scaz,sunaz)

The solar azimuth is computed by (1) projecting the solar vector onto the ring
plane and then (2) projecting the projected vector onto the image plane.  The
solar azimuth is the angle between a horizontal line pointing to the right and
this projected vector, measured clockwise.  The other azimuths are defined
similarly.


C Calling Sequences:

In the following C calling sequences are input arguments appear first, followed
by the output arguments.

  double om[3][3];        /* Camera to planet transformation matrix */
  double vsc[3];          /* Vector from target center to spacecraft (RS) */
  float fl;               /* Camera focal length in mm */
  float oal,oas;          /* Optical axis intercept line-sample */
  float scale;            /* Picture scale in pixels/mm */
  double radius,rlon;     /* Radius and longitude of ring point */
  double rline,rsamp;     /* Image line-sample coordinates */
  int status;             /* Return status, 1=success, 0=failure */

  status = zring_proj(om,vsc,&fl,&oal,&oas,&scale,radius,rlon,&rline,&rsamp);
  status = zring_inv(om,vsc,&fl,&oal,&oas,&scale,rline,rsamp,&radius,&rlon);

For zring_proj, (radius,rlon) is input and (rline,rsamp) is returned.
For zring_inv, (rline,rsamp) is input  and (radius,rlon) is returned.


The routine zring_light requires the following output arguments defined:

  double i;              /* solar incidence angle */
  double e;              /* emission angle */ 
  double p;              /* phase angle */

  zring_light(vsc,vsun,r,lon,&i,&e,&p);

The routine zring_scale requires the following output arguments defined:

  double slant_range;    /* Distance from ring point to spacecraft */
  double rscl,ascl;      /* Radial and azimuthal picture scale in km/pixel */

  status = zring_scale(om,vsc,&fl,&oal,&oas,&scale,nl,ns,r,rlon,slant_range,
               rline,rsamp,rscl,ascl);


The routine zrazimuth requires the following output arguments defined:

  double noraz;          /* north azimuth */
  double scaz;           /* spacecraft azimuth */
  double sunaz;          /* solar azimuth */

  zrazimuth(om,op,vsc,vsun,noraz,scaz,sunaz);

See FORTRAN calling sequence (above) for description of arguments.
 
2 HISTORY
 
Original Programmer: Gary Yagi, Sept 1, 1997
Current Cognizant Programmer: Gary Yagi
Source Language: FORTRAN
Revision history:
 
03 Nov 97  GMY  Correct azimuth calculation (FR 90511)
                Add all C-bridges
14 Feb 98  GMY  If ring is not in image, return highest resolution scale,
                even if off the image.  Flag all values as invalid (-999.) if
                error in computation.
25 Jun 1998 TXH Modified zring_scale, zring_inv, and zring_proj, the C-bridge 
                to subroutines ring_scale, ring_inv, and ring_proj to have 
                their float parameters to be pointers to floats.  The problem 
                was found under SGI, where its float values are treated as 
                doubles.
                Modified IMAKE file to use ANSI C.

$ Return
$!#############################################################################
