$!****************************************************************************
$!
$! Build proc for MIPL module pproj
$! VPACK Version 1.9, Monday, December 07, 2009, 16:32:19
$!
$! Execute by entering:		$ @pproj
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
$ write sys$output "*** module pproj ***"
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
$ write sys$output "Invalid argument given to pproj.com file -- ", primary
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
$   if F$SEARCH("pproj.imake") .nes. ""
$   then
$      vimake pproj
$      purge pproj.bld
$   else
$      if F$SEARCH("pproj.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pproj
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pproj.bld "STD"
$   else
$      @pproj.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pproj.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pproj.com -mixed -
	-s pproj.f zpproj.c -
	-i pproj.imake -
	-t tpproj.f tzpproj.c tpproj.imake tpproj.pdf tstpproj.pdf -
	-o pproj.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pproj.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE PPROJ(DATA,LINE,SAMP,LAT,LON,IMODE,ILAT,RADIUS,SRANGE,
     & IND)
c   2Aug90  lwk  added ILAT parameter
c  10Sep90  lwk  added RTANG parameter
c  27Jun91  lwk  renamed RTANG to RADIUS (& fixed it for ellipsoidal case),
c                  added SRANGE parameter
c  10dec92  lwk  used RADIUS in mode=2 to allow disabling of BOP test
c   4nov94  sxp  Made portable for UNIX: changed REAL*8 to DOUBLE PRECISION,
c                REAL*4 to REAL, and calls to MVL to calls to MVE.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION CP(3),OP(3),CPP(3),OM(3,3),RS(3)
      REAL EPS/1.E-6/
      REAL LAT,LON,LINE,SAMP,RADIUS,SRANGE,DATA(*)
      DATA RADDEG, DEGRAD / 5.72957795130823D1,
     & 1.74532925199433D-2/
C
      IND = 0
      FL = DATA(27)
      OAL = DATA(28)
      OAS = DATA(29)
      SCALE = DATA(30)
      RP = DATA(25)
      RE = DATA(26)
      E = RE/RP
      E2 = E*E
      CALL MVE(8,9,DATA,OM,1,1)
      CALL MVE(8,3,DATA(19),RS,1,1)
C
      IF(IMODE.EQ.2) GOTO 30
C     ....Here to convert (lat,lon) to (line,samp)
      RLAT = LAT*DEGRAD
      RLON = LON*DEGRAD
C          CONVERT FROM GEODETIC TO GEOCENTRIC LATITUDE
      IF (ILAT.EQ.1 .AND. ABS(LAT).NE.90.D0) RLAT=DATAN(DTAN(RLAT)/E2)
      CLAT = DCOS(RLAT)
      SLAT = DSIN(RLAT)
C          COMPUTE GEOCENTRIC RADIUS
      D1 = RP*CLAT
      D2 = RE*SLAT
      R = (RE*RP)/DSQRT(D1*D1+D2*D2)
      CP(1) = R*CLAT*DCOS(RLON) - RS(1)
      CP(2) = -R*CLAT*DSIN(RLON) - RS(2)
      CP(3) = R*SLAT - RS(3)
C
C     ....Back of planet test
      D1 = 0.
      D2 = 0.
      DO I=1,3
         D1 = D1 + CP(I)**2
         D2 = D2 + RS(I)**2
      ENDDO
      IBOP = 0
      IF (D1+R**2.GT.D2) THEN		!Point behind planet
	IF (RADIUS.GE.0.) RETURN
	IBOP = 1
      ENDIF
C
      DO 20 I=1,3
      D1 = 0.D0
      DO 10 J=1,3
   10 D1 = D1 + OM(I,J)*CP(J)
   20 CPP(I) = D1
C
      S = FL*SCALE/CPP(3)
      LINE = OAL + S*CPP(2)
      SAMP = OAS + S*CPP(1)
      IND = 1-IBOP
      RETURN
C
C     ....Here to convert (line,samp) to lat,lon)
   30 RADIUS = 0.0
      SRANGE = 0.0
      X = SAMP - OAS
      Y = LINE - OAL
      Z = FL*SCALE
C
      DO 40 I=1,3
   40 CP(I) = OM(1,I)*X + OM(2,I)*Y + OM(3,I)*Z
C
      A = CP(1)*CP(1) + CP(2)*CP(2) + E2*CP(3)*CP(3)
      B = CP(1)*RS(1) + CP(2)*RS(2) + E2*CP(3)*RS(3)
      C = RS(1)*RS(1) + RS(2)*RS(2) + E2*RS(3)*RS(3) - RE*RE
      D = B*B - A*C
      IF (D.GE.0.) GO TO 50
C
C     ... point is off planet.  Just to be nice, we will proceed assuming
C	  that the tangent radius along this line of sight is the correct 
C	  radius.  (This allows off-limb points to be plotted, for 
C	  Orthographic & Perspective projections.)
      D = 0
      GO TO 60
C
   50 IND = 1
   60 S = (-B-DSQRT(D))/A
C
C	OP is the vector from planet center to surface intercept
C	S*CP is the vector from s/c to surface intercept
      RADIUS = 0.
      SRANGE = 0.
      DO I=1,3
	OP(I) = S*CP(I) + RS(I)
	RADIUS = RADIUS + OP(I)*OP(I)
	SRANGE = SRANGE + CP(I)*CP(I)
      ENDDO
      RADIUS = SQRT(RADIUS)
      SRANGE = S*SQRT(SRANGE)
C
      X = OP(1)
      Y = OP(2)
      Z = OP(3)
      X1 = DABS(X)
      Y1 = DABS(Y)
      Z1 = DABS(Z)
      D = DSQRT(X*X+Y*Y)
      IF(D.LT.Z1*EPS) GOTO 98
      IF (ILAT.EQ.0) THEN
	LAT = DATAN(Z/D)*RADDEG		! GEOCENTRIC LAT.
      ELSE
	LAT = DATAN(E2*Z/D)*RADDEG	! GEODETIC LAT.
      ENDIF
      IF(Y1.LT.X1*EPS) GOTO 96
      IF(X1.LT.Y1*EPS) GOTO 94
      LON = 360. - DATAN2(Y,X)*RADDEG
      IF(LON.GT.360.) LON=LON-360.
      RETURN
   94 LON = 270.
      IF(Y.LT.0.) LON=90.
      RETURN
   96 LON = 0.
      IF(X.LT.0.) LON=180.
      RETURN
C
   98 LAT = 90.
      IF(Z.LT.0.) LAT=-LAT
      LON = 0.
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zpproj.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zpproj - CONVERT LAT,LON TO LINE, SAMP FOR PERSPECTIVE*/
/************************************************************************/

void zpproj(data,line,samp,lat,lon,imode,ilat,radius,srange,ind)
int imode, ilat;
int *ind;
float *lat, *lon, *line, *samp, *radius, *srange;
float data[];
{
FTN_NAME(pproj)(data,line,samp,lat,lon,&imode,&ilat,radius,srange,ind);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pproj.imake
/* Imake file for VICAR subroutine PPROJ */

#define SUBROUTINE pproj

#define MODULE_LIST pproj.f zpproj.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tpproj.f
C-----THIS IS A TEST OF MODULE PPROJ
      INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C-----THIS ROUTINE WILL SET UP A DATA BUFFER FOR INPUT TO PPROJ.
C-----PPROJ WILL THEN CONVERT (L,S) TO (LAT,LON) OR INVERSE.
	IMPLICIT REAL (A-Z)
        CHARACTER*132 PBUF
	INTEGER ILAT,IND
	REAL MAP(40)
CCCC      REAL*8 OM(3,3),RS(3)                    FYI, BUT NOT REFERENCED.
CCCC      EQUIVALENCE (MAP,OM),(MAP(19),RS)
C
	CALL ZIA(MAP,40)
	MAP(25) = 1000.             ! POLAR RADIUS       KM
	MAP(26) = 1500.             ! EQUATORIAL RADIUS  KM
	MAP(27) = 1500.             ! FOCAL LENGTH IN MM
	MAP(28) = 500.              ! OPTICAL AXIS LINE NUMBER
	MAP(29) = 500.              ! OPTICAL AXIS SAMP NUMBER
	MAP(30) = 100.              ! SCALE   PX/MM
	MAP(31) = 10.               ! SUB SPCRFT POINT  LAT
	MAP(32) = 10.               ! SUB SPCRFT POINT  LON 
	MAP(33) = 300.              ! SUB SPCRFT POINT  LINE
	MAP(34) = 400.              ! SUB SPCRFT POINT  SAMP
	MAP(35) = 20.               ! NORTH ANGLE
	MAP(38) = 100000.           ! RANGE TO CENTER OF PLANET  KM
	CALL FORMOM(MAP)            ! CREATE OM AND RS 
C
C-----ALL SET TO TRY PPROJ
	LINE = 400.
	SAMP = 500.
10	ILAT = 1	! GEODETIC LAT
	CALL XVMESSAGE('DERIVE (LT,LN) FROM (L,S)',' ')
	CALL PPROJ(MAP,LINE,SAMP,LAT,LON,2,ILAT,RTANG,SLANT,IND)
	IF (IND.EQ.0) THEN
	  CALL XVMESSAGE('POINT IS OFF PLANET',' ')
	  CALL PRNT(7,1,RTANG,'TANGENT RADIUS =.')
	ELSE
          WRITE (PBUF,9000) LINE,SAMP,LAT,LON
9000      FORMAT ('L,S,LT,LN', 1PE10.3,1PE10.3,1PE10.3,1PE10.3)
          CALL XVMESSAGE(PBUF, ' ')
	  CALL PRNT(7,1,RTANG,'RADIUS =.')
	  CALL PRNT(7,1,SLANT,'SLANT DISTANCE =.')
	ENDIF
C  PUT IN A TEST FOR RTANG OPTION:
        IF (LINE.GT.400.) GO TO 15
	LINE = 1500.
	SAMP = 5000.
	GO TO 10
15	LAT = 30.
	LON = 5.
	CALL XVMESSAGE('DERIVE (L,S) FROM (LT,LN)',' ')
	CALL PPROJ(MAP,LINE,SAMP,LAT,LON,1,ILAT,RTANG,SLANT,IND)
	IF (IND.ne.0) then
          WRITE (PBUF,9000) LINE,SAMP,LAT,LON
          CALL XVMESSAGE(PBUF, ' ')
	else
20	CALL XVMESSAGE('POINT IS ON BACKSIDE OF PLANET',' ')
        end if
        CALL XVMESSAGE(
     . 'Repeat test case in C to test C interface: zpproj', ' ')

        call tzpproj(MAP)

	RETURN
	END

C     THIS IS IBM SUBROUTINE FARENC   -----NAME CHANGE-----
C/*   2 FEB 83   ...CCA...     INITIAL RELEASE
      SUBROUTINE FORMOM(DATA)
C ROUTINE TO SET UP CAMERA POINTING GEOMETRY INFO FOR CALCULATION OF
C THE PLANET-TO-CAMERA ROTATION MATRIX (OM).
      IMPLICIT DOUBLE PRECISION (A-Z)
      REAL DATA(*)
C
      PI = 3.141592653589793D0
      RADDEG = 180.D0 / PI
      DEGRAD = PI / 180.D0
      FL = DATA(27)
      OAL = DATA(28)
      OAS = DATA(29)
      SCALE = DATA(30)
      LAT = DATA(31)
      LON = DATA(32)
      LSS = DATA(33)
      SSS = DATA(34)
      NA = DATA(35)
      D = DATA(38)
C          CONVERT FROM GEODETIC TO GEOCENTRIC LATITUDE
      IF(DABS(LAT).EQ.90.D0) GOTO 10
      RP = DATA(25)
      RE = DATA(26)
      E = RE/RP
      LAT = DATAN(DTAN(LAT*DEGRAD)/E**2)*RADDEG
   10 CALL MOMATI(OAL,OAS,LSS,SSS,SCALE,FL,LON,LAT,NA,D,DATA,DATA(19))
      RETURN
C
      END
$!-----------------------------------------------------------------------------
$ create tzpproj.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzpproj)(map)
float map[];
{
float lat, lon, line, samp, rtang, slant;
int ind, ilat;
char pbuf[133];
/*  ==================================================================  */
	line = 400.;
	samp = 500.;
start:	ilat = 1;	/* geodetic lat */
	zvmessage("DERIVE (LT,LN) FROM (L,S)","");
        zpproj(map,&line,&samp,&lat,&lon,2,ilat,&rtang,&slant,&ind);
	if (ind == 0) {
	  zvmessage("POINT IS OFF PLANET","");
	  zprnt(7,1,&rtang,"TANGENT RADIUS =.");
        }
	else {
          sprintf( pbuf, "L,S,LT,LN %10.3E %10.3E %10.3E %10.3E", 
                          line,samp,lat,lon); 
          zvmessage(pbuf, "");
	  zprnt(7,1,&rtang,"RADIUS =.");
	  zprnt(7,1,&slant,"SLANT DISTANCE =.");
	}
/*  put in a test for rtang option:  */
        if (line <= 400.) {
	  line = 1500.;
	  samp = 5000.;
 	  goto start;
        }
	lat = 30.;
	lon = 5.;
	zvmessage("DERIVE (L,S) FROM (LT,LN)","");
	zpproj(map,&line,&samp,&lat,&lon,1,ilat,&rtang,&slant,&ind);
	if (ind != 0) {
          sprintf( pbuf, "L,S,LT,LN %10.3E %10.3E %10.3E %10.3E", 
                          line,samp,lat,lon); 
          zvmessage(pbuf, "");
 	  return;
        }
        else 
 	  zvmessage("POINT IS ON BACKSIDE OF PLANET",""); 
}
$!-----------------------------------------------------------------------------
$ create tpproj.imake
/* Imake file for Test of VICAR subroutine PPROJ */

#define PROGRAM tpproj

#define MODULE_LIST tpproj.f tzpproj.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create tpproj.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstpproj.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tpproj
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create pproj.hlp
1  PPROJ

A FORTRAN and C callable routine which transforms coordinates from 
object space (line,sample) to (latitude,longitude) and vice-versa.

  Fortran Calling Sequence:
      CALL PPROJ(MAP,LINE,SAMP,LAT,LON,MODE,ILAT,RADIUS,SRANGE,IND)

  C Calling Sequence: 
      zpproj(map,&line,&samp,&lat,&lon,mode,ilat,&radius,&srange,&ind);
    Note the & for all scalar arguments except mode and ilat, denoting the 
    passing by address.

2  ARGUMENTS

      MAP        is the standard 40-word geometry buffer (see subroutine
                 CONVEV), R*4.

      LINE,SAMP  is the object space location of a point, R*4.

      LAT,LON    are the planetary coordinates of the point, R*4.
                 Longitude is degrees West, Latitude is in degrees,
                 either planetodetic or planetocentric, depending on
                 ILAT.

      MODE       1 = (LAT,LON) to (LINE,SAMP) I*4
                 2 = (LINE,SAMP) to (LAT,LON) 

      ILAT       0 = planetocentric latitudes
	         1 = planetodetic latitudes

      RADIUS     is the distance from planet center to the intercept point
                 of the line of sight with the planet surface.  If the point
                 falls off the planet then it is the tangent radius to the
                 line of sight from planet center.   Note that in this case,
                 LAT,LON are computed assuming this radius instead of the 
                 one in MAP(26).
                 This parameter is ignored when MODE=1.

      SRANGE     is the distance from the spacecraft to the intercept point
                 of the line of sight with the planet surface.  If the point
                 falls off the planet then it is the distance to the tangent 
                 to the tangent point defined under RADIUS for this case.
                 This parameter is ignored when MODE=1.

      IND        Return indicator.  1=success, 0=failure.

If MODE=1, IND=0 if the input (lat,lon) coordinates are on the back side of
the planet.

If MODE=2, IND=0 if the input (line,samp) coordinates are off the planet.

2  HISTORY

Original Programmer: Gary Yagi, 20 September 1983
Current Cognizant Programmer: Gary Yagi
Source Language: Fortran
Revision:
  20 SEP 83  cca  convert to vax
  15 May 90  GMY  Replace alternate return with return indicator
   2 Aug 90  lwk  added ILAT parameter
  10 Sep 90  lwk  added RTANG parameter
  27 Jun 91  lwk  renamed RTANG to RADIUS (& fixed it for ellipsoidal case),
                  added SRANGE parameter
   5 Nov 94  SXP  Made portable.  Added zpproj for calls from C.

2  OPERATION

PPROJ uses a perspective projection model of the camera system to transform
coordinates from object space (line,sample) to (latitude,longitude) and
vice-versa. The transformation uses the OM-matrix stored in the 40-word
geometry buffer and assumes an oblate spheroid model of the planet.  PPROJ
performs the same function as IPPCOV and CORCAV.  However, it also returns
the distances from the point viewed on the planet to the spacecraft and
planet center (SRANGE and RADIUS parameters).

When computing latitude/longitude (MODE=2), an additional feature is provided
when the point falls off the planet:  in this case the routine computes the
value that the radius would need to have in order to make the line of sight 
just tangent to the surface and returns this to the caller.  The routine also 
returns the lat/long of this tangent point.  However, note that IND=0 for this 
case, so by default these returned values should be ignored;  they are provided 
in order to enable the user to project off-limb data for certain projections.

Subroutines called:  MVE
$ Return
$!#############################################################################
