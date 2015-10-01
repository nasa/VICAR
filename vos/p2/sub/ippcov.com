$!****************************************************************************
$!
$! Build proc for MIPL module ippcov
$! VPACK Version 1.9, Monday, December 07, 2009, 16:24:28
$!
$! Execute by entering:		$ @ippcov
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
$ write sys$output "*** module ippcov ***"
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
$ write sys$output "Invalid argument given to ippcov.com file -- ", primary
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
$   if F$SEARCH("ippcov.imake") .nes. ""
$   then
$      vimake ippcov
$      purge ippcov.bld
$   else
$      if F$SEARCH("ippcov.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ippcov
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ippcov.bld "STD"
$   else
$      @ippcov.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ippcov.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ippcov.com -mixed -
	-s ippcov.f zippcov.c -
	-i ippcov.imake -
	-t tippcov.f tzippcov.c tippcov.imake tippcov.pdf tstippcov.pdf -
	-o ippcov.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ippcov.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE IPPCOV(RLAT,RLON,Z,X,A,RS,OM,E,CL,CS,FLAG)
C
C
C        FUNCTION -
C
C          CALCULATES LATITUDE AND LONGITUDE FOR SPECIFIED DATA
C          POINT ON AN INVERSE PERSPECTIVE PROJECTION
C
C        PARAMETERS -
C
C          RLAT = CALCULATED LATITUDE(RADIANS)
C          RLON = CALCULATED LONGITUDE (E. RADIANS)
C          Z = LINE VALUE OF INPUT POINT
C          X = SAMPLE VALUE OF INPUT POINT
C          A = DESCRIPTIVE ARRAY
C              A(1) = EQUATORIAL RADIUS
C              A(2) = CAMERA FOCAL LENGTH
C              A(3) = DISTANCE PLANET TO SPACECRAFT
C
C        OTHER VARIABLES -
C
C          OM = ROTATION MATRIX PLANET TO CAMERA - om matrix
C          E = ECCENTRICITY MATRIX (1.,0.,0.,0.,1.,0.,0.,0.,re/rp)
C          RS = POSITION OF SPACECRAFT IN PLANET COORDINATES-rs vector
C
C
      REAL A(3)
      DOUBLE PRECISION RS(3), OM(3,3),E(3,3),T(3),U(3),V(3),AA,BB,CC,
     * DENOM,D,XX,ZZ
      DATA TWOPI/6.283185308/
C
C        INITIALIZATION
C
      AA = 0.
      BB = 0.
      CC = 0.
      DO 20 I = 1,3
         T(I) = 0.
         U(I) = 0.
   20 CONTINUE
      XX=X-CS
      ZZ=Z-CL
C
C        CALCULATE QUADRATIC COEFFICIENTS
C          AA = (NORM(U))**2
C          BB = 2 * DOT(U,V)
C          CC = (NORM(V))**2 - R**2
C              WHERE
C          T = R * (XX,ZZ,A(2))
C          U = E * T
C          V = E * RS
C
C        NOTE -- R IS ROTATION FROM PLANET TO CAMERA COORDINATES,
C          SO MULTIPLY IN TRANSPOSE SENSE
C
      DO 40 I = 1,3
   40 T(I) = OM(1,I)*XX + OM(2,I)*ZZ + OM(3,I)*A(2)
      DO 50 I = 1,3
      U(I) = E(I,1)*T(1) + E(I,2)*T(2) + E(I,3)*T(3)
   50 V(I) = E(I,1)*RS(1) + E(I,2)*RS(2) + E(I,3)*RS(3)
C
      AA = U(1)*U(1) + U(2)*U(2) + U(3)*U(3)
      BB = U(1)*V(1) + U(2)*V(2) + U(3)*V(3)
      CC = V(1)*V(1) + V(2)*V(2) + V(3)*V(3)
      BB = 2. * BB
      CC = CC - A(1)*A(1)
C
C        CALCULATE DEPTH FACTOR 'D'.  NEGATIVE DISCRIMINANT MEANS
C        POINT OFF PLANET.  USE -SQRT TO INSURE VISIBLE SIDE OF
C        PLANET.
C
      D = BB*BB - 4.*AA*CC
      IF (D .GE. 0.) GO TO 70
      RLAT=FLAG
      RLON=FLAG
      RETURN
   70 D = (-BB - DSQRT(D))/(2.*AA)
C
C        FIND RLAT AND RLON
C
      DO 80 I = 1,3
   80 T(I) = D * T(I) + RS(I)
      DENOM = DSQRT(T(1)*T(1) + T(2)*T(2))
      RLAT = DATAN(T(3)/DENOM)
      RLON = DATAN2(T(2),T(1)) + TWOPI
      RLON = AMOD(RLON,TWOPI)
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zippcov.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zippcov - CONVERT LINE, SAMP to LAT,LON FOR PERSPECTIVE*/
/************************************************************************/

void zippcov(rlat,rlon,z,x,a,rs,om,e,cl,cs,flag)
float *rlat, *rlon, *z, *x, *cl, *cs, *flag;
double *om, *rs, *e;
float *a;
{
FTN_NAME2(ippcov, IPPCOV) (rlat,rlon,z,x,a,rs,om,e,cl,cs,flag);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ippcov.imake
/* Imake file for VICAR subroutine IPPCOV */

#define SUBROUTINE ippcov

#define MODULE_LIST ippcov.f zippcov.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tippcov.f
C         TEST PROGRAM FOR IPPCOV SUBPROGRAM
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      DOUBLE PRECISION OM(3,3),RS(3),EMA(9)
      REAL PTS(3),LINE
      DATA EMA/1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0/

      CHARACTER*560 BUFFER

      RLAT =0.
      RLON =0.

C  DEFAULT VARIABLES FOR IO

      RS(1) = -730950.0
      RS(2) = -339690.0
      RS(3) = -1209.3

      PTS(1) = 1815.0
      PTS(2) = 1500.19 * 84.821
      PTS(3) = SQRT(RS(1)**2+RS(2)**2+RS(3)**2)

      OM(1,1) = 0.407057464D0
      OM(2,1) = 0.112318814D0
      OM(3,1) = 0.906470418D0
      OM(1,2) = -0.874813914D0
      OM(2,2) = -0.237469494D0
      OM(3,2) = 0.422266245D0
      OM(1,3) = 0.262687564D0
      OM(2,3) = -0.964879572D0
      OM(3,3) = 0.0015943704D0
      LINE = 614.10
      SAMP = 614.38
      OAL = 500.0
      OAS = 500.0
      FLAG = 99.99
 
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('BEFORE CONVERSION FROM LINE/SAMPLE TO LAT/LONG:',
     .               ' ')
      WRITE(BUFFER,100)RLAT,RLON,LINE,SAMP,FLAG,RS,OM,PTS,OAL,OAS
      CALL XVMESSAGE(BUFFER(  2: 80),' ')
      CALL XVMESSAGE(BUFFER( 82:160),' ')
      CALL XVMESSAGE(BUFFER(162:240),' ')
      CALL XVMESSAGE(BUFFER(242:320),' ')
      CALL XVMESSAGE(BUFFER(322:400),' ')
      CALL XVMESSAGE(BUFFER(402:480),' ')
      CALL XVMESSAGE(BUFFER(482:560),' ')
      CALL XVMESSAGE(' ',' ')

      CALL IPPCOV(RLAT,RLON,LINE,SAMP,PTS,RS,OM,EMA,OAL,OAS,FLAG)

      CALL XVMESSAGE('AFTER CONVERSION TO LAT/LONG:',' ')
      WRITE(BUFFER,100)RLAT*57.0,RLON*57.0,LINE,SAMP,FLAG,RS,OM,PTS,
     1      OAL,OAS
      CALL XVMESSAGE(BUFFER(  2: 80),' ')
      CALL XVMESSAGE(BUFFER( 82:160),' ')
      CALL XVMESSAGE(BUFFER(162:240),' ')
      CALL XVMESSAGE(BUFFER(242:320),' ')
      CALL XVMESSAGE(BUFFER(322:400),' ')
      CALL XVMESSAGE(BUFFER(402:480),' ')
      CALL XVMESSAGE(BUFFER(482:560),' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(
     . 'Repeat test case in C to test C interface: zippcov', ' ')

      call tzippcov( OM, RS, EMA, PTS)

      RETURN
  100 FORMAT(' LAT: ',F8.2,'  LONG: ',F8.2,4X,'LINE: ',F8.2,'  SAMP: ',
     1       F8.2,' FLAG: ',F5.1,4X,' RS: ',3(E11.5,4X),30X,' OM:',2(
     3       3(E11.5,4X),35X),3(E11.5,4X),31X,' PTS: ',3(F10.2,2X),
     4       'OAL: ',F8.2,2X,'OAS: ',F8.2,30X)
      END
$!-----------------------------------------------------------------------------
$ create tzippcov.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzippcov)(om, rs, ema, pts)
double *om, *rs, *ema;
float *pts;
{
float rlat, rlon, line, samp, cl, cs, flag;
/*  ==================================================================  */
      rlat =0.;
      rlon =0.;
      line = 614.10;
      samp = 614.38;
      cl = 500.0;
      cs = 500.0;
      flag = 99.99;
  zippcov(&rlat,&rlon,&line, &samp,pts, rs, om,ema, &cl,&cs,&flag);
      rlat = 57.0*rlat;
      rlon = 57.0*rlon;  /*  convert to degrees  */
      zprnt(7,1,&rlat," LAT =.");
      zprnt(7,1,&rlon," LONG =.");

}
$!-----------------------------------------------------------------------------
$ create tippcov.imake
/* Imake file for Test of VICAR subroutine IPPCOV */

#define PROGRAM tippcov

#define MODULE_LIST tippcov.f tzippcov.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create tippcov.pdf
 PROCESS
 END-PROC
$!-----------------------------------------------------------------------------
$ create tstippcov.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tippcov
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create ippcov.hlp
1  IPPCOV
 To convert from line, sample to latitude, longitude in a
 geometrically corrected, perspective projection.
  
 FORTRAN Calling Sequence:  
       CALL IPPCOV(RLAT,RLON,Z,X,A,RS,OM,E,CL,CS,FLAG)

 C Calling Sequence:  
  zippcov(&rlat,&rlon,&z,&x,a,rs,om,e,&cl,&cs,&flag);
      Note the & for all scalar arguments denoting the passing by address.

2  ARGUMENTS
 RLAT and RLON are the output parameters (RADIANS).  All others are inputs.
 RS, OM, and E are DOUBLE PRECISION. All others are single precision (REAL).

       RLAT        CALCULATED LATITUDE(RADIANS)

       RLON        CALCULATED LONGITUDE (E. RADIANS)

       Z           LINE VALUE OF INPUT POINT

       X           SAMPLE VALUE OF INPUT POINT

       A(3)        DESCRIPTIVE ARRAY
                   A(1) = EQUATORIAL RADIUS  , KM
                   A(2) = CAMERA FOCAL LENGTH, OBJECT SPACEPIXELS
                   A(3) = DISTANCE PLANET TO SPACECRAFT, KM

       RS(3)       POSITION OF SPACECRAFT IN PLANET COORDINATES
                   RSVECTOR

       OM(9)       ROTATION MATRIX PLANET TO CAMERA (OMMATRIX)

       E(9)        ECCENTRICITY MATRIX

                   E(1) = 1.0
                   E(2) = 0.0
                   E(3) = 0.0
                   E(4) = 0.0
                   E(5) = 1.0
                   E(6) = 0.0
                   E(7) = 0.0
                   E(8) = 0.0
                   E(9) = EQUATORIAL RADIUS/POLAR RADIUS

       CL          OPTICAL AXIS LINE       , OBJECT SPACE PIXELS R*4

       CS          OPTICAL AXIS SAMPLE     , OBJECT SPACE PIXELS R*4
               
       FLAG        IF POINT IS OFF PLANET RLAT, RLON ARE SET TO FLAG R*4

2  HISTORY

      Original Programmer: J. J. Lorre, 16 June 1977
      Current Cognizant Programmer: J. Lorre
      Source Language: Fortran
      Ported to UNIX: Steve Pohorsky
      Revisions: 
         20 April 1986 ...JAM... added more digits to PI.
          2 March 1993  ..SP...  Made portable.  Added zippcov for calls from C.
$ Return
$!#############################################################################
