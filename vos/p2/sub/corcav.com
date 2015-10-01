$!****************************************************************************
$!
$! Build proc for MIPL module corcav
$! VPACK Version 1.9, Monday, December 07, 2009, 16:09:25
$!
$! Execute by entering:		$ @corcav
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
$ write sys$output "*** module corcav ***"
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
$ write sys$output "Invalid argument given to corcav.com file -- ", primary
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
$   if F$SEARCH("corcav.imake") .nes. ""
$   then
$      vimake corcav
$      purge corcav.bld
$   else
$      if F$SEARCH("corcav.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake corcav
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @corcav.bld "STD"
$   else
$      @corcav.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create corcav.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack corcav.com -mixed -
	-s corcav.f zcorcav.c -
	-i corcav.imake -
	-t tcorcav.f tzcorcav.c tcorcav.imake tcorcav.pdf tstcorcav.pdf -
	-o corcav.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create corcav.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Given latitude and longitude of a point on an oblate spheroid,
C returns the line and sample values of that point for a geometrically
C corrected perspective projection.
C
C        PARAMETERS -
C
C          IND = RETURN INDICATOR
C              0 = NORMAL RETURN
C              99 = POINT BEHIND PLANET
C              -1 ON ENTRY SUPPRESSES BACK-OF-PLANET TEST
C          XL = RETURNED LINE VALUE (REAL
C          YS = RETURNED SAMPLE VALUE (REAL)
C          RLAT = INPUT LATITUDE (DEGREES)
C          RLON = INPUT LONGITUDE (DEGREES)
C            RS = INPUT VECTOR TO SPACECRAFT IN PLANET COORDINATE SYSTEM
C            FLAT = INPUT POLAR FLATTENING (EQ. RAD - POL. RAD.)
C            FOCL = INPUT CAMERA FOCAL LENGTH
C            REQ = INPUT PLANET EQUATORIAL RADIUS
C
C
      SUBROUTINE CORCAV(IND,RLAT,RLON,OM,RS,FOCL,REQ,FLAT,XL,YS,
     &CL,CS,FLAG)
      DOUBLE PRECISION OM(3,3),RS(3)
      DIMENSION SP(3),CFP(3)
      REAL PIFAC/57.29577951/
      RAD(PHI,REQ,FLAT)=REQ/SQRT(  REQ*REQ/((REQ-FLAT)**2)+(1.-REQ*REQ/
     &((REQ-FLAT)**2))*(COS(PHI*3.14159265/180.))**2 )

C        SOLVE FOR CAMERA FOCAL PLANE (CFP) COORDINATES

      CPJ = COS(RLAT/PIFAC)
      SPJ = SIN(RLAT/PIFAC)
      CLJ = COS(RLON/PIFAC)
      SLJ = SIN(RLON/PIFAC)

C     ....Compute geocentric radius
      IF (RLAT.EQ.0. .OR. ABS(RLAT).EQ.180.) THEN
         RJ = REQ
      ELSE
         RJ=RAD(RLAT,REQ,FLAT)
      ENDIF

      SP(1)=RJ*CPJ*CLJ-RS(1)
      SP(2)=RJ*CPJ*SLJ-RS(2)
      SP(3)=RJ*SPJ-RS(3)

      DO 120 I = 1,3
  120 CFP(I) = OM(I,1)*SP(1) + OM(I,2)*SP(2) + OM(I,3)*SP(3)

C  SKIP BACK-OF-PLANET TEST IF IND=-1

      IF(IND.EQ.-1)GO TO 130

C        TEST FOR BACK OF PLANET

      ZETA=0.
      TST=0.
      DO 125 I=1,3
      ZETA=ZETA+SP(I)**2
125   TST=TST+RS(I)**2
      TST=TST-RJ**2
      IF(ZETA.LE.TST) GO TO 130
      XS=FLAG
      ZL=FLAG
      IND = 99
      RETURN

C        SOLVE FOR PIXEL COORDINATES

130   U=FOCL*CFP(1)/CFP(3)
      V=FOCL*CFP(2)/CFP(3)
      XL=CL+V
      YS=CS+U
      IND = 0
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zcorcav.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zcorcav - CONVERT LAT,LON TO LINE, SAMP FOR PERSPECTIVE*/
/************************************************************************/

void zcorcav(ind,lat,elo,omma,rsvec,focl,req,flat,line,samp,cl,cs,flag)
int *ind;
float *lat, *elo, *focl, *req, *flat, *line, *samp, *cl, *cs, *flag;
double *omma, *rsvec;
{
FTN_NAME2(corcav, CORCAV) (ind,lat,elo,omma,rsvec,focl,req,flat,line,samp,cl,cs,flag);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create corcav.imake
/* Imake file for VICAR subroutine CORCAV */

#define SUBROUTINE corcav

#define MODULE_LIST corcav.f zcorcav.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tcorcav.f
C         TEST PROGRAM FOR CORCAV SUBPROGRAM
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      DOUBLE PRECISION OM(3,3),RS(3)
      CHARACTER*560 BUFFER
C==================================================================
C  DEFAULT VARIABLES FOR IO

      REQ = 1815.0
      RPOL = 1815.0
      FLAT = REQ - RPOL

C  RANDOM VALUES USED IN TEST

      IND = 0
      XL  = 0.
      YS  = 0.
      FOCL = 1500.19 * 84.821
      RS(1) = -730950.0D0
      RS(2) = -339690.0D0
      RS(3) = -1209.3D0
      OM(1,1) = 0.407057464D0
      OM(2,1) = 0.112318814D0
      OM(3,1) = 0.906470418D0
      OM(1,2) = -0.874813914D0
      OM(2,2) = -0.237469494D0
      OM(3,2) = 0.422266245D0
      OM(1,3) = 0.262687564D0
      OM(2,3) = -0.964879572D0
      OM(3,3) = 0.0015943704D0
      RLAT = -13.8018
      RLON = 360.0-150.1259
      CL = 500.0
      CS = 500.0
 
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('LONGITUDE IS EAST ',' ')
      CALL XVMESSAGE('BEFORE CONVERSION FROM LAT/LONG TO LINE/SAMPLE:',
     .             ' ')
      WRITE(BUFFER,100)RLAT,RLON,CL,CS,IND,XL,YS,FLAG,RS,OM,FOCL,
     1            REQ,FLAT
      CALL XVMESSAGE(BUFFER(  2: 80),' ')
      CALL XVMESSAGE(BUFFER( 82:160),' ')
      CALL XVMESSAGE(BUFFER(162:240),' ')
      CALL XVMESSAGE(BUFFER(242:320),' ')
      CALL XVMESSAGE(BUFFER(322:400),' ')
      CALL XVMESSAGE(BUFFER(402:480),' ')
      CALL XVMESSAGE(BUFFER(482:560),' ')
      CALL XVMESSAGE(' ',' ')

      CALL CORCAV(IND,RLAT,RLON,OM,RS,FOCL,REQ,FLAT,XL,YS,
     1            CL,CS,FLAG)
      CALL XVMESSAGE('AFTER CONVERSION TO LINE/SAMPLE:',' ')
      CALL XVMESSAGE('LINE SHOULD BE=614.1, SAMPLE SHOULD BE 614.38',
     .               ' ')
      WRITE(BUFFER,100)RLAT,RLON,CL,CS,IND,XL,YS,FLAG,RS,OM,FOCL,
     1            REQ,FLAT
      CALL XVMESSAGE(BUFFER(  2: 80),' ')
      CALL XVMESSAGE(BUFFER( 82:160),' ')
      CALL XVMESSAGE(BUFFER(162:240),' ')
      CALL XVMESSAGE(BUFFER(242:320),' ')
      CALL XVMESSAGE(BUFFER(322:400),' ')
      CALL XVMESSAGE(BUFFER(402:480),' ')
      CALL XVMESSAGE(BUFFER(482:560),' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(
     . 'Repeat test case in C to test C interface: zcorcav', ' ')

      call tzcorcav( OM, RS)

      RETURN
  100 FORMAT(' RLAT:',F8.2,'E.RLON:',F8.2,4X,'CL:',E11.5,'  CS:',
     1       E11.5,17X,' IND:',I2,'  LINE:',E12.5,'  SAMPLE:',E12.5,
     2       '  FLAG:',E11.5,15X,' RS: ',3(E11.5,4X),30X,' OM:',2(
     3       3(E11.5,4X),35X),3(E11.5,4X),31X,' FOCL:',E11.5,4X,
     4       'REQ:',E11.5,4X,'FLAT:',E11.5,20X)
      END
$!-----------------------------------------------------------------------------
$ create tzcorcav.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzcorcav)(omma,rsvec) 
double *omma, *rsvec;
{
float lat, elo, focl, req, flat, line, samp, cl, cs, flag, rpol;
int ind;
/*  ==================================================================  */
      req = 1815.0;
      rpol = 1815.0;
      flat = req - rpol;
      ind = 0;
      line  = 0.;
      samp  = 0.;
      focl = 1500.19 * 84.821;
      lat = -13.8018;
      elo = 360.0-150.1259;
      cl = 500.0;
      cs = 500.0;
  zcorcav(&ind,&lat,&elo,omma,rsvec,&focl,&req,&flat,&line,&samp,&cl,&cs,&flag);
      zprnt(7,1,&line," LINE =.");
      zprnt(7,1,&samp," SAMP =.");

}
$!-----------------------------------------------------------------------------
$ create tcorcav.imake
/* Imake file for Test of VICAR subroutine CORCAV */

#define PROGRAM tcorcav

#define MODULE_LIST tcorcav.f tzcorcav.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create tcorcav.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstcorcav.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tcorcav
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create corcav.hlp
1 CORCAV
 To convert from lat/lon to line/sample in a geometrically
 corrected perspective projection.

 FORTRAN Calling Sequence:  
  CALL CORCAV(IND,LAT,ELO,OMMA,RSVEC,FOCL,REQ,FLAT,LINE,SAMP,CL,CS,FLAG)

 C Calling Sequence:  
  zcorcav(&ind,&lat,&elo,omma,rsvec,&focl,&req,&flat,&line,&samp,&cl,&cs,&flag);
      Note the & for all scalar arguments denoting the passing by address.

2 ARGUMENTS
 ind   - (i*4,return)- = 0 normal,  = 99 point behind planet
       - (i*4,entry) - = -1  backside test is skipped.
 lat   - (r*4,entry) - latitude in degrees
 elo   - (r*4,entry) - east longitude in degrees
 omma  - (r*8,entry,9 element array) - OM-matrix
 rsvec - (r*8,entry,3 element array) - RS-vector
 focl  - (r*4,entry) - camera focal length
 req   - (r*4,entry) - equatorial radius of planet
 flat  - (r*4,entry) - req minus polar radius
 line  - (r*4,return) - line coordinate in object space pixels
 samp  - (r*4,return) - sample coordinate in obect space pixels
 cl    - (r*4,entry) - obj. sp. line of optical axis
 cs    - (r*4,entry) - obj. sp. samp of optical axis
 flag  - (r*4,entry) - if point is behind planet, then line=samp=flag
2 HISTORY
 WRITTEN BY : J. J. LORRE,  16 JUNE 1977
 COGNIZANT PROGRAMMER:  J. LORRE
 Ported to UNIX: Steve Pohorsky
 REVISIONS:
   21 AUG 89  ..GMY..  Check for lat=0 or 180 when computing geocentric radius
   23 FEB 93  ..SP...  Made portable.  Added zcorcav for calls from C.
$ Return
$!#############################################################################
