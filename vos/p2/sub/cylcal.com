$!****************************************************************************
$!
$! Build proc for MIPL module cylcal
$! VPACK Version 1.5, Tuesday, January 05, 1993, 10:34:56
$!
$! Execute by entering:		$ @cylcal
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
$ write sys$output "*** module cylcal ***"
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
$   if F$SEARCH("cylcal.imake") .nes. ""
$   then
$      vimake cylcal
$      purge cylcal.bld
$   else
$      if F$SEARCH("cylcal.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake cylcal
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @cylcal.bld "STD"
$   else
$      @cylcal.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cylcal.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cylcal.com -
	-s cylcal.f -
	-i cylcal.imake -
	-t tcylcal.f tcylcal.imake tcylcal.pdf tstcylcal.pdf -
	-o cylcal.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cylcal.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE CYLCAL(ITYPE,RLAT,RLON,ZC,CPSI,F,FL,REQ,PI,FLAG)
C
C     RETURNS CALCULATED LATITUDE AND LONGITUDE AT
C          DELX SAMPLES,DELZ LINES FROM SAMPLE 1 AT LATITUDE ZERO
C          RLAT=DELX AT ENTRANCE
C             =LATITUDE AT EXIT
C         RLON=DELZ AT ENTRANCE
C           =LONGITUDE AT EXIT
C     CPSI =LONGITUDE OF SAMPLE 1
C     ZC   = LINE OF THE EQUATOR
C     F    = SCAL AT EQUATOR (KM/PIXEL)
C     FL    = POLAR RADIUS (KM)
C     REQ   = EQUATORIAL RADIUS (KM)
C     NOTE THAT DELZ WILL BE NEGATIVE FOR POSITIVE LATITUDE
      REAL*8 RFL,DREQ
      REAL*8 ZDEL ,DRPOLE,RAD,DLAT,DLAT2
      REAL*8 TOL
      DATA   TOL/1.D-7/
C==================================================================
      DRPOLE=DBLE(FL)
      DREQ=DBLE(REQ)
      RFL=DREQ/DRPOLE
      RFL=RFL*RFL
      IF(ITYPE.EQ.2)GO TO 600
100   CONTINUE
      DELX=RLAT-1.
      DELZ=-(RLON-ZC)
C
C     CALCULATE LONGITUDE
      RLON=F*DELX/REQ/PI*180.
      IF(ABS(RLON).GT.360)GO TO 400
      RLON=CPSI-RLON
      RLON=AMOD(360.+RLON,360.)
C
C     CALCULATE LATITUDE
C     WE MUST SOLVE
C     LAT=ARCSIN(X*F/R(LAT))
C     WE WILL ASSUME  THE EQUATORIAL RADIUS
C     AND THEN ITERATE UNTIL CONVERGENCE
C
      ZDEL=DBLE(F*DELZ)
C
C     CHK FOR PT OFF PLANET
      IF(DABS(ZDEL)-DRPOLE)130,120,400
120   CONTINUE
c      LAT=SIGN(90.,DELZ)
      RLAT=SIGN(90.,DELZ)   ! JAM 7-JUL-1985  NO OTHER REFS TO "LAT", MUST BE WRONG
      GO TO 500
130   CONTINUE
      ITER=0
C
C     INITIAL GUESS FOR RADIUS AND LAT
      RAD=DBLE(REQ)
      DLAT=DASIN(ZDEL/RAD)
140   CONTINUE
C
C     CALC LAT AND CHK FOR ITERATION
      RAD=DREQ/(DSQRT(RFL+(1.0D0-RFL)*DCOS(DLAT)**2))
      DLAT2=DLAT
      DLAT=DASIN(ZDEL/RAD)
      IF(DABS(DLAT-DLAT2).LT.TOL)GO TO 160
      ITER=ITER+1
      IF(ITER.LT.100)GO TO 140
      CALL QPRINT(' *** CYLINDRICAL PROJ CONVERGENCE ERROR.',40)
	IZERO=0
      I=1/IZERO
160   CONTINUE
      RLAT=DLAT*180.0D0/ PI
      RETURN
400   RLON=FLAG
      RLAT=FLAG
500   CONTINUE
      RETURN
600   CONTINUE
C
C     GIVEN RLAT/RLON FIND DELX/DELZ
      DELX=REQ*(AMOD(360.-(AMOD(RLON+360.,360.)-CPSI),360.))*PI/180./F
      DELZ=ABS(RLAT)
      IF(DELZ.LT.90.0)GO TO 610
C
C     WE ARE AT THE POLE
      DELZ=SIGN(FL/F,RLAT)
      DELZ=-DELZ
      GO TO 700
610   CONTINUE
C
C     FIND RADIUS AT RLAT
      DLAT=RLAT*PI/180.
      RAD=DREQ/(DSQRT(RFL+(1.0D0-RFL)*DCOS(DLAT)**2))
      DELZ=RAD/F*SIN(RLAT*PI/180.)
      DELZ=-DELZ
700   CONTINUE
      RLAT=DELX+1.
      RLON=DELZ+ZC
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create cylcal.imake
/* Imake file for VICAR subroutine cylcal */

#define SUBROUTINE cylcal

#define MODULE_LIST cylcal.f

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tcylcal.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C                 VARIABLE DEFINITIONS
C
      CHARACTER*480 BUFFER
C==================================================================
      PI = 3.14159265
      REQ = 6378.15
      FL = 6356.76
      F = 0.5
      CPSI = 120.0
      ZC = 0.0
      ITYPE = 1
C
      RLAT = 500.0
      RLON = 500.0
C
      WRITE(BUFFER,100)ITYPE,RLAT,RLON,FLAG,ZC,CPSI,F,FL,
     1              REQ,PI
      CALL XVMESSAGE(' ',' ')  
      CALL XVMESSAGE(BUFFER(2:80),' ')
      CALL XVMESSAGE(BUFFER(82:160),' ')
      CALL XVMESSAGE(BUFFER(162:240),' ')
      CALL XVMESSAGE(' ',' ')
C
      CALL CYLCAL(ITYPE,RLAT,RLON,ZC,CPSI,F,FL,REQ,PI,FLAG)
C
      WRITE(BUFFER,110)ITYPE,RLAT,RLON,FLAG,ZC,CPSI,F,FL,
     1              REQ,PI
      CALL XVMESSAGE(BUFFER(2:80),' ')
      CALL XVMESSAGE(BUFFER(81:160),' ')
      CALL XVMESSAGE(BUFFER(162:240),' ')
      CALL XVMESSAGE(' ',' ')
C
      ITYPE = 2
      CALL CYLCAL(ITYPE,RLAT,RLON,ZC,CPSI,F,FL,REQ,PI,FLAG)
C      
      WRITE(BUFFER,120)ITYPE,RLAT,RLON,FLAG,ZC,CPSI,F,FL,
     1              REQ,PI
      CALL XVMESSAGE(BUFFER(2:80),' ')
      CALL XVMESSAGE(BUFFER(81:160),' ')
      CALL XVMESSAGE(BUFFER(162:240),' ')
      CALL XVMESSAGE(' ',' ')
C      
      RETURN
  100 FORMAT(' INITIAL CONDITIONS FOR LINE/SAMPLE TO LAT/LONG:',
     1       32X,' ITYPE: ',I2,'  LINE:     ',F10.2,'  SAMPLE:    ',
     2       F10.2,'  FLAG: ',F10.2,7X,' ZC: ',F3.1,'  CPSI: ',F7.2,
     3       '  F: ',F5.2,'  FL: ',F8.1,'  REQ: ',F8.1,'  PI: ',F6.4,
     4       '   ')
  110 FORMAT(' AFTER CYLCAL CALL:',61X,
     1       'ITYPE: ',I2,'  LATITUDE: ',F10.2,'  LONGITUDE: ',F10.2,
     2       '  FLAG: ',F10.2,9X,'ZC: ',F3.1,'  CPSI: ',F7.2,'  F: ',
     3       F5.2,'  FL: ',F8.1,'  REQ: ',F8.1,'  PI: ',F6.4,5X)
  120 FORMAT(' AFTER CYLCAL CALL FOR LAT/LONG TO LINE/SAMPLE:',33X,
     1       'ITYPE: ',I2,'  LINE:     ',F10.2,'  SAMPLE:    ',F10.2,
     2       '  FLAG: ',F10.2,9X,'ZC: ',F3.1,'  CPSI: ',F7.2,'  F: ',
     3       F5.2,'  FL: ',F8.1,'  REQ: ',F8.1,'  PI: ',F6.4,5X)
      END
$!-----------------------------------------------------------------------------
$ create tcylcal.imake
/* Imake file for Test of VICAR subroutine cylcal */

#define PROGRAM tcylcal

#define MODULE_LIST tcylcal.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tcylcal.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstcylcal.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tcylcal
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create cylcal.hlp
1 CYLCAL

	CYLCAL calculates the line, sample position on a 
	Lambert cylindrical equal area projection, for a 
	specified latitude and longitude on a planet.

2 USAGE

  Calling sequence:

        CALL CYLCAL(ITYPE,RLAT,RLON,ZC,CPSI,F,FL,REQ,PI,FLAG)

  Arguments:
	ITYPE (input, integer): tranformation flag
	RLAT  (in/out, real): DELX/LAT at entrance, LAT/DELX at exit
	RLON  (in/out, real): DELZ/LON at entrance, LON/DELZ at exit
	ZC    (input, real): line of the equator
	CPSI  (input, real): longitude of sample 1
	F     (input,real): scale at equator (km/pixel)
	FL    (input, real): polar radius (km)
	REQ   (input,real): equatorial radius (km)
	PI    (input, real): 3.14159265 (???)
	FLAG  (input, integer): off-planet flag

2 HISTORY

  Original Programmer:  A. A. Schwartz, 8-10-79
  Current Cognizant Programmer: Steve Pohorsky
  Documentation Author:  A. A. Schwartz
  Source Language:  FORTRAN
  Ported to UNIX: Steve Pohorsky
  Revision: 1 7-JUL-1985 Joel Mosher error returning latitude in direct case
2 PURPOSE

  CYLCAL calculates the line, sample position on a Lambert
  cylindrical equal area projection for a specified latitude
  and longitude on a planet.  Given a line, sample position 
  on a cylindrical map, the program calculates the latitude
  and longitude.  This routine is designed to be used by the
  general mapping subroutine CONVEV, and in most cases it is 
  appropriate for the user to call CONVEV rather than directly 
  calling CYLCAL.  Note that for an oblate spheroid this
  projection is not strictly equal area, because there is not
  an intermediate projection onto the authalic sphere.

2 ARGUMENTS
    
3 ITYPE
        Flag indicating which type of transformation is being processed:
          1 - line & sample to latidute & longitude
          2 - latitude & longitude to line & sample

3 RLAT
	The real variable which at entrance contains either the
	line or latitude of a point, depending upon ITYPE, and 
	on exit contains the converse.
3 RLON
 	The real variable which at entrance contains either the 
	sample or longitude of a point, depending upon ITYPE, and
	on exit contains the converse.
3 ZC
  	A real variable which contains the line position of the 
	equator in the cylindrical projection.
3 CPSI
        A real variable which contains the longitude of sample
	one in the cylindrical projection.
3 F
        A real variable which contains the projection scale at
	the equator (Km/pixel).
3 FL
        A real variable which contains the polar radius of a planet
3 REQ
        A real variable which contains the equatorial radius of 
	the planet.
3 PI
        A single precision value of PI (3.14159265)
3 FLAG
        A real variable which contains the off-planet flag value
	for an ITYPE=1 transformation.  If a line, sample position
	specified is off the planet then RLONG and RLAT are set 
	to this flag value.


2 OPERATION

          Given a line, sample position on a cylindrical map,
          the program calculates the latitude and longitude.
          This routine is designed to be used by the general
          mapping subroutine CONVEV, and in most cases it is more
          appropriate for the user to call CONVEV rather than directly
          calling CYLCAL.  Note that for an oblate spheroid this projection
          is not strictly equal area, because there is not an intermediate
          projection onto the authalic sphere.
2 METHOD
          For ITYPE = 1 (line & sample to latitude & longitude) the 
          following  equations are solved.

            LAMDA = ARCSIN (x * k / R'lamda)     (latitude)
            PHI   = CPSI - (y * k / r) * 180/PI  (longitude)

          Where  x = ZC - SAMPLE
                y = LINE - 1
                k = F
                r = REQ
       
          Note: The latitude equation is a recursive relationship. There
               may be formulations which avoid this problem, but the problem
               treats it as follows.
               First an approximate value for LAMDA is calculated using
               R'lamda = r.  This value of LAMDA is then used to calculate
               a better value for the radius using the equation:

               R'lamda = A / ((A*A)/(B*B) + (1 - (A*A)/(B*B)) * (W*W))

               Where A = REQ,  B = FL  &  w = cos LAMDA

          This procedure is repeated using the new value of R'lamda, until
          successive values of LAMDA differ by less than single precision
          significance.

          For ITYPE = 2 the following equations are solved.

             x = ZC - (R'lamda / k) sin LAMDA

             y = ((PI * R) / (180 * k)) * (0 - CPSI) + 1

$ Return
$!#############################################################################
