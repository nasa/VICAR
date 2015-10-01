$!****************************************************************************
$!
$! Build proc for MIPL module timsnav
$! VPACK Version 1.5, Monday, March 29, 1993, 14:50:46
$!
$! Execute by entering:		$ @timsnav
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
$!   PDF         Only the PDF file is created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$ write sys$output "*** module timsnav ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Imake = ""
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
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Imake = "Y"
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
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("timsnav.imake") .nes. ""
$   then
$      vimake timsnav
$      purge timsnav.bld
$   else
$      if F$SEARCH("timsnav.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake timsnav
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @timsnav.bld "STD"
$   else
$      @timsnav.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create timsnav.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack timsnav.com -
	-s timsnav.f -
	-p timsnav.pdf -
	-i timsnav.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create timsnav.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C  TIMSNAV
C  PURPOSE: ADD NAV DATA TO TIMS AUX FILE
C
	SUBROUTINE MAIN44
	IMPLICIT INTEGER(A-Z)
	CHARACTER*80 PRT
	REAL*8 SEC
	REAL AUX(41) ! BUFFER FOR AUX DATA
	REAL C2(5000),C8(5000),C9(5000),C10(5000),C11(5000),C12(5000)
	REAL C13(5000),C14(5000),C15(5000),C16(5000),C17(5000),C18(5000)
	REAL C19(5000),C20(5000),C21(5000)
	REAL R1,OSEC,DEL,SSEC,TSEC,TIME
	INTEGER HMS(3)
	LOGICAL QTEST/.TRUE./
C
C---- READ PARAMETERS. OPEN FILES
C
	CALL XVUNIT(UNIT,'INP',1,STAT,' ')
	CALL XVOPEN(UNIT,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
	CALL XVGET(UNIT,STAT,'NS',NS,'NL',NL,' ')
	WRITE (PRT,10) NL,NS
   10	FORMAT(' TIMS AUX input NL =',I5,'   NS =',I3)
	CALL XVMESSAGE(PRT,' ')
	IF(NS.LT.31) THEN
	    CALL XVMESSAGE(
     +		' TIMS NS LT 31, use ALL option in TIMSLOG',' ')
	    CALL ABEND
	ENDIF
C
	CALL RDFIL(NUNIT,2,CLEN,NUMC,STAT) ! OPEN NAV INTERFACE FILE
	CLEN1 = CLEN-1
	IF(NUMC.NE.21) THEN
	    CALL XVMESSAGE(' Columns in NAV file NE 21',' ')
	    CALL ABEND
	ENDIF
	CALL GETCOL(NUNIT,2,CLEN,C2) ! GET TIMES (SECONDS) IN C2
	CALL GETCOL(NUNIT,8,CLEN,C8) ! DRIFT
	CALL GETCOL(NUNIT,9,CLEN,C9) ! LATITUDE
	CALL GETCOL(NUNIT,10,CLEN,C10) ! LONGITUDE
	CALL GETCOL(NUNIT,11,CLEN,C11) ! GROUND SPEED
	CALL GETCOL(NUNIT,12,CLEN,C12) ! TRUE HEADING
	CALL GETCOL(NUNIT,13,CLEN,C13) ! WIND SPEED
	CALL GETCOL(NUNIT,14,CLEN,C14) ! WIND ANGLE
	CALL GETCOL(NUNIT,15,CLEN,C15) ! PRT-5
	CALL GETCOL(NUNIT,16,CLEN,C16) ! DEW POINT
	CALL GETCOL(NUNIT,17,CLEN,C17) ! TOTAL AIR TEMP
	CALL GETCOL(NUNIT,18,CLEN,C18) ! PRESSURE ALTITUDE
	CALL GETCOL(NUNIT,19,CLEN,C19) ! PITCH
	CALL GETCOL(NUNIT,20,CLEN,C20) ! ROLL
	CALL GETCOL(NUNIT,21,CLEN,C21) ! RADAR ALTITUDE
C
	CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
	CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_NS',41,' ')
	CALL XVP('STIME',HMS,CNT)
	SEC = 3600.*HMS(1) + 60.*HMS(2) + HMS(3)
	CALL SETTIME(UNIT,TIME,DEL,NL)
	IF (SEC .EQ. 0.0) THEN
	    SEC = TIME - DEL
	ELSE
	    SEC = SEC - DEL
	END IF
	SSEC = -999.99
	OSEC = SSEC
	IC = 1
	LEND = 0
	LST = NL+1
C
	DO 100 L=1,NL
	    CALL XVREAD(UNIT,AUX,STAT,'LINE',L,' ')
	    AUX(40) = AUX(32)
	    AUX(41) = AUX(33)
	    TSEC=3600.*AUX(18)+60.*AUX(19)+AUX(20)
C
C				If this time is not the same as the last time,
C				but within 0.2 seconds of expected time, accept
C				this time as accurate.  Otherwise, project a new
C				time based upon the previous line's time and the
C				scan speed.
	    IF (TSEC.NE.SSEC .AND. ABS(TSEC-OSEC-DEL).LT.0.2) THEN
		SEC = TSEC
	    ELSE
		SEC = SEC + DEL
	    END IF
	    ISEC = SEC
	    MINS = ISEC/60
	    AUX(18) = MINS/60
	    AUX(19) = MOD(MINS,60)
	    AUX(20) = SEC - 60.0*MINS
C
	    DO 60 II=IC,CLEN1
		IF (SEC .LE. C2(II+1)) GO TO 80
   60	    CONTINUE
	    II = CLEN1
C
   80	    CONTINUE
C							Is extrapolation needed?
	    IF (SEC .LT. C2(1)) LEND=L
	    IF (SEC .GT. C2(CLEN) .AND. QTEST) THEN
		QTEST = .FALSE.
		LST = L
	    END IF
C
	    IC = II
	    R1=(SEC-C2(IC))/(C2(IC+1)-C2(IC))      ! INTERPOLATION FACTOR
	    AUX(25)=C9(IC)+R1*(C9(IC+1)-C9(IC))    ! LATITUDE
	    AUX(26)=C10(IC)+R1*(C10(IC+1)-C10(IC)) ! LONGITUDE
	    AUX(27)=C12(IC)+R1*(C12(IC+1)-C12(IC)) ! TRUE HEADING
	    AUX(28)=C19(IC)+R1*(C19(IC+1)-C19(IC)) ! PITCH
	    AUX(29)=C20(IC)+R1*(C20(IC+1)-C20(IC)) ! ROLL
	    AUX(30)=C11(IC)+R1*(C11(IC+1)-C11(IC)) ! GROUND SPEED
	    AUX(31)=C8(IC)+R1*(C8(IC+1)-C8(IC))    ! DRIFT
	    AUX(32)=C21(IC)+R1*(C21(IC+1)-C21(IC)) ! RADAR ALTITUDE (FT)
	    AUX(32)=AUX(32)/3.281                     ! CONVERT TO METERS
	    AUX(33)=C18(IC)+R1*(C18(IC+1)-C18(IC)) ! PRESSURE ALTITUDE (FT)
	    AUX(33)=AUX(33)/3.281                     ! CONVERT TO METERS
	    AUX(34)=C17(IC)+R1*(C17(IC+1)-C17(IC)) ! TOTAL AIR TEMP
	    AUX(35)=C16(IC)+R1*(C16(IC+1)-C16(IC)) ! DEW POINT
	    AUX(36)=C15(IC)+R1*(C15(IC+1)-C15(IC)) ! PRT-5
	    AUX(37)=C14(IC)+R1*(C14(IC+1)-C14(IC)) ! WIND ANGLE
	    AUX(38)=C13(IC)+R1*(C13(IC+1)-C13(IC)) ! WIND SPEED
	    AUX(39)=AUX(33)-AUX(32)                ! GROUND ELEVATION (METERS)
	    CALL XVWRIT(OUNIT,AUX,STAT,' ')
	    OSEC=SEC                         ! SAVE OLD SEC (TIME THAT WAS USED)
	    IF(TSEC.NE.0.) SSEC=TSEC         ! ALSO SAVE TIMS TIME, IF NON-ZERO
  100	CONTINUE
C
	IF (LEND .GT. 0) THEN
	    WRITE (PRT,150) 1,LEND
	    CALL XVMESSAGE(PRT,' ')
	END IF
	IF (LST .LE. NL) THEN
	    WRITE (PRT,150) LST,NL
	    CALL XVMESSAGE(PRT,' ')
	END IF
  150	FORMAT(' Extrapolation used on Lines',I6,' -',I6)
C
	CALL XVCLOSE(UNIT,STAT,' ')
	CALL XVCLOSE(OUNIT,STAT,' ')
	CALL XVCLOSE(NUNIT,STAT,' ')
	RETURN
C
	END
C*******************************************************************************
	SUBROUTINE SETTIME(INUNIT,TIME,DEL,NL)
C
C	This routine searches for 3 scan lines of one time followed by 3 scan
C	lines of a different time.  It is assumed that the time on the first
C	scan line with the second time is correct.  The exact time of the first
C	scan line is then computed, using the scan speed to determine the
C	time interval from the first scan line to the first scan line of the 
C	second time.
C
	REAL BUF(41)
	SECS(HR,XMIN,SEC) = 3600.0*HR + 60.0*XMIN + SEC
C
	CALL XVREAD(INUNIT,BUF,ISTAT,' ')
	VAL = SECS(BUF(18),BUF(19),BUF(20))
	IF (BUF(24) .NE. 0.0) DEL = 1.0/BUF(24)
	II = 1
C
  100	CONTINUE
	DO I=II+1,NL			! do we have 3 lines of the first value?
	    CALL XVREAD(INUNIT,BUF,ISTAT,' ')
	    X = SECS(BUF(18),BUF(19),BUF(20))
	    IF (X .NE. VAL) THEN
		VAL = X
		IF (I-II .LT. 3) THEN		! NO - reset and try again
		    II = I
		    GO TO 100
		ELSE				! YES - go look for 3 of 2nd val
		    II = I
		    GO TO 200
		END IF
	    END IF
	END DO
	CALL XVMESSAGE(
     +		' Unable to find a reliable sequence of times',' ')
	CALL ABEND
C
  200	CONTINUE
	DO I=1,2
	    CALL XVREAD(INUNIT,BUF,ISTAT,' ')
	    X = SECS(BUF(18),BUF(19),BUF(20))
	    IF (X .NE. VAL) THEN		! reset and try again from start
		II = II+I
		VAL = X
		GO TO 100
	    END IF
	END DO
C						! valid pattern of times found
	IF (BUF(24) .NE. 0.0) DEL = 1.0/BUF(24)
	TIME = VAL - (II-1)*DEL
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create timsnav.pdf
process help=*
PARM  INP	TYPE=STRING	COUNT=2
PARM  OUT	TYPE=STRING
PARM  STIME	TYPE=INTEGER	COUNT=3		DEFAULT=(0,0,0)
END-PROC

.TITLE
VICAR/IBIS Program TIMSNAV
.HELP
PURPOSE:
TIMSNAV adds NAV data to the TIMS Auxiliary file.
It requires two inputs, the TIMS Auxiliary file created with the 'ALL
option and the NAVLOG output file.  The output is a new TIMS Auxiliary
file with eight additional fields (Radar Altitude, Pressure Altitude,
Total Air Temp, Dew Point, PRT-5, Wind Angle, Wind Speed, and Ground
Elevation) plus NAV data inserted into seven of the existing fields.

The data is stored as follows :

Samp	Data
 1	Ref Temp 1
 2	Ref Temp 2
 3-14	Data for 6 channels
15	Year
16	Month
17	Day
18	Hour
19	Minute
20	Second
21	(Sortie)
22	Line
23	Gain
24	Scan speed
25	Latitude in degrees *
26	Longitude in degrees *
27	True Heading *
28	Pitch *
29	Roll *
30	Ground Speed *
31	Drift *
32	Radar Altitude (meters) **
33	Pressure Altitude (meters) **
34	Total Air Temp **
35	Dew Point **
36	PRT-5 **
37	Wind Angle **
38	Wind Speed **
39	Ground Elevation (meters)  (33-32) **
40      Ambient Temperature #1 ***
41      Ambient Temperature #2 ***

* indicates inserted data
** indicates new data
*** indicates relocated data

EXECUTION:

TIMSNAV (TIMS.AUX,NAV.INT) TIMS.NAV

METHOD:

TIMSNAV reads time from the TIMS data and uses the data for the
corresponding time in the NAV data.  The TIMS time is typically given
in integer seconds even though data is taken up to 25 times a second.
When two consecutive times are the same, or inconsisent, TIMSNAV will 
adjust the time according to the scanning speed to get fractional seconds.

It then finds the two times in the NAV data that cover this time
and interpolates between the data entries for the two times.
The TIMS time is sometimes missing (stored as zero) and TIMSNAV
uses the scanning speed to calculate the expected time.

However, if the time is missing at the beginning of the file, the
user must manually input it with the STIME option.

PARAMETERS:

STIME=(Hour, Minute, Second) must be used when start time is missing.


Original Programmer :  Howard J. Frieden   February 1991
Cognizant Programmer:  Howard J. Frieden 

.LEVEL1
.VARIABLE INP
Input filenames.  
First input is the
TIMSLOG Auxiliary file
created with 'ALL option.
Second file is the
NAVLOG output file that
includes the TIMS time.
.VARIABLE OUT
Output filename.
A new Auxiliary file.
.VARIABLE STIME
When initial time
is invalid, user 
must input it as
(Hour, Minute, Second)
$ Return
$!#############################################################################
$Imake_File:
$ create timsnav.imake
#define  PROGRAM   timsnav

#define MODULE_LIST timsnav.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
