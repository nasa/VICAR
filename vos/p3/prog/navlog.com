$!****************************************************************************
$!
$! Build proc for MIPL module navlog
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:24:00
$!
$! Execute by entering:		$ @navlog
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
$ write sys$output "*** module navlog ***"
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
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to navlog.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
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
$   if F$SEARCH("navlog.imake") .nes. ""
$   then
$      vimake navlog
$      purge navlog.bld
$   else
$      if F$SEARCH("navlog.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake navlog
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @navlog.bld "STD"
$   else
$      @navlog.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create navlog.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack navlog.com -mixed -
	-s navlog.f -
	-p navlog.pdf -
	-i navlog.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create navlog.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'

C  NAVLOG
C  PURPOSE:  PRODUCE AN IBIS INTERFACE FILE FROM NAVIGATION TAPE
C  USER PARAMETERS:
C

	SUBROUTINE MAIN44
	IMPLICIT INTEGER(A-Z)
	BYTE BUF(51200) ! BUFFER FOR INPUT BLOCKS
	CHARACTER*51200 CBUF
	BYTE NAME(60),LATS,LONGS
	REAL SEC,LATM,LONGM,LAT,LONG,TH,TAT,PITCH,ROLL,PRT5,DP,DRIFT
	REAL C1(5000),C2(5000),C3(5000),C4(5000),C5(5000),C6(5000)
	REAL C7(5000),C8(5000),C9(5000),C10(5000),C11(5000),C12(5000)
	REAL C13(5000),C14(5000),C15(5000),C16(5000),C17(5000),C18(5000)
	REAL C19(5000),C20(5000),C21(5000)
C
C---- READ PARAMETERS. OPEN FILES
C
	CALL XVP('INP',NAME,NI)
	CALL XVUNIT(UNIT,'INP',1,STAT,' ')
	CALL XVOPEN(UNIT,STAT,' ') ! TRY TO OPEN WITH VICAR LABEL
	IF(STAT.NE.1) CALL XVOPEN(UNIT,STAT,'COND','NOLABELS',' ')
	CALL XVSIGNAL(UNIT,STAT,-1)
	CALL XVGET(UNIT,STAT,'NS',NS,'NL',NL,' ')
	PRINT *,' NL',NL
	IF(NL.GT.5000) THEN
	    PRINT *,' NL REDUCED TO 5000'
	    NL=5000
	ENDIF
	IF(NS.NE.2048) THEN
	    CALL XVMESSAGE(' Nuber of samples must be 2048',' ')
	    CALL ABEND
	ENDIF
C
	DO 1000 L=1,NL
	    CALL XVREAD(UNIT,BUF,STAT,' ')
	    CALL MVLC(BUF,CBUF,51200)
	    IF(STAT.LT.0) GOTO 2000 ! IF EOF
c	    DECODE (36,10,BUF(1025),err=130) RC,DAY,HR,MN,SEC
	    READ (fmt=10,unit=CBUF(1025:),err=130) RC,DAY,HR,MN,SEC
   10	    FORMAT(I5,21X,I3,I2,I2,F3.1)
c	    DECODE (4,20,BUF(1141),err=25) DRIFT
	    READ (fmt=20,unit=CBUF(1141:),err=25) DRIFT
   20	    FORMAT(F4.1)
   25	    CONTINUE
c	    DECODE (6,30,BUF(1167),err=35) LATS,LATD,LATM
	    READ (fmt=30,unit=CBUF(1167:),err=35) LATS,LATD,LATM
   30	    FORMAT(A1,I2,F3.1)
   35	    CONTINUE
c	    DECODE (7,40,BUF(1182),err=45) LONGS,LONGD,LONGM
	    READ (fmt=40,unit=CBUF(1182:),err=45) LONGS,LONGD,LONGM
   40	    FORMAT(A1,I3,F3.1)
   45	    CONTINUE
c	    DECODE (3,50,BUF(1198),err=55) GS
	    READ (fmt=50,unit=CBUF(1198:),err=55) GS
   50	    FORMAT(I3)
   55	    CONTINUE
c	    DECODE (4,20,BUF(1223),err=60) TH
	    READ (fmt=20,unit=CBUF(1223:),err=60) TH
   60	    CONTINUE
c	    DECODE (3,50,BUF(1236),err=65) WS
	    READ (fmt=50,unit=CBUF(1236:),err=65) WS
   65	    CONTINUE
c	    DECODE (3,50,BUF(1248),err=70) WA
	    READ (fmt=50,unit=CBUF(1248:),err=70) WA
   70	    CONTINUE
c	    DECODE (6,75,BUF(1285),err=80) PRT5
	    READ (fmt=75,unit=CBUF(1285:),err=80) PRT5
   75	    FORMAT(F6.1)
   80	    CONTINUE
c	    DECODE (6,75,BUF(1291),err=85) DP
	    READ (fmt=75,unit=CBUF(1291:),err=85) DP
   85	    CONTINUE
c	    DECODE (5,90,BUF(1297),err=95) TAT
	    READ (fmt=90,unit=CBUF(1297:),err=95) TAT
   90	    FORMAT(F5.2)
   95	    CONTINUE
c	    DECODE (5,100,BUF(1303),err=105) PALT
	    READ (fmt=100,unit=CBUF(1303:),err=105) PALT
  100	    FORMAT(I5)
  105	    CONTINUE
c	    DECODE (4,20,BUF(1347),err=110) PITCH
	    READ (fmt=20,unit=CBUF(1347:),err=110) PITCH
  110	    CONTINUE
c	    DECODE (4,20,BUF(1351),err=115) ROLL
	    READ (fmt=20,unit=CBUF(1351:),err=115) ROLL
  115	    CONTINUE
c	    DECODE (5,100,BUF(1363),err=120) RALT
	    READ (fmt=100,unit=CBUF(1363:),err=120) RALT
  120	    CONTINUE
c	    DECODE (26,125,BUF(1427),err=130)TSITE,TFLT,TPROJ,TLINE,TRUN
	    READ (fmt=125,unit=CBUF(1427:),err=130)TSITE,TFLT,TPROJ,
     +				TLINE,TRUN
  125	    FORMAT(I3,7X,I3,3X,I5,I3,I2)
  130	    CONTINUE
C
	    C1(L)=RC ! REC CTR
	    C2(L)=3600.*HR+60.*MN+SEC ! TIME IN SECS
	    C3(L)=DAY
	    C4(L)=TSITE
	    C5(L)=TLINE ! THUMBWHEEL LINE NO
	    C6(L)=TRUN
	    C7(L)=TPROJ
	    C8(L)=DRIFT
	    LAT=LATD+LATM/60. ! LATITUDE IN DEGREES
	    IF(LATS.EQ.ichar('S')) LAT=-LAT ! SOUTH = MINUS
	    C9(L)=LAT
	    LONG=LONGD+LONGM/60.
	    IF(LONGS.EQ.ichar('W')) LONG=-LONG
	    C10(L)=LONG ! LONGITUDE, WEST = MINUS
	    C11(L)=GS ! GROUND SPEED
	    C12(L)=TH ! TRUE HEADING
	    C13(L)=WS ! WIND SPEED
	    C14(L)=WA ! WIND ANGLE
	    C15(L)=PRT5
	    C16(L)=DP
	    C17(L)=TAT ! TOTAL AIR TEMP
	    C18(L)=PALT ! PRESSURE ALTITUDE
	    C19(L)=PITCH
	    C20(L)=ROLL
	    C21(L)=RALT ! RADAR ALTITUDE
 1000	CONTINUE
C
	GO TO 3000
 2000	CONTINUE
	PRINT *,' ERROR AT LINE',L
	NL=L-1
 3000	CONTINUE
	CALL XVCLOSE(UNIT,STAT,' ')
	CALL WRFIL(UNIT,1,NL,21,STAT)
	CALL PUTCOL(UNIT,1,NL,C1)
	CALL PUTCOL(UNIT,2,NL,C2)
	CALL PUTCOL(UNIT,3,NL,C3)
	CALL PUTCOL(UNIT,4,NL,C4)
	CALL PUTCOL(UNIT,5,NL,C5)
	CALL PUTCOL(UNIT,6,NL,C6)
	CALL PUTCOL(UNIT,7,NL,C7)
	CALL PUTCOL(UNIT,8,NL,C8)
	CALL PUTCOL(UNIT,9,NL,C9)
	CALL PUTCOL(UNIT,10,NL,C10)
	CALL PUTCOL(UNIT,11,NL,C11)
	CALL PUTCOL(UNIT,12,NL,C12)
	CALL PUTCOL(UNIT,13,NL,C13)
	CALL PUTCOL(UNIT,14,NL,C14)
	CALL PUTCOL(UNIT,15,NL,C15)
	CALL PUTCOL(UNIT,16,NL,C16)
	CALL PUTCOL(UNIT,17,NL,C17)
	CALL PUTCOL(UNIT,18,NL,C18)
	CALL PUTCOL(UNIT,19,NL,C19)
	CALL PUTCOL(UNIT,20,NL,C20)
	CALL PUTCOL(UNIT,21,NL,C21)
	CALL XVCLOSE(UNIT,STAT,' ')
	RETURN
C
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create navlog.pdf
process help=*
PARM  INP	TYPE=(STRING,80)
PARM  OUT	TYPE=(STRING,80)
END-PROC

.TITLE
VICAR/IBIS Program NAVLOG

.HELP
PURPOSE:
NAVLOG converts NAV data into an IBIS interface file with 21 columns.
The NAV data must be copied from tape to disk before NAVLOG is run.
This can be done with the DCL TAPES program or with VICAR CONVIM.

The IBIS data is stored as :
Column	Data
 1	Record counter
 2	Time in seconds
 3	Day
 4	Thumbwheel site #
 5	Thumbwheel line #
 6	Thumbwheel run #
 7	Thumbwheel project #
 8	Drift
 9	Latitude in degrees (negative for South)
10	Longitude in degrees (negative for West)
11	Ground speed
12	True heading
13	Wind speed
14	Wind angle
15	PRT-5 (many values are in error, replaced by zero)
16	Dew point
17	Total air temperature
18	Pressure altitude (feet)
19	Pitch
20	Roll
21	Radar altitude (feet)

EXECUTION:

DCL MOU/FOR MSA0:
DCL TAPES MSA0/FIXED/OUT=A.DAT/FILE=1

NAVLOG A.DAT B.INT


Original Programmer :  Howard J. Frieden   February 1991
Cognizant Programmer:  Howard J. Frieden 

.LEVEL1
.VARIABLE INP
Input filename.  
.VARIABLE OUT
Output filename.
$ Return
$!#############################################################################
$Imake_File:
$ create navlog.imake
#define  PROGRAM   navlog

#define MODULE_LIST navlog.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
