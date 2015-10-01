$!****************************************************************************
$!
$! Build proc for MIPL module tmslog
$! VPACK Version 1.5, Monday, March 29, 1993, 14:50:46
$!
$! Execute by entering:		$ @tmslog
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
$ write sys$output "*** module tmslog ***"
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
$   if F$SEARCH("tmslog.imake") .nes. ""
$   then
$      vimake tmslog
$      purge tmslog.bld
$   else
$      if F$SEARCH("tmslog.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tmslog
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tmslog.bld "STD"
$   else
$      @tmslog.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tmslog.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tmslog.com -
	-s tmslog.f -
	-p tmslog.pdf -
	-i tmslog.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tmslog.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C  27 NOV 91   ...REA...   New Program
C   5 FEB 92   ...REA...   Place gains in AUX, rather than label
C
	INTEGER IP1(12),IP2(12)
	INTEGER SLO,SSO,ELO,PIX,AUX
	REAL*4  OUTBUF(42)
	LOGICAL*1 INBUF(9192)
C
C	  ***  Open input data set ***
C
	CALL XVUNIT(INP,'INP',1,ISTAT,' ')
	CALL XVOPEN(INP,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C	  ***  Parameter processing ***
C
	CALL XVPCNT('OUT',NO)
	CALL XVSIZE(SLO,SSO,NLO,NSO,NLIN,NSIN)
	IF (NSO.EQ.NSIN) NSO=716
C
C	  ***  Set pointers for each channel ***
C
	DO ICHAN = 1,12
	    IP1(ICHAN) = ((ICHAN-1)*766)+1
	    IP2(ICHAN) = IP1(ICHAN)+SSO+49
	END DO
C
C	  ***  Open first output data set ***
C
	CALL XVUNIT(PIX,'OUT',1,STAT,' ')
	CALL XVOPEN(PIX,STAT,'OPEN_ACT','SA','IO_ACT','SA','U_NL',
     &		NLO,'U_NS',NSO,'U_NB',12,'U_ORG','BIL','OP','WRITE',
     &		'O_FORMAT','BYTE','U_FORMAT','BYTE',' ')
C
C	  *** For standard processing, write an auxiliary file ***
C
	IF(NO.GT.1) THEN
	    CALL XVUNIT(AUX,'OUT',2,ISTAT,' ')
	    CALL XVOPEN(AUX,ISTAT,'OPEN_ACT','SA','IO_ACT','SA','U_NL',
     &		NLO,'U_NS',42,'U_ORG','BSQ','OP','WRITE',
     &		'U_FORMAT','REAL','O_FORMAT','REAL',' ')
	ENDIF
C
C	  *** Process data records ***
C
	ELO = SLO + NLO - 1
	DO ILINE = SLO,ELO
	    CALL XVREAD(INP,INBUF,ISTAT,'LINE',ILINE,' ')
C
	    IF (ILINE .EQ. SLO) THEN
		IRUN = I2VAL(INBUF(3))
		IDATE = I4VAL(INBUF(9))
		JDATE = JULDAT(IDATE)
		SCANSPD = I2VAL(INBUF(17))/10.0
		DEMAG = I2VAL(INBUF(25))/100.0
		CALL XLADD(PIX,'HISTORY','DATE',JDATE,ISTAT,
     & 		           'FORMAT','INT',' ')
		CALL XLADD(PIX,'HISTORY','RUN',IRUN,ISTAT,
     & 		           'FORMAT','INT',' ')
		CALL XLADD(PIX,'HISTORY','SCANSPD',SCANSPEED,ISTAT,
     &		           'FORMAT','REAL',' ')
		CALL XLADD(PIX,'HISTORY','DEMAG',DEMAG,ISTAT,
     &			   'FORMAT','REAL',' ')
		IF (NO .GT. 1) THEN
		    CALL XLADD(AUX,'HISTORY','DATE',JDATE,ISTAT,
     &		               'FORMAT','INT',' ')
		    CALL XLADD(PIX,'HISTORY','RUN',IRUN,ISTAT,
     & 				'FORMAT','INT',' ')
	    	    CALL XLADD(AUX,'HISTORY','SCANSPD',SCANSPEED,ISTAT,
     &			       'FORMAT','REAL',' ')
		    CALL XLADD(AUX,'HISTORY','DEMAG',DEMAG,ISTAT,
     &			       'FORMAT','REAL',' ')
		END IF
	    END IF
	    DO I=1,12
		CALL XVWRIT(PIX,INBUF(IP2(I)),ISTAT,' ')
	    END DO
C								write AUX file
	    IF (NO .GT. 1) THEN
		OUTBUF(1) = I2VAL(INBUF(13))/100.0		! BB1 Temp
		OUTBUF(2) = I2VAL(INBUF(15))/100.0		! BB2 Temp
		OUTBUF(27) = I4VAL(INBUF(5))			! Scan Line
		OUTBUF(28) = I4VAL(INBUF(33))/10.0		! Time HHMMSS.T
		OUTBUF(29) = I2VAL(INBUF(41))			! Roll
		OUTBUF(30) = I2VAL(INBUF(1))			! Frame Status
		DO I=1,12
		    OUTBUF(2*I+1) = I2VAL(INBUF(IP1(I)+36))	! BB1 DN
		    OUTBUF(2*I+2) = I2VAL(INBUF(IP1(I)+38))	! BB2 DN
		    OUTBUF(I+30) = I2VAL(INBUF(IP1(I)+28))/1000.0 ! Gain
		END DO
		CALL XVWRIT(AUX,OUTBUF,ISTAT,' ')
	    END IF
	END DO
C
	CALL XVCLOSE(INP,ISTAT,' ')
	CALL XVCLOSE(PIX,ISTAT,' ')
	IF(NO.GT.1) CALL XVCLOSE(AUX,ISTAT,' ')
	RETURN
	END
C******************************************************************************
	INTEGER FUNCTION I2VAL(BUF)
C
	BYTE BUF(2)
C
	IF (BUF(1) .GE. 0) THEN
	    I1 = BUF(1)
	ELSE
	    I1 = BUF(1) + 256
	END IF
	IF (BUF(2) .GE. 0) THEN
	    I2 = BUF(2)
	ELSE
	    I2 = BUF(2) + 256
	END IF
	I2VAL = 256*I1 + I2
	IF (I2VAL .GT. 32767) I2VAL=I2VAL-65536
	RETURN
	END
C******************************************************************************
	INTEGER FUNCTION I4VAL(BUF)
C
	BYTE BUF(4)
C
	IF (BUF(1) .GE. 0) THEN
	    I1 = BUF(1)
	    ISIGN = 0
	ELSE
	    I1 = BUF(1) + 128
	    ISIGN = 1
	END IF
	IF (BUF(2) .GE. 0) THEN
	    I2 = BUF(2)
	ELSE
	    I2 = BUF(2) + 256
	END IF
	IF (BUF(3) .GE. 0) THEN
	    I3 = BUF(3)
	ELSE
	    I3 = BUF(3) + 256
	END IF
	IF (BUF(4) .GE. 0) THEN
	    I4 = BUF(4)
	ELSE
	    I4 = BUF(4) + 256
	END IF
	I4VAL = 16777216*I1 + 65536*I2 + 256*I3 + I4
	IF (ISIGN .EQ. 1) I4VAL = I4VAL - 2**30 - 2**30
	RETURN
	END
C****************************************************************************
	INTEGER FUNCTION JULDAT(NUM)
C
	IYR = NUM/1000000
	JDAY = MOD(NUM,1000)
C
	IF (MOD(IYR,4).NE.0 .AND. JDAY.GT.59) JDAY=JDAY+1
	IF (JDAY .LE. 31) THEN
	    MON = 1
	    IDAY = JDAY
	ELSE IF (JDAY .LE. 60) THEN
	    MON = 2
	    IDAY = JDAY-31
	ELSE IF (JDAY .LE. 91) THEN
	    MON = 3
	    IDAY = JDAY-60
	ELSE IF (JDAY .LE. 121) THEN
	    MON = 4
	    IDAY = JDAY-91
	ELSE IF (JDAY .LE. 152) THEN
	    MON = 5
	    IDAY = JDAY-121
	ELSE IF (JDAY .LE. 182) THEN
	    MON = 6
	    IDAY = JDAY-152
	ELSE IF (JDAY .LE. 213) THEN
	    MON = 7
	    IDAY = JDAY-182
	ELSE IF (JDAY .LE. 244) THEN
	    MON = 8
	    IDAY = JDAY-213
	ELSE IF (JDAY .LE. 274) THEN
	    MON = 9
	    IDAY = JDAY-244
	ELSE IF (JDAY .LE. 305) THEN
	    MON = 10
	    IDAY = JDAY-274
	ELSE IF (JDAY .LE. 335) THEN
	    MON = 11
	    IDAY = JDAY-305
	ELSE
	    MON = 12
	    IDAY = JDAY-335
	END IF
C
	JULDAT = 10000*IYR + 100*MON + IDAY
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create tmslog.pdf
Process help=*
PARM INP     (STRING,40)
PARM OUT     (STRING,40)  COUNT=1:2
PARM SIZE    INTEGER 	  COUNT=0:4 	DEFAULT=--
PARM SL      INTEGER 			DEFAULT=1
PARM SS      INTEGER 			DEFAULT=1
PARM NL      INTEGER 			DEFAULT=0
PARM NS      INTEGER 			DEFAULT=0
End-proc
 
.TITLE
VICAR Program TMSLOG
.HELP
PURPOSE:
 
   TMSLOG is a VICAR program that logs  Daedalus Thematic Mapper Simulator (TMS)
data.
 
OPERATION:
 
   TMSLOG takes a normal raw TMS file taken from a NASA Ames produced tape 
via CONVIM and reformats it into two files; an image file (PIX) 
and, optionally, an auxiliary calibration data file (AUX).  
   The first file (PIX) consists of the 12 bands of data in BIL format.
It may be any number of lines in length.
   The second file (AUX) is optional.  It has a record length of 42 pixels,
in REAL*4 format. The contents are as follows:
.PAGE
     Pixel     Contents
     -----     -----------------------------------
       1       Temperature of first blackbody (degrees C)
       2       Temperature of second blackbody (degrees C)
       3       DN of first blackbody in Channel 1
       4       DN of second blackbody in Channel 1
       .
       .
       .
      25       DN of first blackbody in Channel 12
      26       DN of second blackbody in Channel 12
      27       Scan Line Number
      28       GMT time, expressed as HHMMSS.T
      29       Roll (positive is left) - two counts per pixel
      30       Data frame status     0 = good frame
                                 10-16 = interpolated data
                                 20-26 = repeated data
                                 30-36 = zero fill for data
.PAGE
     Pixel     Contents
     -----     -----------------------------------
      31       Gain of Channel 1
       .
       .
       .
      42       Gain of Channel 12
.PAGE
WRITTEN BY:  Ron Alley  27 NOV 1991

LAST MODIFIED: 5 FEB 1992
 
COGNIZANT PROGRAMMER:  Ron Alley
 
.LEVEL1
.VARI INP
Input raw TMS file
.VARI OUT
(1) Output image file
(2) Output calibration data
.VARI SIZE
Window into input
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI NL
Number of lines
.VARI NS
Number of samples
.END
$ Return
$!#############################################################################
$Imake_File:
$ create tmslog.imake
#define  PROGRAM   tmslog

#define MODULE_LIST tmslog.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
