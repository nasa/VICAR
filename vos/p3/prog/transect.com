$!****************************************************************************
$!
$! Build proc for MIPL module transect
$! VPACK Version 1.8, Friday, October 25, 1996, 18:43:04
$!
$! Execute by entering:		$ @transect
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
$ write sys$output "*** module transect ***"
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
$ write sys$output "Invalid argument given to transect.com file -- ", primary
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
$   if F$SEARCH("transect.imake") .nes. ""
$   then
$      vimake transect
$      purge transect.bld
$   else
$      if F$SEARCH("transect.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake transect
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @transect.bld "STD"
$   else
$      @transect.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create transect.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack transect.com -
	-s transect.f -
	-p transect.pdf -
	-i transect.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create transect.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C -------------------------------------------------------------
C  13 JUL 89    REA   NEW PROGRAM
C  25 APR 90    REA   INCLUDE AXISPTS, AXISPTS2 SUBROUTINES
C  18 SEP 90    REA   ADD LASER PLOTTING CAPABILITY
C  12 MAY 92    REA   ADD 'PRINT OPTION
C  12 JUL 96    REA   CONVERT FROM REGIS TO IDL USAGE
C
	REAL X(40000),Y(40000),BUF(40000)
	INTEGER IBUF(2)
	LOGICAL XVPTST,QPRINT
	CHARACTER*80 TITLE,MSG
	CHARACTER*48 TITLE2
C
C     PROCESS THE PARAMETERS
C
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'U_FORMAT','REAL','OPEN_ACT','SA',
     +              'IO_ACT','SA',' ')
C                                                     START
	CALL XVPARM('START',IBUF,ICNT,IDEF,0)
	ISL = IBUF(1)
	ISS = IBUF(2)
C						      END
	CALL XVPARM('END',IBUF,ICNT,IDEF,0)
	IEL = IBUF(1)
	IES = IBUF(2)
C						      BAND
	CALL XVPARM('BAND',IBAND,ICNT,IDEFBAND,0)	
	IF (IDEFBAND.EQ.0) THEN
	    WRITE (TITLE2,100) ISL,ISS,IEL,IES,IBAND,CHAR(0)
  100	    FORMAT('From (',I5,',',I5,') to (',I5,',',I5,') in Band',
     +		   I4,A1)
	ELSE
	    WRITE (TITLE2,200) ISL,ISS,IEL,IES,CHAR(0)
  200	    FORMAT('From (',I5,',',I5,') to (',I5,',',I5,')',A1)
	END IF
C						      YAXIS
	CALL XVPARM('YAXIS',X,ICNT,IDEF,0)
	YLO = X(1)
	YHI = X(2)
C							PRINT
	QPRINT = XVPTST('PRINT')
	IF (QPRINT) CALL XVMESSAGE(
     +		'      Line    Sample   Distance       DN',' ')
C							TITLE
	CALL XVP('TITLE',TITLE,ICNT)
	CALL ADD0(TITLE,80,LEN)
C
	DX = ABS(IES-ISS)
	DY = ABS(IEL-ISL)
	IF (DY .GE. DX) THEN				! mostly vertical
	    XINC = SQRT(DX*DX + DY*DY)/DY
	    IF (IEL .GT. ISL) THEN
		INC = 1
	    ELSE
		INC = -1
	    END IF
	    N = 0
	    DO I=ISL,IEL,INC
		FRAC = ABS(I-ISL)/DY
		ISAMP = (1.0-FRAC)*ISS + FRAC*IES + 0.5
		N = N+1
		CALL XVREAD(INUNIT,Y(N),ISTAT,'NSAMPS',1,'SAMP',ISAMP,
     +			    'LINE',I,'BAND',IBAND,' ')
		X(N) = (N-1)*XINC
		IF (QPRINT) THEN
		    WRITE (MSG,500) I,ISAMP,X(N),Y(N)
  500		    FORMAT(2I10,F10.3,F15.4)
 		    CALL XVMESSAGE(MSG,' ')
 		END IF
	    END DO
	ELSE						! mostly horizontal
	    XINC = SQRT(DX*DX + DY*DY)/DX
	    IF (IES .GT. ISS) THEN
		INC = 1
	    ELSE
		INC = -1
	    END IF
	    N = 0
	    LLINE = 0
	    DO I=ISS,IES,INC
		FRAC = ABS(I-ISS)/DX
		LINE = (1.0-FRAC)*ISL + FRAC*IEL + 0.5
		IF (LINE .NE. LLINE) CALL XVREAD(INUNIT,BUF,ISTAT,
     +			    		'LINE',LINE,'BAND',IBAND,' ')
		LLINE = LINE
		N = N+1
		X(N) = (N-1)*XINC
		Y(N) = BUF(I)
	 	IF (QPRINT) THEN
		    WRITE (MSG,500) LINE,I,X(N),Y(N)
		    CALL XVMESSAGE(MSG,' ')
		END IF
	    END DO
	END IF
C
	CALL PLOTXY(X,Y,N,YLO,YHI,INUNIT,TITLE,TITLE2)
	RETURN
	END
C
C****************************************************************************
C
	SUBROUTINE PLOTXY(XBUF,YBUF,NUM,Y1,Y2,INUNIT,TITLE,TITLE2)
C
	REAL XBUF(NUM),YBUF(NUM)
	CHARACTER*80 TITLE
	CHARACTER*48 TITLE2
	CHARACTER*40 XLABEL,YLABEL,MSG
	LOGICAL QPSPLOT
C								scale x-axis
	CALL MINMAX(7,NUM,XBUF,X1,X2,IMIN,IMAX)
	IF (X1.EQ.X2) THEN
	    CALL XVMESSAGE(
     +		' More than 1 value is needed for plotting.',' ')
	    CALL ABEND
	ENDIF
	CALL AXISPTS(X1,X2,XLO,XHI,NXTIC)
C								scale Y-axis
	IF (Y1.EQ.Y2) THEN
	    CALL MINMAX(7,NUM,YBUF,Y1,Y2,IMIN,IMAX)
	    IF (Y1.EQ.Y2) Y2 = Y1+1
	    CALL AXISPTS(Y1,Y2,YLO,YHI,NYTIC)
	ELSE
	    CALL AXISPTS2(Y1,Y2,NYTIC)
	    YLO = Y1
	    YHI = Y2
	END IF
C								   plot the data
	OPEN (11,FILE='scrvtransect001',STATUS='NEW')
	WRITE (11,100) XLO,XHI,YLO,YHI
  100	FORMAT(4E16.7)
	WRITE (11,200) NXTIC,NYTIC
  200	FORMAT(2I10)
	DO I=1,NUM
	    WRITE (11,100) XBUF(I),YBUF(I)
	END DO
	CLOSE(11)
	ISTAT = SYSTEM('idl vicartran.inp')
	OPEN (11,FILE='scrvtransect001',STATUS='OLD')
	CLOSE (11,STATUS='DELETE')
C
	CALL XVINTRACT('IPARAM','Print this plot (Yes, No) [No]? ')
	CALL XVIPARM('PENPLOT',MSG,ICNT,IDEF,0)
	QPSPLOT = MSG(1:1).EQ.'Y' .OR. MSG(1:1).EQ.'y'
C							submit to the printer
	IF (QPSPLOT) THEN
	    CALL XVGET(INUNIT,ISTAT,'NAME',MSG,' ')
	    CALL ADD0(MSG,40,LEN)
	    CALL XVPARM('XLABEL',XLABEL,ICNT,IDEF,0)
	    CALL ADD0(XLABEL,40,LEN)
	    CALL XVPARM('YLABEL',YLABEL,ICNT,IDEF,0)
	    CALL ADD0(YLABEL,40,LEN)
	    CALL PSPLOT(XBUF,YBUF,NUM,XLO,XHI,YLO,YHI,NXTIC,NYTIC,
     +			XLABEL,YLABEL,TITLE,MSG,TITLE2,-1)
	END IF
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create transect.pdf
process help=*
SUBCMD-DEFAULT NORMAL
PARM INP      TYPE=(STRING,40)
PARM START    TYPE=INTEGER COUNT=2
PARM END      TYPE=INTEGER COUNT=2
PARM BAND     TYPE=INTEGER DEFAULT=1
PARM YAXIS    TYPE=REAL COUNT=2 DEFAULT=(0,0)
PARM TITLE    TYPE=(STRING,80) DEFAULT=" "
PARM XLABEL   TYPE=(STRING,40) DEFAULT="Relative Pixel Position"
PARM YLABEL   TYPE=(STRING,40) DEFAULT="DN Value"
PARM PRINT    TYPE=KEYWORD VALID=(PRINT,NOPRINT) DEFAULT=NOPRINT
END-SUBCMD
SUBCMD IPARAM
PARM PENPLOT   TYPE=KEYWORD DEFAULT=-- VALID=(YES,NO) COUNT=(0:1)
END-SUBCMD
END-PROC
.TITLE
TRANSECT
.HELP
PURPOSE:
     TRANSECT is a program for plotting the values along a line within an
image. The user specifies the line and sample coordinates for the end points
of the line, and the DN's are plotted as a function of distance (in pixels)
from the starting point. The user may also designate the band (if multichannel
data), the limits of the y axis, and a title to be placed on the printed plot,
if generated.
     TRANSECT does no interpolation to arrive at the values it plots. If
the transect is mostly horizontal in the image, a value is taken at each
sample, using the nearest line. For the more vertical transects, a value is
taken at each line, using the nearest sample.
.LEVEL1
.VARIABLE INP
Input file name
.VARIABLE START
Line and sample of 
the starting  point
.VARIABLE END
Line and sample of 
the end point
.VARIABLE BAND
Band number (if multichannel)
.VARIABLE YAXIS
Range of y-axis
.VARIABLE TITLE
Title placed on hardcopy plot
.VARIABLE XLABEL
X-axis label (paper plot only)
.VARIABLE YLABEL
Y-axis label (paper plot only)
.VARIABLE PRINT
Print pixel values?
(PRINT or NOPRINT)
.LEVEL2
.VARIABLE INP
Input file name. The file may be BYTE, HALF, FULL, or REAL; if multichannel,
it may be BIL or BSQ.  BIP is not presently implemented.
.VARIABLE START
This is the line and sample of the starting point of the transect (or DN 
profile). The value at this location is plotted at zero on the x-axis.
.VARIABLE END
This is the line and sample of the end point of the transect. The value at
this location is plotted as the rightmost value along the x-axis.
.VARIABLE BAND
If the input dataset is multichannel, BAND designates which band is to be
plotted. The default is Band 1.  
.VARIABLE YAXIS
YAXIS indicates the full scale limits of the y-axis. If defaulted, the program
chooses limits that completely span the range of the data plotted. 
.VARIABLE TITLE
The user may place a line of annotation up to 80 characters in length on the 
printed plots. This annotation does not appear on the screen, and, if a printed
plot is not requested, the value of TITLE is not used.
.VARIABLE XLABEL
When printing on the laser printer, the user may place a label on the X-axis.
The label may be up to 40 characters in length, and does not appear on the 
other plot media. The default is "Relative Pixel Position".
.VARIABLE YLABEL
When printing on the laser printer, the user may place a label on the Y-axis.
The label may be up to 40 characters in length, and does not appear on the 
other plot media. The default is "DN Value".
.VARIABLE PRINT
If the keyword PRINT is given, the pixel locations and values are printed to
the terminal and session log.  Otherwise, only a plot is made.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create transect.imake
#define  PROGRAM   transect

#define MODULE_LIST transect.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
