$!****************************************************************************
$!
$! Build proc for MIPL module timsresp
$! VPACK Version 1.8, Thursday, February 08, 2001, 17:41:43
$!
$! Execute by entering:		$ @timsresp
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
$ write sys$output "*** module timsresp ***"
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
$ write sys$output "Invalid argument given to timsresp.com file -- ", primary
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
$   if F$SEARCH("timsresp.imake") .nes. ""
$   then
$      vimake timsresp
$      purge timsresp.bld
$   else
$      if F$SEARCH("timsresp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake timsresp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @timsresp.bld "STD"
$   else
$      @timsresp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create timsresp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack timsresp.com -
	-s timsresp.f -
	-p timsresp.pdf -
	-i timsresp.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create timsresp.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
C
C  89-06-24 ...REA...  new program
C  91-05-01 ...REA...  convert to UNIX/VICAR
C  01-02-08 ...REA...  remove screen printing debris 
C
	SUBROUTINE MAIN44
C
	COMMON /RAWFIL/FILTER,WVLEN
	REAL*4 FILTER(158,6),WVLEN(158),CWAVE(6)
	CHARACTER*80 BUF
C					                      process parameters
	CALL XVPARM('DATE',JDATE,ICNT,IDEF,0)
	CALL XVPARM('ADJUST',IADJ,ICNT,IDEF,0)
C							  get response functions
	CALL CENTWAV(JDATE,CWAVE)
C								  print them out
	DO I=1,158
	    WRITE (BUF,100) WVLEN(I),(FILTER(I,J),J=1,6)
  100	    FORMAT(F7.3,'   ',6F10.4)
	    CALL XVMESSAGE(BUF,' ')
	END DO
C							  Plot the TIMS brothers
	CALL PLOTXY(FILTER,WVLEN,JDATE)
C						   Print out central wavelengths
	WRITE (BUF,200) (CWAVE(I),I=1,6)
  200	FORMAT(' Central wavelengths are:',6F7.3)
	CALL XVMESSAGE(BUF,' ')
C
	RETURN
	END
C**********************************************************************
C
	SUBROUTINE PLOTXY(FILTER,WVLEN,JDATE)
C
	REAL FILTER(158,6),WVLEN(158)
	CHARACTER*80 MSG,MSG2,MSG3,MSG4,MSG5
C
	NUM = 158
	XLO = 7.5
	XHI = 12.5
	YLO = 0.0
	YHI = 1.0
	NXTIC = 10
	NYTIC = 10
C							create PostScript file
	WRITE (MSG4,100) JDATE,CHAR(0)
  100	FORMAT(' Calibration in effect on ',I8,A1)
	MSG  = 'Microns' // CHAR(0)
	MSG2 = 'Response' // CHAR(0)
	MSG3 = 'TIMS Response Functions' // CHAR(0)
	MSG5 = ' ' // CHAR(0)
	CALL PSPLOT(WVLEN,FILTER(1,1),NUM,XLO,XHI,YLO,YHI,NXTIC,
     +		NYTIC,MSG,MSG2,MSG3,MSG4,MSG5,0)
	CALL PSPLOT(WVLEN,FILTER(1,2),NUM,XLO,XHI,YLO,YHI,-1,-1,
     +		MSG,MSG2,MSG3,MSG4,MSG5,0)
	CALL PSPLOT(WVLEN,FILTER(1,3),NUM,XLO,XHI,YLO,YHI,-1,-1,
     +		MSG,MSG2,MSG3,MSG4,MSG5,0)
	CALL PSPLOT(WVLEN,FILTER(1,4),NUM,XLO,XHI,YLO,YHI,-1,-1,
     +		MSG,MSG2,MSG3,MSG4,MSG5,0)
	CALL PSPLOT(WVLEN,FILTER(1,5),NUM,XLO,XHI,YLO,YHI,-1,-1,
     +		MSG,MSG2,MSG3,MSG4,MSG5,0)
	CALL PSPLOT(WVLEN,FILTER(1,6),NUM,XLO,XHI,YLO,YHI,-1,-1,
     +		MSG,MSG2,MSG3,MSG4,MSG5,-1)
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create timsresp.pdf
process help=*
SUBCMD-DEFAULT NORMAL
PARM DATE	TYPE=INTEGER
PARM ADJUST	TYPE=INTEGER                 DEFAULT=0
END-SUBCMD
SUBCMD IPARAM
PARM HPPLOT TYPE=KEYWORD DEFAULT=-- VALID=(YES,NO) COUNT=(0:1)
END-SUBCMD
END-PROC
.TITLE
VICAR Program TIMSRESP
.HELP
PURPOSE:
TIMSRESP produces a plot of the TIMS response curves in effect on a specified 
date. The spectral location of these curves may be adjusted by 0.032 micron
increments. 

WRITTEN BY:  Ron Alley,  June, 1989
COGNIZANT PROGRAMMER:  Ron Alley

.LEVEL1
.VARIABLE DATE
Date of interest, as yymmdd
e.g. 870615 for June 15, 1987
.VARIABLE ADJUST
# of 0.032 micron steps to
adjust the spectral calibration
.LEVEL2
.VARIABLE DATE
The program will report the calibration that was in effect on this date. That
is, it will use the last calibration on or prior to this date. The format of
the date should be a number consisting of the last two digits of the year,
the month number (as 2 digits) and the day of month. For example, May 21, 1988
would be entered as 880521.
.VARIABLE ADJUST
The spectral calibration can be adjusted by using this parameter. The
adjustments are made in 0.032 micron steps. For example, ADJUST=2 will take
the sensor response reported at 10.000 microns during calibration and list it
at 10.064 microns.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create timsresp.imake
#define  PROGRAM   timsresp

#define MODULE_LIST timsresp.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
