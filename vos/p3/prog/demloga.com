$!****************************************************************************
$!
$! Build proc for MIPL module demloga
$! VPACK Version 1.8, Friday, April 04, 1997, 08:02:14
$!
$! Execute by entering:		$ @demloga
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
$ write sys$output "*** module demloga ***"
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
$ write sys$output "Invalid argument given to demloga.com file -- ", primary
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
$   if F$SEARCH("demloga.imake") .nes. ""
$   then
$      vimake demloga
$      purge demloga.bld
$   else
$      if F$SEARCH("demloga.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake demloga
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @demloga.bld "STD"
$   else
$      @demloga.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create demloga.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack demloga.com -
	-s demloga.f -
	-i demloga.imake -
	-p demloga.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create demloga.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C**********************************************************************
      SUBROUTINE MAIN44
C
C     DEMLOGA:
C
C    THIS PROGRAM WILL LOG USGS DEM FILES THAT ARE IN THE FORMAT DESCRIBED
C	IN THE USGS NATIONAL MAPPING DIVISION OPEN-FILE REPORT 86-004
C	"STANDARDS FOR DIGITAL ELEVATION MODELS"
C
C    THIS FORMAT DIFFERS FROM THE ONE KNOWN TO VDEMLOG; IT INVOLVES
C	UNLABELLED TAPES WITH FIXED LENGTH BLOCKSIZE OF 4096 BYTES.
C	ALL DATA ARE STORED AS ASCII CHARACTERS IN 1024 BYTE LOGICAL
C	RECORDS.
C
C	WRITTEN BY RON ALLEY, NOV 1986
C	MODIFIED TO INCLUDE BLOCKING FACTOR PARAMETER, AUG 1987
C	converted to unix, Ron Alley, November 1994
C	modified to handle <CR> delimited records,  Ron Alley,  April, 1997
C
	REAL*8 CORNER(2,4),ZMIN,ZMAX,ANGLE,YLOC,OFFSET
	INTEGER*2 LEVEL,IPATTERN,ISYSTEM,IZONE,IXY,IZ,NSIDES
	INTEGER*2 NROWS,NCOLS,NUMREC,NPTS,IBUF2(10000),IOUT(10000)
	INTEGER*2 IZERO/0/
	CHARACTER*32768 XBUF
	CHARACTER*60 NAME
	CHARACTER*132 PRT
	CHARACTER*11 UNITS(5)/'Radians','Feet','Meters','Arc-Seconds',
     +			      ' '/
C							  get blocking factor
	CALL XVPARM('BLOCK',IBLKFAC,ICNT,IDEF,0)
	IF (IBLKFAC .EQ. 0) THEN
	    NSIN = 1025
	ELSE
	    NSIN = 1024 * IBLKFAC
	ENDIF
C
C							     open input file
	CALL XVUNIT(INUNIT,'INP',1,ISTAT,' ')
	CALL XVOPEN(INUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'U_NS',NSIN,'COND','NOLABELS',' ')
C
	CALL XVREAD(INUNIT,XBUF,ISTAT,' ')
	READ (XBUF,100) NAME,LEVEL,IPATTERN,ISYSTEM,IZONE,IXY,IZ,
     +		NSIDES,CORNER,ZMIN,ZMAX,ANGLE,DX,DY,DZ,NROWS,NCOLS
  100	FORMAT(A60,84X,4I6,360X,3I6,4(2D24.15),3D24.15,6X,3E12.6,2I6)
	IF ((IXY.GT.4) .OR. (IXY.LT.0)) IXY=4
	IF ((IZ.GT.2) .OR. (IZ.LT.1)) IXY=4
C							report to screen/log
	CALL XVMESSAGE(NAME,' ')
	WRITE (PRT,102) ZMIN,ZMAX,UNITS(IZ+1)
  102	FORMAT('Range =',F7.1,' to',F7.1,' ',A11)
	CALL XVMESSAGE(PRT,' ')
C
C					Check for the oddball cases that 
C					this version won't handle.
C
	IF (IPATTERN.NE.1) THEN
	    CALL XVMESSAGE(
     +		' Random pattern logging not implemented.',' ')
	    CALL ABEND
	ENDIF
	IF (NSIDES.NE.4) THEN
	    CALL XVMESSAGE(
     +	 ' Logging of non-quadrilateral regions is not implemented.',' ')
	    CALL ABEND
	ENDIF
	IF (NROWS.NE.1) THEN
	    CALL XVMESSAGE(
     +	     ' Logging of multiple row profiles is not implemented.',' ')
	    CALL ABEND
	ENDIF
C
	XBASE = MIN(CORNER(1,1),CORNER(1,2),CORNER(1,3),CORNER(1,4))
	XTOP = MAX(CORNER(1,1),CORNER(1,2),CORNER(1,3),CORNER(1,4))
	YBASE = MIN(CORNER(2,1),CORNER(2,2),CORNER(2,3),CORNER(2,4))
	YTOP = MAX(CORNER(2,1),CORNER(2,2),CORNER(2,3),CORNER(2,4))
	NS = (YTOP-YBASE)/DY + 1
	NL = NCOLS
C								Open output file
	CALL XVUNIT(IOUTUNIT,'OUT',1,ISTAT,' ')
	CALL XVOPEN(IOUTUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',
     +		    'OP','WRITE','U_NL',NL,'U_NS',NS,'U_FORMAT','HALF',
     +		    'O_FORMAT','HALF',' ')
C
C						Add location name to VICAR label
	CALL XLADD(IOUTUNIT,'HISTORY','LOCATION',NAME,ISTAT,
     +		   'FORMAT','STRING',' ')
C
C					    Add geographic coordinates to label
	IF (ISYSTEM.LE.1) THEN
	    IF (ISYSTEM.EQ.1) THEN
		WRITE (PRT,110) IZONE
  110		FORMAT(I4)
		CALL XLADD(IOUTUNIT,'HISTORY','UTM_ZONE',PRT,ISTAT,
     +			   'FORMAT','STRING',' ')
	    END IF
	    WRITE (PRT,120) YBASE,YTOP
  120	    FORMAT(2F14.1)
	    CALL XLADD(IOUTUNIT,'HISTORY','NORTHING',PRT,ISTAT,
     +		   'FORMAT','STRING',' ')
	    WRITE (PRT,120) XBASE,XTOP
	    CALL XLADD(IOUTUNIT,'HISTORY','EASTING',PRT,ISTAT,
     +		   'FORMAT','STRING',' ')
	ELSE IF (ISYSTEM.EQ.2) THEN
	    WRITE (PRT,130) IZONE
  130	    FORMAT(I6)
	    CALL XLADD(IOUTUNIT,'HISTORY','ST_CODE',PRT,ISTAT,
     +		   'FORMAT','STRING',' ')
	ELSE
	    WRITE (PRT,130) ISYSTEM
	    CALL XLADD(IOUTUNIT,'HISTORY','PROJECT',PRT,ISTAT,
     +		   'FORMAT','STRING',' ')
	END IF
C						     Put pixel spacing in label
	IF (DX .EQ. DY) THEN
	    WRITE (PRT,150) DX,UNITS(IXY+1)
  150	    FORMAT(F8.2,' ',A11)
	ELSE
	    WRITE (PRT,160) DX,DY,UNITS(IXY+1)
  160	    FORMAT(F8.2,' by',F8.2,' ',A11)
	END IF
	CALL XLADD(IOUTUNIT,'HISTORY','1_PIXEL',PRT,ISTAT,
     +			'FORMAT','STRING',' ')
C							Put elevation resolution
C							in label
	WRITE (PRT,170) DZ,UNITS(IZ+1)
  170	FORMAT(F8.2,' ',A11)
	CALL XLADD(IOUTUNIT,'HISTORY','1_DN',PRT,ISTAT,
     +			'FORMAT','STRING',' ')
C							Put elevation range in
C							label
	WRITE (PRT,180) ZMIN,ZMAX
  180	FORMAT(F8.2,' -',F8.2)
	CALL XLADD(IOUTUNIT,'HISTORY','RANGE',PRT,ISTAT,
     +			'FORMAT','STRING',' ')
C							Put quality level code
C							in label
	WRITE (PRT,190) LEVEL
  190   FORMAT(I2)
	CALL XLADD(IOUTUNIT,'HISTORY','LEVEL',PRT,ISTAT,
     +			'FORMAT','STRING',' ')
C							Put rotation angle in
C							label, if not 0
	IF (ANGLE.NE.0) THEN
	    WRITE (PRT,195) ANGLE
  195	    FORMAT(F7.2)
	    CALL XLADD(IOUTUNIT,'HISTORY','ROTATION',PRT,ISTAT,
     +			'FORMAT','STRING',' ')
	END IF
C							Build image
	N = 2
	DO LINE=1,NL
	    IF (N.GT.IBLKFAC) THEN
		CALL XVREAD(INUNIT,XBUF,ISTAT,' ')
		N = 1
	    END IF
	    READ(XBUF(1024*(N-1)+1:1024*N),200) NUMREC,NPTS,YLOC,OFFSET,
     +					(IBUF2(I), I=1,146)
  200	    FORMAT(6X,2I6,30X,2D24.15,48X,146I6)
	    N = N+1
	    NBUF = 146
	    IF (NUMREC.NE.LINE) THEN
		CALL XVMESSAGE(' Record sequence error.',' ')
		CALL ABEND
	    ENDIF
	    DO WHILE (NPTS.GT.NBUF)
		IF (N.GT.IBLKFAC) THEN
		    CALL XVREAD(INUNIT,XBUF,ISTAT,' ')
		    N = 1
		END IF
		READ (XBUF(1024*(N-1)+1:1024*N),210) (IBUF2(I), 
     +						      I=NBUF+1,NBUF+170)
  210		FORMAT (170I6)
		N = N+1
		NBUF = NBUF+170
	    END DO
	    IF (OFFSET.NE.0.0) THEN
		DO I=1,NPTS
		    IBUF2(I) = IBUF2(I)+OFFSET+0.5
		END DO
	    END IF
	    NPAD = (YLOC-YBASE)/DY
	    IF (NPAD .NE. 0) THEN
		DO LOC=1,NPAD
		    IOUT(LOC) = IZERO
		END DO
	    END IF
	    DO LOC=1,NPTS
		LOC2 = LOC + NPAD
		IOUT(LOC2) = IBUF2(LOC)
	    END DO
	    NUM = NPAD+NPTS
	    IF (LOC2 .LT. NS) THEN
		DO LOC=LOC2+1,NS
		    IOUT(LOC) = IZERO
		END DO
	    END IF
	    CALL XVWRIT(IOUTUNIT,IOUT,ISTAT,' ')
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create demloga.imake
#define  PROGRAM   demloga

#define MODULE_LIST demloga.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
$PDF_File:
$ create demloga.pdf
PROCESS HELP=*
PARM INP     TYPE=(STRING,60)
PARM OUT     TYPE=(STRING,60)
PARM BLOCK   TYPE=INTEGER     DEFAULT=1 VALID=(0:32)

END-PROC

.TITLE
VICAR Program DEMLOGA
.HELP
PURPOSE:
     DEMLOGA logs into VICAR format DEM (Digital Elevation Model) files obtained
from the USGS. Specifically, this program will log usgs DEM FILES that are in 
the format described in the USGS National Mapping Division Open-file Report 
86-004 "Standards for Digital Elevation Models".
     This format differs from the ones known to VDEMLOG and DTTLOG; it involves
unlabelled tapes or datasets with fixed length blocksize of some multiple of 
1024 bytes. All data are stored as ASCII characters in 1024 byte logical 
records.
                            ***UPDATE***
     The program has been modified to handle datasets with carriage return
delimited records.  These files may be identified by their having a
dataset length that is a multiple of 1025, rather than 1024.  To process
this type of file, use the parameter BLOCK=0

     DEMLOGA is not normally run directly, but is instead called by the 
procedure DEMLOG. Running DEMLOGA outside the procedure DEMLOG yields an image
with west at the top, rather than north.
     Only a single file was available for testing this program, so the 
correctness of this code could not be checked for many of the possible data 
forms. If programming errors exist, they will most likely be in the values
placed in the VICAR label, rather than in the image itself. Examine the VICAR
label carefully before proceeding. Report all problems to Ron Alley, x40751.
.PAGE
EXECUTION:

Example

DEMLOGA  TAPE/4 OUTIMAGE

This command will log the 4th file on the specified drive's tape from
DEM-format into VICAR format. No parameter or SIZE field is allowed.  


DEMLOG TAPE/2 TOPO BLOCK=19

This command will log the second file onthe specified drive's tape. BLOCK=19
implies that there are 19 x 1024 = 19,456 bytes per block on the tape.

WRITTEN BY:  Ron Alley, November, 1986
LATEST REVISION: April, 1997
COGNIZANT PROGRAMMER:  Ron Alley
.LEVEL1
.VARIABLE INP
Input unlabelled file
.VARIABLE OUT
Output file
.VARIABLE BLOCK
Blocking factor on tape
(bytes per record/1024)
For <CR> delimited disk
files, use BLOCK=0
.END
$ Return
$!#############################################################################
