$!****************************************************************************
$!
$! Build proc for MIPL module strippit
$! VPACK Version 1.9, Wednesday, March 10, 2010, 12:32:24
$!
$! Execute by entering:		$ @strippit
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
$ write sys$output "*** module strippit ***"
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
$ write sys$output "Invalid argument given to strippit.com file -- ", primary
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
$   if F$SEARCH("strippit.imake") .nes. ""
$   then
$      vimake strippit
$      purge strippit.bld
$   else
$      if F$SEARCH("strippit.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake strippit
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @strippit.bld "STD"
$   else
$      @strippit.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create strippit.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack strippit.com -mixed -
	-s strippit.f -
	-p strippit.pdf -
	-i strippit.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create strippit.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C  11 DEC 85   ...REW...    CREATION DATE
C  29 APR 91   ...REA...    CONVERTED TO UNIX/VICAR
C
C
C		** Declare variables **
C
	INTEGER BUFLEN, NSTRIPS, ISS(100)
	INTEGER SL, SS, NS, NLINES, NSSTRIP, ISTRIP, ILINE
	INTEGER OUT, NL, ISTAT, INLINE, IGAP, INP(10)
	LOGICAL*1 BUF(10000)
	LOGICAL FLAG
C
C		** Process parameters **
C
	FLAG = .TRUE.
	CALL XVPARM('STRIPLEN',NLINES,NNL,ISTAT,0)
	CALL XVPARM('GAPWIDTH',IGAP,NGP,ISTAT,0)
C
C		** Open input data set for first time **
C
	CALL XVUNIT(INP(1),'INP',1,ISTAT,' ')
	CALL XVOPEN(INP(1),STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
C
C		** Set up necessary parameters **
C
	CALL XVSIZE(SL,SS,NL,NS,NLIN,NSIN)
	NSTRIPS = (NL + NLINES-1)/NLINES
	DO I = 2,NSTRIPS
	    CALL XVUNIT(INP(I),'INP',I,ISTAT,' ')
	    CALL XVOPEN(INP(I),STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
	ENDDO
C
	NSSTRIP = NS + IGAP
	BUFLEN  = NSTRIPS*(NSSTRIP)-IGAP
	IF (BUFLEN.GT.10000) THEN
	    CALL XVMESSAGE(
     +	       ' Output image must be no more than 10,000 samples wide',
     +	       ' ')
	    CALL ABEND
	ENDIF
C
C		** Open output data set **
C
	CALL XVUNIT(OUT,'OUT',1,STAT,' ')
	CALL XVOPEN(OUT,STAT,'OPEN_ACT','SA','IO_ACT','SA','U_NL',
     &		NLINES,'U_NS',BUFLEN,'U_ORG','BSQ','OP','WRITE',
     &		'O_FORMAT','BYTE',' ')
C
C		** Set up pointers for XVREAD **
C
	DO ISTRIP = 1,NSTRIPS
	    ISS(ISTRIP) = (ISTRIP-1)*(NS+IGAP)+1
	ENDDO
C
	DO ILINE = 1, NLINES
	    INLINE = ILINE+SL-1
	    DO ISTRIP = 1, NSTRIPS
		IF(INLINE.LE.NLIN) THEN
		    CALL XVREAD(INP(ISTRIP),BUF(ISS(ISTRIP)),ISTAT,
     &		    'LINE',INLINE,'SAMP',SS,'NSAMPS',NS,' ')
		    INLINE = INLINE + NLINES
		ELSE
		    IF(FLAG) THEN
			CALL ITLA(0,BUF(ISS(ISTRIP)),NSSTRIP)
			FLAG = .FALSE.
		    ENDIF
		ENDIF
	    ENDDO
	    CALL XVWRIT(OUT,BUF,ISTAT,'NSAMPS',BUFLEN,' ')
	ENDDO
C
	DO ISTRIP = 1, NSTRIPS
	    CALL XVCLOSE(INP(ISTRIP),STAT,' ')
	ENDDO
	CALL XVCLOSE(OUT,STAT,' ')
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create strippit.pdf
Process help=*
PARM INP	STRING	COUNT=1:10
PARM OUT	STRING	COUNT=1
PARM SIZE	INTEGER	COUNT=4	DEFAULT=(1,1,0,0)
PARM SL		INTEGER COUNT=1 DEFAULT=1
PARM SS		INTEGER	COUNT=1	DEFAULT=1
PARM NL		INTEGER	COUNT=1	DEFAULT=0
PARM NS		INTEGER	COUNT=1	DEFAULT=0
PARM STRIPLEN	INTEGER	COUNT=1	DEFAULT=3750
PARM GAPWIDTH	INTEGER	COUNT=1	DEFAULT=50

End-proc
 
.TITLE
TAE PROCESS STRIPPIT
.HELP
PURPOSE:
 
   STRIPPIT is a TAE process which takes an image and puts it into strips
of STRIPLEN lines with a spacing of GAPWIDTH number of samples between the
strips.  It was written to facilitate the formatting of long and skinny
flight lines for playback.
 
EXECUTION:
 
   The following is the execution statement format for STRIPPIT:
 
		STRIPPIT INP OUT PARAMS
 
   where INP, OUT, and PARAMS are parameters discussed in their respective
parameter sections. 
.PAGE
OPERATION:
 
   STRIPPIT does the seemingly simple task of cutting and pasting long slender
flight line images into parallel strips in order to format the images for
playback.  It uses a slightly more complex algorithm than would seem necessary
because of the inefficiency of non-sequential I/O (especially XVREADing) which
would slow down the operation considerably.  Instead, a maximum size buffer is
allocated for the output, and the reading and writing is done on a block by
block basis, one block being the maximum number of lines by the number of
strips necessary in the output.

   The two unique parameters (other than SIZE) are STRIPLEN and GAPWIDTH.
STRIPLEN lets the user select the number of lines of the strips, and GAPWIDTH
sets the number of samples spacing between the image strips.  The defaults
are set up for standard TIMS processing.

   This program was written to facilitate the processing of TIMS data, which
has come in lengths of over 22,000 lines in a single flight line.  It can just
as well be used by anyone working with long flight lines of AIS, NS001 or other
airborne sensors, however.
 
 
RESTRICTIONS:
 
   As it is currently designed, the output can be in no wider than 10,000
pixels, and must be byte format.

   The input dataset must be repeated in the INP parameter, once for every
strip to be output. For example, an image that is divided into 3 segments must
have the input dataset listed 3 times with the IMP parameter.
.PAGE 
EXAMPLES:
 
1) STRIPPIT (INP,INP) OUT
 
2) STRIPPIT (INP,INP,INP,INP) OUT STRIPLEN=2000 GAPWIDTH=20
 
 
TIMING:
 
WRITTEN BY:  R. E. Walker     16DEC85
 
COGNIZANT PROGRAMMER:  same
 
REVISION: NEW
 
.LEVEL1
.VARI INP
The image file to be cut and
stripped into segments, listed
once for each segment.
.VARI OUT
Output data set containing image
file in parallel pieces.
.VARI SIZE
Size of input window to be used.
.VARI SL
Line at which to begin reading
input.
.VARI SS
Sample at which to begin reading
input.
.VARI NL
The number of lines to be output
Default is to output entire file
.VARI NS
The  number  of  samples  to  be
output. Default is to do entire
file.
.VARI STRIPLEN
The number of lines to be in the
stripped output file.
.VARI GAPWIDTH
The number of samples to be be-
tween the individual strips.
.LEVEL2
.END
$ Return
$!#############################################################################
$Imake_File:
$ create strippit.imake
#define  PROGRAM   strippit

#define MODULE_LIST strippit.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
