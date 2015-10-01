$!****************************************************************************
$!
$! Build proc for MIPL module rapidmos
$! VPACK Version 1.8, Wednesday, May 17, 1995, 16:49:36
$!
$! Execute by entering:		$ @rapidmos
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
$!   TEST        Only the test files are created.
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
$ write sys$output "*** module rapidmos ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to rapidmos.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
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
$   if F$SEARCH("rapidmos.imake") .nes. ""
$   then
$      vimake rapidmos
$      purge rapidmos.bld
$   else
$      if F$SEARCH("rapidmos.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rapidmos
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rapidmos.bld "STD"
$   else
$      @rapidmos.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rapidmos.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rapidmos.com -
	-s rapidmos.f -
	-i rapidmos.imake -
	-p rapidmos.pdf -
	-t tstrapidmos.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rapidmos.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C---- VICAR PROGRAM "RAPIDMOS.
C     PURPOSE: TAKES MULTIPLE IMAGES AND MOSAICS THEM
C              INTO ONE OUTPUT DATA SET.THE OPERATION IS
C              SIMILAR TO THAT OF "FASTMOS", BUT WITH
C              REDUCED EXECUTION TIME AND MANY NOT WIDELY
C              USED FUNCTIONS REMOVED.
C
C     PROGRAMMER: BORIS GOKHMAN, APRIL 1981.
C
C	EXTENSIVELY MODIFIED BY  K.F. EVANS  APRIL 1986
C
C       PORTED TO UNIX BY C.R SCHENK (CRI) MAY 1995
C       MSTP S/W CONVERSION (VICAR PORTING)

	IMPLICIT NONE
	BYTE	OUTBUFFER(50000), BUFFER(50000)
	INTEGER MAXINP
	PARAMETER (MAXINP=40)
	INTEGER RUNIT(MAXINP), OFFL(MAXINP), OFFS(MAXINP), NS(MAXINP)
	LOGICAL LFLAG(MAXINP), RFLAG(MAXINP)

	INTEGER	OFFSETS(2*MAXINP)
	INTEGER	FIRSTPIX(MAXINP), LASTPIX(MAXINP)
	INTEGER	NL(MAXINP)

	INTEGER	I, IX, IX1, IX2, INP
	INTEGER	WUNIT, STATUS, NINP
	INTEGER NOFF, OFFDEF, NPIX
	INTEGER	SLOUT, SSOUT, NLOUT, NSOUT
	INTEGER	NLMIN, NLMAX, LINE, NLI, NSI
	CHARACTER*72 INPFILES(MAXINP)

	COMMON /RAPIDCOM/ RUNIT, OFFL, OFFS, NS, LFLAG, RFLAG

C
C---- READ PARAMETERS, OPEN FILES, PROCESS LABELS.
C
C
        CALL IFMESSAGE('RAPIDMOS verion 08-May-1995')
	CALL XVP ('INP', INPFILES, NINP)
	DO I = 1,NINP
	    CALL XVUNIT (RUNIT(I), 'INP', I, STATUS,' ')
	    CALL XVOPEN (RUNIT(I), STATUS,  
     *			'OPEN_ACT','SA', 'IO_ACT','SA',' ')
	    CALL XVGET (RUNIT(I), STATUS, 'NL',NL(I), 'NS',NS(I),' ')
	ENDDO
	CALL XVPARM ('OFFSETS', OFFSETS, NOFF, OFFDEF,80)
	CALL XVSIZE (SLOUT,SSOUT, NLOUT,NSOUT,NLI,NSI)

	CALL XVUNIT (WUNIT,'OUT',1,STATUS,' ')
	CALL XVOPEN (WUNIT, STATUS, 'OP','WRITE',
     *            'U_NL',NLOUT, 'U_NS',NSOUT,
     *            'OPEN_ACT','SA', 'IO_ACT','SA',' ')

	DO I = 1, NINP
	    OFFL(I) = 1
	    OFFS(I) = 1
	    LFLAG(I) = .FALSE.
	    RFLAG(I) = .FALSE.
	ENDDO
	IF (OFFDEF.EQ.0) THEN
	    DO I=1,NOFF/2
		OFFL(I) = OFFSETS(2*I-1)
		OFFS(I) = OFFSETS(2*I)
	    ENDDO
	ENDIF

	NLMIN = 1000000
	NLMAX = 0
	DO I = 1,NINP
	    NLMIN = MIN0(NLMIN,OFFL(I))
	    NLMAX = MAX0(NLMAX,OFFL(I)+NL(I)-1)
	ENDDO



C---- BEGIN LINE BY LINE PROCESSING.


	DO LINE = 1, NLOUT
	    DO I = 1, NSOUT
		OUTBUFFER(I) = 0
	    ENDDO

C		FILL TOP AND BOTTOM WITH ZEROES.
         IF (LINE.LT.NLMIN .OR. LINE.GT.NLMAX) GO TO 2300

C		DO MOSAIC
	    DO INP = NINP, 1, -1
		IF (LINE .LT. OFFL(INP) .OR.
     *			 LINE .GT. OFFL(INP)+NL(INP)-1) GO TO 2200
		CALL FINDEDGES (INP, LINE, FIRSTPIX,LASTPIX, NPIX, BUFFER)
		IF (NPIX .EQ. 0) GO TO 2200
		IX1 = FIRSTPIX(INP) + OFFS(INP) - 1
		IX2 = LASTPIX(INP) + OFFS(INP) - 1
		IF (IX2 .GE. 1 .AND. IX1 .LE. NSOUT) THEN
		    IX1 = MAX (IX1,1)
		    IX2 = MIN (IX2,NSOUT)
		    I = IX1 - (OFFS(INP) - 1)
		    DO IX = IX1, IX2
			OUTBUFFER(IX) = BUFFER(I)
			I = I + 1
		    ENDDO
		ENDIF
 2200		CONTINUE
	    ENDDO
 2300       CONTINUE
	    CALL XVWRIT (WUNIT,OUTBUFFER, STATUS, 'LINE',LINE,' ')
	ENDDO

	CALL XVCLOSE (WUNIT, STATUS,' ')

	RETURN
	END




	SUBROUTINE FINDEDGES (INP, LINE, FIRSTPIX,LASTPIX, NPIX, BUFFER)
C		FINDEDGES reads in a line of pixels from the INP'th 
C		image into BUFFER  and using the old FIRSTPIX and LASTPIX 
C		values finds the new first and last non-zero pixel in 
C		the line.  NPIX is set to zero if the whole line is blank.
	IMPLICIT NONE
	INTEGER	INP, LINE, NPIX
	INTEGER	FIRSTPIX(1),LASTPIX(1)
	BYTE	BUFFER(1)
	INTEGER	I, J, STATUS, NSAMP

	INTEGER MAXINP
	PARAMETER (MAXINP=40)
	INTEGER RUNIT(MAXINP), OFFL(MAXINP), OFFS(MAXINP), NS(MAXINP)
	LOGICAL LFLAG(MAXINP), RFLAG(MAXINP)
	COMMON /RAPIDCOM/ RUNIT, OFFL, OFFS, NS, LFLAG, RFLAG


	NSAMP = NS(INP)
	CALL XVREAD (RUNIT(INP), BUFFER,STATUS, 'LINE',
     +               LINE-OFFL(INP)+1,' ')

	IF (LFLAG(INP) .AND. RFLAG(INP)) GO TO 20

C		SET INITIAL LEFT AND RIGHT EDGES.
5	CONTINUE
	LFLAG(INP) = .FALSE.
	DO I = 1, NSAMP
	    IF (BUFFER(I) .NE. 0) THEN
		LFLAG(INP) = .TRUE.
		FIRSTPIX(INP) = I
		GOTO 10
	    ENDIF
	ENDDO
10	CONTINUE

	RFLAG(INP) = .FALSE.
	DO I = NSAMP, 1, -1
	    IF (BUFFER(I) .NE. 0) THEN
		RFLAG(INP) = .TRUE.
		LASTPIX(INP) = I
		GOTO 15
	    ENDIF
	ENDDO
15	CONTINUE
	GO TO 100



C		FIND LEFT EDGE

20	CONTINUE
	J = FIRSTPIX(INP)
	IF (BUFFER(J) .NE. 0) THEN
C			GO TO THE LEFT
	    DO I = J, 1, -1
		IF (BUFFER(I) .EQ. 0) THEN
		    FIRSTPIX(INP) = I+1
		    GO TO 60
		ENDIF
	    ENDDO
	    FIRSTPIX(INP) = 1
	    GO TO 60
	ELSE
C			GO TO THE RIGHT
	    DO I = J, NSAMP
		IF (BUFFER(I) .NE. 0) THEN
		    FIRSTPIX(INP) = I
		    GO TO 60
		ENDIF
	    ENDDO
	    LFLAG(INP) = .FALSE.
	    RFLAG(INP) = .FALSE.
	    GO TO 100
	ENDIF




C		FIND RIGHT EDGE

60	CONTINUE

	J = LASTPIX(INP)
	IF (BUFFER(J) .EQ. 0) THEN
C			GO TO THE LEFT
	    DO I = J, 1, -1
		IF (BUFFER(I) .NE. 0) THEN
		    LASTPIX(INP) = I
		    GO TO 100
		ENDIF
	    ENDDO
	    LFLAG(INP) = .FALSE.
	    RFLAG(INP) = .FALSE.
	    GO TO 100
	ELSE
C			GO TO THE RIGHT
	    DO I = J, NSAMP
		IF (BUFFER(I) .EQ. 0) THEN
		    LASTPIX(INP) = I-1
		    GO TO 100
		ENDIF
	    ENDDO
	    LASTPIX(INP) = NSAMP
	ENDIF


  100 CONTINUE
	NPIX = LASTPIX(INP) - FIRSTPIX(INP) + 1
	IF (.NOT. LFLAG(INP)) THEN
	    NPIX=0
	ELSE   ! If first and last got screwed up then scan the whole line
	    IF (NPIX .LE. 0) GO TO 5  
	ENDIF

	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rapidmos.imake
#define  PROGRAM   rapidmos

#define MODULE_LIST rapidmos.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create rapidmos.pdf
PROCESS    HELP=*
PARM INP      TYPE=(STRING,72) COUNT=(1:40)
PARM OUT      TYPE=(STRING,72)
PARM SIZE     TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM NL       TYPE=INTEGER DEFAULT=0
PARM NS       TYPE=INTEGER DEFAULT=0
PARM OFFSETS  TYPE=INTEGER COUNT=(1:80) DEFAULT=(1,1)
!# parm inp(3-40) hints=default
END-PROC
.TITLE
VICAR2 Program RAPIDMOS
.HELP
PURPOSE

    RAPIDMOS mosaics multiple images into one output image. 
The operation is similar to that of FASTMOS, but with reduced 
execution time and with some functions removed.  Up to 40
images may be mosaicked at one time.  Currently only byte
images are handled.  NOTE: the non-zero portion of the image
must be convex.


EXECUTION

     rapidmos INP=(A1,A2,...,An)  OUT=B   NL=NL  NS=NS  + 
		  OFFSETS=(LOFF1,SOFF1, LOFF2,SOFF2, ... , LOFFn,SOFFn)
	
     A1,A2,...,An       are the input images.
     B                  is an output image.
     NL			is the number of line in the output image
     NS			is the number of samples in the output image
     LOFF1,SOFF1...     line and sample in the output image where the
                        upper left corner of the input image will be
                        positioned.
OPERATION

     Mosaicking  is  performed by reading the lines  of  all 
     input images and combining them into the line of output 
     image.   If  the  input pixels overlap,  the  preceding 
     input image supersedes the successive.   Zero pixels of 
     the  input images are ignored if they lie  outside  the 
     non-zero perimeter; however, they are preserved if they 
     are  a  part  of a "zero-island"  within  the  non-zero 
     perimeter.  Pointers to the left and right edges of each
     image are kept and updated for each line.  For this reason
     concave images (which have more than two edges on a line)
     will not be mosaicked correctly.  It is the use of these
     pointers, however, that reduces the CPU time compared with
     FASTMOS.


EXAMPLE

     rapidmos INP=(A,B,C) OUT=D SIZE=(1,1,1000,1000) +
        	      OFF=(100,100, 100,500, -25,150)

     This example mosaics three images, positioning their left 
     upper   corners  into  pixels (100,100),  (100,500) and 
     (-25,150) of the output image respectively.


RESTRICTIONS

     1.  Only byte images are supported.
     2.  The non-zero portion of the images must be convex.
     3.  There may be up to 40 input images.
     4.  The maximum number of samples for input or output images is 50,000.

WRITTEN BY:                B. Gokhman

COGNIZANT PROGRAMMER:      K. F. Evans

REVISION:  2		April 1986

Ported to UNIX             C.R. Schenk (CRI) May 1995


.LEVEL1
.VARIABLE INP
Input images (up to 40)
.VARIABLE OUT
Output image
.VARIABLE SIZE
Size field for output picture
Only NL and NS are used.
.VARIABLE NL
Number of lines in the output
.VARIABLE NS
Number of samples in the output
.VARIABLE OFFSETS
Specifies positioning of inputs

.LEVEL2
.VARIABLE INP
    INP=(A1,...,An)      Input images (up to 40)
.VARIABLE OUT
    OUT=B                Output image
.VARIABLE SIZE
     SIZE=(1,1,NL,NS)	 Size field for output picture
			 Only NL and NS are used.
.VARIABLE NL
     NL=I		 Number of lines in the output
.VARIABLE NS
     NS=I		 Number of samples in the output
.VARIABLE OFFSETS
     OFF=(L1,S1, L2,S2, ... Ln,Sn)

                         Lk,Sk refer to positioning of the 
                         k'th  image in  the  output image.
			 Pixel (1,1) of the k'th input image 
			 is  placed  in the output image  at 
                         pixel (Lk,Jk).  L and/or S may be 
                         positive (yielding image  placement 
                         downward   and/or   to  the   right 
                         respectively),  or L and/or S may 
                         be zero or negative (yielding image 
                         placement upward and/or to the left 
                         respectively).  Default:  L=1, S=1.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstrapidmos.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
  gen A0  NL=50 NS=60 LINC=0 SINC=0 IVAL=110

!  FIRST TEST DEFAULT CASE, AMOUNTING TO A SIMPLE COPY.

  rapidmos A0 A1
!                    SHOULD GET 0 DIFFERENCES.
  difpic (A0,A1)

  gen B0  NL=55 NS=65 LINC=0 SINC=0 IVAL=120
  gen C0  NL=60 NS=65 LINC=0 SINC=0 IVAL=130
  gen D0  NL=60 NS=70 LINC=0 SINC=0 IVAL=140
  tieconm OUT=TIEP  NAH=10 NAV=10    'MGEOM  +
	  TIEPOINT=(40,20,1,1, 110,60,100,1, 20,110,1,100, 80,120,100,100)
  lgeom A0  A  PARMS=TIEP  SIZE=(1,1,100,100) 'NOIZ
  lgeom B0  B  PARMS=TIEP  SIZE=(1,1,100,100) 'NOIZ
  lgeom C0  C  PARMS=TIEP  SIZE=(1,1,100,100) 'NOIZ
  lgeom D0  D  PARMS=TIEP  SIZE=(1,1,100,100) 'NOIZ
  fastmos (A,B,C,D)  F   SIZE=(1,1,120,200) +
			OFF1=(-10,20) OFF2=(-30,60) OFF3=(20,30) OFF4=(10,70)
  rapidmos (A,B,C,D) R   SIZE=(1,1,120,200) +
			OFF=(-10,20, -30,60, 20,30, 10,70)
  f2 (F,R) D  FUNCTION="IN1-IN2+128"
  write "The following HIST should show the difference image to be all DN 128"
  hist D  'NOHIST
end-proc
$ Return
$!#############################################################################
