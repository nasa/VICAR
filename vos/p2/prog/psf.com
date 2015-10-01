$!****************************************************************************
$!
$! Build proc for MIPL module psf
$! VPACK Version 1.7, Wednesday, November 02, 1994, 15:28:27
$!
$! Execute by entering:		$ @psf
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
$ write sys$output "*** module psf ***"
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
$ write sys$output "Invalid argument given to psf.com file -- ", primary
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
$   if F$SEARCH("psf.imake") .nes. ""
$   then
$      vimake psf
$      purge psf.bld
$   else
$      if F$SEARCH("psf.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake psf
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @psf.bld "STD"
$   else
$      @psf.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create psf.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack psf.com -
	-s psf.f -
	-i psf.imake -
	-p psf.pdf -
	-t tstpsf.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create psf.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C  95-1-2  ...AS....  (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C  85-3-28 ...LWK...  CONVERTED TO VAX VICAR2, ADDED STACKA CALL
C                      & 'SHIFT' KEYWORD
      SUBROUTINE MAIN44
      IMPLICIT INTEGER (A-Z)
      INTEGER*4 AREA(4)
      EXTERNAL WORK

      CALL IFMESSAGE('PSF version 2-JAN-95')
      CALL XVEACTION('SA',' ')

      CALL XVUNIT( IUN, 'INP', 1, ISTAT,' ')
      CALL XVOPEN( IUN, ISTAT, 'U_FORMAT', 'HALF', ' ')
      CALL XVSIZE( SL, SS, NLO, NSO, NL, NS)

      CALL XVPARM( 'AREA', AREA, CNT, DEF, 4)	! (NO DEFAULT ALLOWED)
      IF (AREA(3).EQ.(AREA(3)/2)*2) THEN
	AREA(3)=AREA(3)+1
	CALL XVMESSAGE('NL OF AREA INCREASED BY 1',' ')
      ENDIF
      IF (AREA(4).EQ.(AREA(4)/2)*2) THEN
	AREA(4)=AREA(4)+1
	CALL XVMESSAGE('NS OF AREA INCREASED BY 1',' ')
      ENDIF

      NB1 = 2*MAX0( NS, NSO)		! 'IN' BUFFER SIZE (IN BYTES)
      NB2 = 2*AREA(4)			! PSF BUFFER SIZE
      CALL STACKA( 10,WORK, 2, NB1, NB2, NL, NS, NLO, NSO, AREA, IUN)
      RETURN
      END

C*****************************************************************
      SUBROUTINE WORK( IN, NB1, PSF, NB2, NL, NS, NLO, NSO, AREA, IUN)
      IMPLICIT INTEGER (A-Z)
      INTEGER*2 IN( NB1/2), PSF(NB2/2)
      INTEGER*4 AREA(4)
      CHARACTER*8 FMT
      REAL*4 XDN, CENX, CENY
      LOGICAL XVPTST

      CALL XVGET( IUN, STAT, 'FORMAT', FMT,' ')
      IF (FMT.NE.'BYTE' .AND. FMT.NE.'HALF' .AND. FMT.NE.'WORD') THEN
	CALL MABEND('** ONLY BYTE & HALFWORD FORMATS SUPPORTED **')
      ENDIF
      IF (FMT.EQ.'BYTE') THEN
	IH = 2			! ** FOR VICAR2 BUG IN 'NSAMPS'
	LIMIT = 255
      ELSE
	IH = 1
	LIMIT = 32767
      ENDIF

      LT = AREA(1)
      LB = AREA(3)+LT-1
      IL = AREA(2)
      IR = AREA(4)+IL-1
      IF (LB.GT.NL) LB=NL
      IF (IR.GT.NS) IR=NS
      IF (IL.GT.NS .OR. LT.GT.NL) THEN
	CALL MABEND('** AREA IS OUTSIDE INPUT IMAGE **')
      ENDIF
      NLIN=LB-LT+1
      NPIX=IR-IL+1
      NLIN2=NLIN/2
      NLINL=NLIN-NLIN2
      NSA2=NPIX/2
      NSAL=NPIX-NSA2

      CALL XVUNIT( OUN, 'OUT', 1, STATUS,' ')
      CALL XVOPEN( OUN, STATUS, 'OP', 'WRITE', 'U_NL', NLO, 'U_NS',
     .	NSO, 'U_FORMAT', 'HALF', ' ')

C  READ BORDER POINTS TO GET MAX DN
C
      N=0
      MEAN=0
      CALL XVREAD( IUN, IN, STATUS, 'LINE', LT,' ')
      DO L=LT,LB
	IF (L.EQ.LT .OR. L.EQ.LB) THEN
	  DO J=IL,IR
	    N=N+1
	    MEAN=MEAN+IN(J)
	  ENDDO
	ELSE
	  N=N+2
	  MEAN=MEAN+IN(IL)+IN(IR)
	ENDIF
	IF (L.LT.LB) CALL XVREAD( IUN, IN, STATUS,' ')
      ENDDO
      MEAN=NINT(FLOAT(MEAN)/FLOAT(N))
      CALL PRNT(4,1,MEAN,'BACKGROUND DN =')
C
C  DETERMINE CENTROID
C
      XDN = 0.0
      CENX = 0.0
      CENY = 0.0
      CALL XVREAD( IUN, IN, STATUS, 'LINE', LT,' ')
      DO L=LT,LB
	DO J=IL,IR
	  N = IN(J)-MEAN
	  IF (N.LT.0) N=0
	  N = N*N
	  XDN = XDN+N
	  CENX = CENX+FLOAT(J-IL)*N
	  CENY = CENY+FLOAT(L-LT)*N
	ENDDO
	IF (L.LT.LB) CALL XVREAD( IUN, IN, STATUS,' ')
      ENDDO
      ICENX = CENX/XDN+IL+.5
      ICENY = CENY/XDN+LT+.5
      CALL PRNT(4,1,ICENX,'X CENTROID =')
      CALL PRNT(4,1,ICENY,'Y CENTROID =')
C
C  SET UP AREA TO CONTAIN PSF
C IF 'SHIFT' WAS SPECIFIED, RETAIN ORIGINAL SIZE, MOVE CENTER.
C ELSE RETAIN ORIGINAL AREA & QUARTER THE PSF UNEQUALLY.
C **> (MAY BE BETTER TO REDUCE SIZE & QUARTER SYMMETRICALLY!)
      IF (XVPTST( 'SHIFT')) THEN
	LB=ICENY+NLIN2		! BOTTOM LINE OF PSF
	LT=ICENY-NLIN2		! TOP LINE OF PSF
	IR=ICENX+NSA2		! RIGHT EDGE OF PSF
	IL=ICENX-NSA2		! LEFT EDGE OF PSF
	IF (LB.GT.NL) LB=NL
	IF (IR.GT.NS) IR=NS
	IF (LT.LT.1) LT=1
	IF (IL.LT.1) IL=1
	NLIN=LB-LT+1		! NL OF PSF
	NPIX=IR-IL+1		! NS OF PSF
	NSA2=NPIX/2
	NLIN2=NLIN/2
      ELSE
	NLIN2 = ICENY-LT+1
	NSA2 = ICENX-IL+1
      ENDIF
      NLINL=NLIN-NLIN2
      NSAL=NPIX-NSA2
C
C  COPY DATA TO OUTPUT AND SUBTRACT BACKGROUND
C
      N=0
      DO OLIN = 1, NLO			! OUTPUT LINE NUMBER
	CALL MVE(2,NSO,0,IN,0,1)	! ZERO THE OUTPUT BUFFER
	ILIN = 0			! ZERO THE INPUT LINE NUMBER
	IF (OLIN.LE.NLINL)
     .	 ILIN = OLIN+LT-1+NLIN2  	! BOTTOM OF PSF
	IF (OLIN.GE.NLO-NLIN2+1)
     .	 ILIN = OLIN-NLO+NLIN2+LT-1	! TOP OF PSF
	IF (ILIN.NE.0) THEN
	  CALL XVREAD( IUN, PSF, STATUS, 'LINE', ILIN, 'SAMP', IL,
     .	   'NSAMPS', IH*NPIX,' ')		! READ THE PSF AREA
	  CALL SUBV(-6,NPIX,MEAN,PSF,0,1) ! SUBTRACT BACKGROUND
	  CALL TRUNC(PSF,0,LIMIT,NPIX)
	  CALL MVE(2,NSA2,PSF,IN(NSO-NSA2+1),1,1) ! LEFT SIDE OF PSF
	  CALL MVE(2,NSAL,PSF(NSA2+1),IN,1,1)	! RIGHT SIDE OF PSF
	ENDIF

	CALL XVWRIT( OUN, IN, STAT,' ')
      ENDDO

      RETURN
      END


      SUBROUTINE TRUNC( BUF, LO, HI, N)
C  TRUNCATE N BUF VALUES TO RANGE (LO, HI).
      IMPLICIT INTEGER*4 (A-Z)
      INTEGER*2 BUF(1)

      DO I=1,N
	IF (BUF(I).LT.LO) BUF(I) = LO
	IF (BUF(I).GT.HI) BUF(I) = HI
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create psf.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM psf

   To Create the build file give the command:

		$ vimake psf			(VMS)
   or
		% vimake psf			(Unix)


************************************************************************/


#define PROGRAM	psf
#define R2LIB

#define MODULE_LIST psf.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create psf.pdf
process help=*
PARM INP (STRING)
PARM OUT (STRING) 
PARM SIZE INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL INTEGER DEFAULT=1
PARM SS INTEGER DEFAULT=1
PARM NL INTEGER DEFAULT=0
PARM NS INTEGER DEFAULT=0
PARM AREA INTEGER COUNT=4 
PARM SHIFT KEYWORD VALID=(SHIFT,NOSHIFT) DEFAULT=SHIFT
END-PROC
.TITLE
VICAR1 Program "psf"
.HELP
This program copies a point spread function (PSF) contained in the
input image to an output image in a format suitable for the production
of the optical transfer function (OTF) by program "fft22".  The
program was designed to be used in the procedure "restore", which deconvolves
the point spread function from an image, but can also be used in stand-
alone mode.

HISTORY:

Written by:  J.J.Lorre,  1 Jan. 1978
Converted to VAX by:  L.W.Kamp,  29 Mar. 1985
Current Cognizant Programmer:  L.W.Kamp
Revisions:
Made portable for UNIX A.Scop (CRI) 2 Jan. 1995
.page
EXECUTION FORMAT:

  psf  IN  OUT  SIZE  AREA  SHIFT

where:	IN	is the input image.
	OUT	is the output point spread function.
	SIZE	is the size of the output file, and should be
		  the same dimension as the image to which the
		  OTF will be applied.
	AREA	is a required parameter defining the location of
		  the point spread function in the input image.
	SHIFT   is an optional parameter determining whether the
		  PSF area will be allowed to shift in the input.
.page
METHOD

First, the program determines the mean of the DNs lying on the border
of the region defined by the AREA parameter.  This mean is used as the
background DN value.  The subimage of size determined by AREA minus the
background value is the PSF.

Next, the program determines the centroid of the PSF.  The subimage is
quartered about the pixel closest to the centroid and is copied into
the four corners of the output data set.  (This process is intended to
eliminate translation of an image when deconvolved with the point spread
function, although subpixel translations will still be present.)  The
rest of the output file is left empty (0 DN).  
.LEVEL1
.vari INP
Input image
.vari OUT
Output PSF
.vari SIZE
Size of output file.
 = (SL,SS,NL,NS)
.vari SL
Starting line
(always 1)
.VARI SS
Starting sample
(always 1)
.VARIABLE NL
Number of Lines
.VARIABLE NS
Number of samples
.vari AREA
Area containing PSF.
.VARI SHIFT
Shift PSF about new
centroid?
.LEVEL2
.vari INP
INP is the name of the image file from which the PSF will be extracted,
using the AREA parameter.
.vari OUT
OUT is the name of the output file into which the properly formatted PSF
will be written.
.vari SIZE
SIZE is the standard Vicar2 parameter containing:
 
  (Starting Line, Starting Sample, Number of Lines, Number of Samples)
 
Note that in this program it refers only to the output file, and therefore
the first two elements are ignored.  This parameter is included only for
unformity in Vicar2 application program interface.
.vari SL
See HELP SIZE.
.vari SS
See HELP SIZE.
.VARIABLE NL
NL specifies the size of the image in the line direction, i.e., the
number of lines in the image.
.VARIABLE NS
NS specifies the size of the image in the sample direction, i.e.,
the number of pixels per line in the image.
.vari AREA
AREA specifies the area in the input file (INP) from which the PSF will be
extracted, after subtraction of the background DN value.  It has the
same structure as the SIZE parameter, i.e.:
 
  (Starting Line, Starting Sample, Number of Lines, Number of Samples).
 
Note that the last two elements should be less than or equal to the
corresponding elements in SIZE (or NL and NS) for meaningful results.

If NOSHIFT is specified, then the final PSF will remain strictly inside
this area after determination of the centroid.  Otherwise, the final
PSF will be of the same size as specified by AREA, but centered on the
centroid of the function.
.vari SHIFT
This parameter controls whether the PSF will be allowed to shift outside
the area specified by the AREA parameter after determination of its
centroid.

If SHIFT is specified, then the final PSF will be of the size specified 
by the AREA parameter, but centered on the centroid of the function. (If
part falls outside the image, the size is reduced accordingly.)  This is
the normal choice when the user does know the exact location of the PSF
in the input.

If NOSHIFT is specified, then the area of the PSF remains that specified
by AREA, but the quartering is done about the centroid, so that the four
quarters may be unequal.  This choice is useful if the location of the
PSF in the input is accurately known and it is important that data
outside this area be excluded.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpsf.pdf
procedure
! TO RUN ON VMS, TYPE   TSTPSF
! TO RUN ON UNIX OR AXP, MOVE THE TEST FILE TO THE MACHINE FROM THE VAX
! IF NOT AVAILABLE ON THAT MACHINE, AND TYPE
! tstpsf DIR=dirname
! where dirname = pathname of directory containing file with trailing / OR
!               = "" if in current directory.
refgbl $echo
refgbl $autousage
PARM DIR TYPE=STRING DEFAULT="MIPL:[MIPL.VGR]"
LOCAL INPIC TYPE=STRING
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
let INPIC= "&DIR"//"star.img"
write "This is a test file for program PSF"
psf inp=&INPIC out=PSF size=(1,1,50,50) AREA=(10,10,30,30) SHIFT=SHIFT
list PSF (1,1,20,20)
end-proc
$ Return
$!#############################################################################
