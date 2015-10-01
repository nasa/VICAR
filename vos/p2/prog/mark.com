$!****************************************************************************
$!
$! Build proc for MIPL module mark
$! VPACK Version 1.9, Tuesday, March 06, 2001, 11:52:03
$!
$! Execute by entering:		$ @mark
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
$ write sys$output "*** module mark ***"
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
$ write sys$output "Invalid argument given to mark.com file -- ", primary
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
$   if F$SEARCH("mark.imake") .nes. ""
$   then
$      vimake mark
$      purge mark.bld
$   else
$      if F$SEARCH("mark.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mark
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mark.bld "STD"
$   else
$      @mark.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mark.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mark.com -mixed -
	-s mark.f -
	-i mark.imake -
	-p mark.pdf -
	-t tstmark.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mark.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C VICAR program MARK
C
      SUBROUTINE MAIN44
      IMPLICIT NONE
      COMMON/C1/LOCS(2,5000),PIX(2048,32)
      REAL*4 LOCS
      INTEGER*2 PIX		!10 consecutive image lines
      INTEGER NBUF
      PARAMETER (NBUF=32)	!# of image lines in PIX (should be even)
      
      INTEGER NLI,NSI		!Input picture is NLI X NSI
      INTEGER NLO,NSO		!Output picture is NLO X NSO
      INTEGER SLI,SSI		!Starting line and sample of input image
      INTEGER ELI
      INTEGER MIDDN,MAXDN,DN,ISUM,MEAN
      INTEGER NLOCS,N,N1,N2,NB,I,K,JX,KX
      INTEGER OUNI,IMG,STAT,CNT
      INTEGER LINE,L,S
      INTEGER SL,SS,EL,ES	!Boundaries of rectangular area
      REAL*4 SKIP
      CHARACTER*4 FMT

      CALL IFMESSAGE('MARK version March 5, 2001')
      CALL GET_LOCS(PIX,locs,nlocs)

      CALL XVUNIT(IMG,'INP',1,STAT,' ')
      CALL XVOPEN(IMG,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     *		'U_FORMAT','HALF',' ')
      CALL XVGET(IMG,STAT,'FORMAT',FMT,' ')
      IF (FMT.EQ.'BYTE') THEN
         MAXDN = 255
      ELSEIF (FMT.EQ.'HALF') THEN
         MAXDN = 511
      ELSE
         CALL MABEND('***Invalid data format for input image')
      ENDIF
      CALL XVP('MAXDN',N,CNT)
      IF (CNT.EQ.1) MAXDN=N
      MIDDN = (MAXDN+1)/2

      CALL XVSIZE(SLI,SSI,NLO,NSO,NLI,NSI)
      NLO = MIN0(NLI-SLI+1,NLO)
      NSO = MIN0(NSI-SSI+1,NSO)
      ELI = SLI + NLO - 1

C     ...If (SLI,SSI) is not (1,1), redefine origin of coordinates
      IF (SLI.GT.1) THEN
         SKIP = FLOAT(SLI-1)
         DO I=1,NLOCS
            LOCS(1,I)=LOCS(1,I)-SKIP
         ENDDO
      ENDIF
      IF (SSI.GT.1) THEN
         SKIP = FLOAT(SSI-1)
         DO I=1,NLOCS
            LOCS(2,I)=LOCS(2,I)-SKIP
         ENDDO
      ENDIF

      CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     *		'OP','WRITE','U_NL',NLO,'U_NS',NSO,' ')

      CALL XVP('BOXSIZE',N,CNT)
      N1 = N/2
      N2 = (N+1)/2
      NB = NBUF/2

      KX = 0	              !Image buffer index

      DO 100 LINE=SLI,ELI+9
      KX = (MOD(KX,NBUF)) + 1
      IF (LINE.LE.NLO) THEN
         CALL XVREAD(IMG,PIX(1,KX),STAT,'LINE',LINE,'SAMP',SSI,
     *		'NSAMPS',NSO,' ')
         IF (LINE.EQ.SLI) THEN
            DO JX=2,10
               CALL MVE(2,NSO,PIX,PIX(1,JX),1,1)
            ENDDO
         ENDIF
      ENDIF

      DO 90 I=1,NLOCS
      L = LOCS(1,I) - .25
      IF (LINE-SLI+1.NE.L+NB) GOTO 90  
      IF (LOCS(1,I)-L.LT..75) THEN	!Here if entire rectangle is in memory
         SL = NB - N1		!Compute bounding coordinates of
         EL = NB + N1 + 1	!rectangle:  (SL,SS) to (EL,ES)
      ELSE
         SL = NB - N2 + 1
         EL = NB + N2 + 1
      ENDIF
      S = LOCS(2,I) - .25
      IF (LOCS(2,I)-S.LT..75) THEN
         SS = S - N1
         ES = S + N1 + 1
      ELSE
         SS = S - N2 + 1
         ES = S + N2 + 1
      ENDIF

      ISUM = 0			!Compute sum of DNs on perimeter of rectangle
      DO 50 L=SL,EL
      JX = MOD(KX+L-1,NBUF) + 1
      DO 50 S=SS,ES
      IF (L.NE.SL .AND. L.NE.EL .AND. S.NE.SS .AND. S.NE.ES) GOTO 50
      ISUM = ISUM + PIX(S,JX)
   50 CONTINUE

      K  = 2*(EL-SL+ES-SS)	!Compute # of pixels on perimeter
      MEAN = ISUM/K		!Average DN in area
      DN = 0
      IF (MEAN.LT.MIDDN) DN=MAXDN

      DO 60 L=SL,EL
      JX = MOD(KX+L-1,NBUF) + 1
      DO 60 S=SS,ES
      IF (L.NE.SL .AND. L.NE.EL .AND. S.NE.SS .AND.S.NE.ES) GOTO 60
      PIX(S,JX) = DN
   60 CONTINUE
   90 CONTINUE

      IF (LINE.LT.SLI+NBUF-1) GOTO 100
      JX = MOD(KX,NBUF) + 1
      CALL XVWRIT(OUNI,PIX(1,JX),STAT,' ')
  100 CONTINUE

      CALL XVCLOSE(OUNI,STAT,' ')
      CALL XVCLOSE(IMG,STAT,' ')
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get the pixel locations from paramters or secondary input file.
C
      SUBROUTINE GET_LOCS(BUF,LOCS,NLOCS)
      IMPLICIT NONE
      REAL*4 BUF(2,5000)	!Temporary buffer
      REAL*4 LOCS(2,1)		!Output coordinates SL,s1,EL,s2,...
      INTEGER NLOCS		!number of coordinate pairs

      INTEGER*4 IUNIT,NI,NL,NS,NSAMPS,I,L,N,INC,ISTART,STAT
      LOGICAL XVPTST

      CALL XVP('DATA',LOCS,NS)
      NLOCS = NS/2
      IF (2*NLOCS.NE.NS) CALL MABEND('***Pixel locations not in pairs')

      CALL XVPCNT('INP',NI)	!Get number of input files
      IF (NI.NE.2) RETURN

C     ...If EVEN is specified, start with the second pixel location
      ISTART = 1
      IF (XVPTST('EVENONLY')) ISTART=2
C     ....If ODD or EVEN is specified, skip every other pixel location
      INC = 1
      IF (XVPTST('ODD_ONLY').OR. XVPTST('EVENONLY')) INC=2
  
      CALL XVUNIT(IUNIT,'INP',2,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVGET(IUNIT,STAT,'NL',NL,'NS',NS,' ')

      NSAMPS = NS/2
      NLOCS = NL*NSAMPS/INC
      IF (NLOCS.GT.5000) CALL MABEND('***Pixel locations exceed 5000')
      N = 0

      DO L=0,NL-1
        CALL XVREAD(IUNIT,BUF(1,L*NSAMPS+1),STAT,'NSAMPS',NS,' ')
        DO I=ISTART,NSAMPS,INC
           N = N + 1
           LOCS(1,N) = BUF(1,I)
           LOCS(2,N) = BUF(2,I)
        ENDDO
      ENDDO
      CALL XVCLOSE(IUNIT,STAT,' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mark.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM mark

   To Create the build file give the command:

		$ vimake mark			(VMS)
   or
		% vimake mark			(Unix)

************************************************************************/

#define PROGRAM	mark
#define R2LIB
#define MODULE_LIST mark.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create mark.pdf
process help=*
PARM INP  TYPE=STRING COUNT=(1:2)
PARM OUT  TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL   TYPE=INTEGER COUNT=(0:1) VALID=1:2048 DEFAULT=1
PARM SS   TYPE=INTEGER COUNT=(0:1) VALID=1:2048 DEFAULT=1
PARM NL   TYPE=INTEGER COUNT=(0:1) VALID=0:999999 DEFAULT=0
PARM NS   TYPE=INTEGER COUNT=(0:1) VALID=0:2048   DEFAULT=0
PARM BOXSIZE TYPE=INTEGER COUNT=(0:1) VALID=(1:30) DEFAULT=8
PARM DATA TYPE=REAL COUNT=(0:500) DEFAULT=--
PARM RESTRICT KEYWORD COUNT=(0:1) VALID=(ODD_ONLY,EVENONLY) default=--
PARM MAXDN TYPE=INTEGER COUNT=(0:1) DEFAULT=0
END-PROC

.TITLE
   VICAR PROGRAM MARK
.HELP
PURPOSE:

   MARK is a VICAR applications program used to scribe rectangles
   about specified pixel locations.

EXECUTION:

        MARK  INP=(PIC,LOC) OUT=OPIC
  or
        MARK  INP=PIC  OUT=OPIC  DATA=(l1,s1,l2,s2,l3,s3,...)

  where PIC is the input image (byte or halfword),
        OPIC is the output image (same data format as PIC).

  The pixel locations to be enclosed inside rectangles are specified either
  by inputing them via a secondary input file (LOC) or via user parameters.
  See parameters INP or DATA for details.

.page
OPERATION:

 MAXDN=n specifies the DN value of the scribed rectangle.
 The DN value of the scribed rectangle will be 0 or MAXDN, depending upon
 whether the mean value of the pixels on the perimeter are greater than or
 less than MAXDN/2, respectively.  Default MAXDN=255 for byte data and
 511 for halfword data.

 Rectangles are scribed in such a way that center locations can be
 specified to one-half pixel. The program does this by selecting
 either an even or an odd dimension for the rectiangle depending on
 where the coordinate falls with respect to the pixel mesh. This is
 done independently in the line and sample direction.
   
 The location of the inscribed rectangle boundaries depends on the
 fractional part of the location of the center and the BOXSIZE 
 parameter specified. If the center has been specified by line-sample
 coordinates (L,S) and the BOXSIZE as N.  Then the actual interior size
 and the center of the box, excluding the scribed lines will be:
   
         FRACTIONAL PART=F        SIZE=N         CENTERS
                                 ODD  EVEN    LINE    SAMPLE
        0.0 .LE. F .LT. .25      N    N-1      L        S   
        .25 .LE. F .LT. .25      N-1  N        L+1/2    S+1/2
        .75 .LE. F .LT. 1.0      N    N-1      L+1      S+1

 So if the BOXSIZE=8, the interior region would be 7 lines by
 8 samples if the fractional part of the real numbers specifing
 the line and sample coordinates are less then .25 and between
 .25 and .75, respectively. For example, BOXSIZE=8, L=35.1 and
 S=16.3 results in a box of 7 lines by 8 samples with the upper
 left corner located at line 30 and sample 11.

.page
PROGRAM HISTORY:

Written By: Sayuri Harami, September 13, 1971
Cognizant Programmer: Gary Yagi
REVISIONS: 
    5 MAR  2001  GMY  Increased max BOXSIZE to 30.  Fixed SIZE field.
		      Added EVENONLY and ODD_ONLY keywords
		      Added MAXDN parameter.
   31 OCT  1994...A.SCOP..(CRI)..Made portable for UNIX
   29 MAR  1985...M.E.MORRILL....REAL MARK FORMAT FOR RESEAUS
   24 OCT  1984...M.E.MORRILL....CONVERSION TO VAX-VICAR*2
   27 JUNE 1975...D.A.HASS.......CONVERCION TO IBM 360/OS
   26 JULY 1973...K.R.N..........VMIO ALT BUFFERING
   15 SEPT 1971...J.E.KREZNAR....UPDATE
   13 SEPT 1971...S.X.HARAMI.....INITIAL RELEASE  

.LEVEL1
.VARIABLE INP
 (1) An image file-REQUIRED
 (2) A coordinate file-OPTIONAL
     FORMAT=REAL
.VARIABLE OUT
 The image file with
 marked locations.
.VARIABLE SL
 INTEGER-OPTIONAL
 Starting line.
.VARIABLE SS
 INTEGER-OPTIONAL
 Starting sample.
.VARIABLE NL
 INTEGER-OPTIONAL
 Number of lines.
.VARIABLE NS
 INTEGER-OPTIONAL
 Number of samples.
.VARIABLE BOXSIZE
 INTEGER-OPTIONAL
 Size of scribed boxes.
.VARIABLE DATA
 REAL-OPTIONAL
 Centers of boxes
 to be marked.
.VAR RESTRICT 
KEYWORD-OPTIONAL
Restrict marks to odd or
even tiepoints only
.VARI MAXDN
INTEGER-OPTIONAL
DN value of scribed rectangle

.LEVEL2
.VARIABLE INP

     INP=PIC
 or  INP=(PIC,LOC)

 The primary input image (PIC) will be reproduced as the output image
 with scribed rectangles.  The maximum image width is 2048 samples.

 The optional secondary input (LOC) is used to specify the pixel locations to
 be scribed inside rectangles.  Each record of LOC should contain a list of
 line-sample coordinates (L,S) in REAL data format (single precision floating
 point) as follows:

   L1,S1,L2,S2,L3,S3,...

 A maximum of 900 pixel locations may be input.  

 In certain applications (e.g. TRACKER3) the matching pixel locations from
 pairs of images (tiepoints) may be interleaved in LOC:

    L1,S1,U1,V1,L2,S2,U2,V2,L3,S3,U3,V3,....

 If the keyword 'ODD_ONLY is specified, then pixel coordinates (L1,S1), (L2,S2),
 (L3,S3),... are used.

 If the keyword 'EVENONLY is specified, then pixel coordinates (U1,V1), (U2,V2),
 (U3,V3),... are used.

.VARIABLE OUT
 The output image will contain the input with scribed rectangles.
.VARIABLE SL
 INTEGER-OPTIONAL
 Starting line.
.VARIABLE SS
 INTEGER-OPTIONAL
 Starting sample.
.VARIABLE NL
 INTEGER-OPTIONAL
 Number of lines.
.VARIABLE NS
 INTEGER-OPTIONAL
 Number of samples.
.VARIABLE BOXSIZE
 TYPE=INTEGER

 The interior dimensions of the rectangles.
 The exact dimensions depend on the fractional location of the 
 rectangle center and wither the box is an odd or even number of
 pixels:
         FRACTIONAL PART=F        SIZE=N         CENTERS
                                 ODD  EVEN    LINE    SAMPLE
        0.0 .LE. F .LT. .25      N    N-1      L        S   
        .25 .LE. F .LT. .25      N-1  N        L+1/2    S+1/2
        .75 .LE. F .LT. 1.0      N    N-1      L+1      S+1

 The maximum boxsize is 30 x 30 samples. Default is BOXSIZE=8.

.VARIABLE DATA

	DATA=(l1,s2,l2,s2,l3,s3,...)

 The center locations of rectangles to be scribed specified as (line,samp)
 pairs of floating point numbers.   A maximum of 500 locations can be input.

.VARI MAXDN

	MAXDN = 3000

 The DN value of the scribed rectangle will be 0 or MAXDN, depending upon
 whether the mean value of the pixels on the perimeter are greater than or
 less than MAXDN/2, respectively.  Default MAXDN=255 for byte data and
 511 for halfword data.

 Note the MAXDN parameter is useful primarily for halfword data since the
 default is only appropriate for old Mariner missions which returned 9 bit data.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmark.pdf
procedure	!Test of VICAR program MARK
refgbl $echo
refgbl $syschar
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
local path type=string init="wms_test_work:[testdata.mipl.vgr]"
if ($syschar(1)="UNIX") let path="/project/test_work/testdata/mipl/vgr/"

mark (&"path"raw.sn4,&"path"res.sn4) out boxsize=3
list &"path"res.sn4 (1,1,1,4)	!Print pixel coordinates of box centers
list out (21,8,7,7)		!Print the 1st box
list out (12,54,6,7)		!Print the 2nd box

!Check ODD_ONLY and EVENONLY keywords
mark (&"path"raw.sn4,&"path"res.sn4) out boxsize=3 'odd
list out (21,8,7,7)		!Print the box (odd)
list out (12,54,6,7)		!No box (even is skipped)

mark (&"path"raw.sn4,&"path"res.sn4) out boxsize=3 'even
list out (21,8,7,7)		!No bos (odd is skipped)
list out (12,54,6,7)		!Print the box (even)

!Check size field
mark (&"path"raw.sn4,&"path"res.sn4) out sl=3 ss=3 boxsize=3
list out (19,6,7,7)		!Image is moved and so has box

!Check MAXDN
mark (&"path"raw.sn4,&"path"res.sn4) out boxsize=3 maxdn=128
list out (21,8,7,7)		!Box is now 128 DN
end-proc
$ Return
$!#############################################################################
