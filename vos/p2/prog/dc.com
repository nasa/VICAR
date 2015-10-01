$!****************************************************************************
$!
$! Build proc for MIPL module dc
$! VPACK Version 1.7, Monday, May 23, 1994, 11:29:39
$!
$! Execute by entering:		$ @dc
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
$ write sys$output "*** module dc ***"
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
$ write sys$output "Invalid argument given to dc.com file -- ", primary
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
$   if F$SEARCH("dc.imake") .nes. ""
$   then
$      vimake dc
$      purge dc.bld
$   else
$      if F$SEARCH("dc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dc.bld "STD"
$   else
$      @dc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dc.com -
	-s dc.f -
	-i dc.imake -
	-p dc.pdf -
	-t tstdc.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create dc.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C    5 SEP 94    ...AS...    (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C         *******    SUBROUTINE DC   *******
C   10 OCT 88    ...SP...    CHANGED TO IGNORE FORMAT PARAMETER BECAUSE VICAR2 
C                            USES ONLY THE FORMAT IN LABEL.
C   11 MAY 84    ...HBD...   CONVERT FOR VAX and to vicar2 format
C   22 OCT 79    ...JAM...   INITIAL RELEASE
C
      INCLUDE 'VICMAIN_FOR'
C
      SUBROUTINE MAIN44
      IMPLICIT NONE
C
C          LOCAL VARIABLES
C  STATUS -- OPEN AND CLOSE STATUS INDICATOR
C  INUNIT -- CONTAINS UNIT NUMBERS FOR INPUT DATASETS
C  NOLINE -- CONTAINS THE NUMBER OF LINES FOR EACH INPUT DATASET
C  NOSAMP -- CONTAINS THE NUMBER OF SAMPLES FOR EACH INPUT DATASET
C  REFIN  -- STARTING BYTE OF EACH PIX IN INPUT BUFFER (INBUF in DCB)
C
      INTEGER NOLINE(10), NOSAMP(10), REFIN(10), INUNIT(10)
      INTEGER STATUS, NUMIPT, NUMOPT, I, INDEX
      CHARACTER*10 LBUF(10)
C
      CHARACTER*25 MSG1

      COMMON /C1/ NOSAMP,NOLINE,REFIN,NUMIPT,NUMOPT,INUNIT

      CALL IFMESSAGE('DC version 5-SEP-94')
      CALL XVEACTION('SA',' ')
C--- Zero out buffers
      MSG1(1:25)='NUMBER OF INPUTS NOT GT 1'
      DO I = 1, 10
         REFIN(I) = 0
         NOLINE(I) = 0
         NOSAMP(I) = 0
      END DO
      CALL XVP('INP',LBUF,NUMIPT)
      CALL XVP('OUT',LBUF,NUMOPT)
      IF (NUMIPT .LE. 1) THEN
         CALL XVMESSAGE(MSG1,' ')
         CALL ABEND
      ENDIF
      INDEX = 1
C
C--- Open input datasets
      DO I = 1,NUMIPT
         REFIN(I) = INDEX
         CALL XVUNIT(INUNIT(I),'INP',I,STATUS,' ')
	 CALL XVOPEN(INUNIT(I),STATUS,'U_FORMAT','HALF',' ')
	 CALL XVGET(INUNIT(I),STATUS,'NL',NOLINE(I),'NS',NOSAMP(I),' ')
         INDEX = NOSAMP(I)+INDEX
      END DO

      CALL DCB

      RETURN
      END


      SUBROUTINE DCB
C
      IMPLICIT NONE
C
      INTEGER*2 INBUF(10000)
      INTEGER*2 OBUF1(50000),OBUF2(50000),OBUF3(50000),OBUF4(50000)
      INTEGER NOLINE(10),NOSAMP(10),REFIN(10),NUMIPT,NUMOPT,INUNIT(10)
      INTEGER OUTUNIT(4),CNT,DEF, STATUS, I, J, LINE, IPIXSIZ
      INTEGER SLO,SSO,NLO,NSO,MAXDN,OSAMP,ISAMP,DN,NLI,NSI
      INTEGER INT,IREC,ISIGN,ISLOPE,XPNTST
      REAL*4 EXPO(10),ACONST,CONST,DELTA,DENOM,DENOM2,SLOPE,SCALE
      REAL*4 X,X2,XY,Y,RMAXDN,RINT,YPOINT,PDN,RDN,TDELTA, XPOINT
      CHARACTER*4 FORMAT(2)
      LOGICAL ISWTCH, NEGA
      LOGICAL XVPTST

      COMMON /C1/ NOSAMP,NOLINE,REFIN,NUMIPT,NUMOPT,INUNIT
      DATA FORMAT/'WORD','BYTE'/
C
C--- Get SIZE parameters
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      IREC = SLO

C--- Test for halfword and negative DC picture
      CALL XVGET(INUNIT(1), STATUS, 'PIX_SIZE', IPIXSIZ,' ')  !BYTES PER PIXEL.
      IF ( IPIXSIZ .NE. 1 .AND. IPIXSIZ .NE. 2 )
     .     CALL MABEND('ERROR:INP. IMAGE IS NOT BYTE OR HALF FORMAT')

      ISWTCH = IPIXSIZ .EQ. 2
      ISIGN = 1
      NEGA = XVPTST('NEGATIVE')
      IF (NEGA) ISIGN = -1

C--- Determine MAXDN value
      CALL XVPARM('MAXDN',MAXDN,CNT,DEF,1)
      IF (ISWTCH .AND. DEF .NE. 0) MAXDN = 32767
      RMAXDN = MAXDN
      CONST = ISIGN / 2.0

C--- Get EXPOSURE times
      CALL XVP('EXPOSURE',EXPO,CNT)
      IF (CNT .EQ. 0) THEN
	 CALL XVMESSAGE('DC - Exposure values required.',' ')
	 CALL ABEND
      END IF
      DO I = 1, NUMIPT
	 X = X + EXPO(I)
	 X2 = X2 + EXPO(I) * EXPO(I)
      END DO
      DENOM = NUMIPT * X2 - X * X

C--- Get scaling factor
      CALL XVP('SCALE',SCALE,CNT)
      DENOM2 = DENOM / SCALE
      CALL XVP('X',XPOINT,XPNTST)
      CALL XVP('CONS',ACONST,CNT)

C--- Open output data sets
      DO I = 1, NUMOPT
	 CALL XVUNIT(OUTUNIT(I),'OUT',I,STATUS,' ')
	 CALL XVOPEN(OUTUNIT(I),STATUS,'OP','WRITE',' ')
      END DO
      DO 400 LINE = 1, NLO
         DO J = 1, NUMIPT
	    CALL XVREAD(INUNIT(J),INBUF(REFIN(J)),STATUS,'SAMP',SSO,
     &		       'NSAMPS',NSO,'LINE',IREC,' ')
         END DO
         IREC = IREC + 1
         OSAMP = 0

C--- This part does linear fit (DN = SLOPE * EXPO + INT)
         DO 200 ISAMP = 1, NSO
            OSAMP = OSAMP + 1
            Y = 0.0
            XY = 0.0
            DO I = 1, NUMIPT
               DN = INBUF(REFIN(I)+ISAMP-1)
               Y = Y + DN
               XY = XY + DN * EXPO(I)
            END DO
            RINT = FLOAT(ISIGN) * ((Y * X2 - X * XY) / DENOM)
            INT = RINT + CONST + ACONST
            IF (ISIGN .EQ. -1) INT = INT + 1
            IF (INT .LT. 1) INT = 0
            IF (INT .GT. MAXDN) INT=MAXDN
            OBUF1(OSAMP) = INT

C--- Calculate SLOPE picture
            IF (2 .LE. NUMOPT) THEN
               SLOPE = ((NUMIPT * XY - X * Y) / DENOM2)
               ISLOPE = SLOPE + 0.5
               IF (ISLOPE .LT. 1) ISLOPE = 0
               IF (ISLOPE .GT. MAXDN) ISLOPE = MAXDN
               OBUF2(OSAMP) = ISLOPE
            ELSE
               GOTO 200
            END IF

C--- Find predicted Y value
            IF (XPNTST .EQ. 0) GO TO 250
            IF (3 .LE. NUMOPT) THEN
               YPOINT = SLOPE * XPOINT + RINT
               IF (YPOINT .LT. 1.0) YPOINT=0.0
               IF (YPOINT .GT. RMAXDN) YPOINT = RMAXDN
               OBUF3(OSAMP) = YPOINT + .5
            ELSE
               GOTO 200
            END IF

C--- Find point which deviates the most from fitted line
  250       IF (4 .LE. NUMOPT) THEN
               DELTA = 0.0
               DO I = 1, NUMIPT                  ! Calculate predicted DN (PDN)
                  PDN = SLOPE * EXPO(I) + RINT
                  RDN = INBUF(REFIN(I)+ISAMP-1)  ! RDN is actual DN
                  TDELTA = ABS(PDN-RDN)
                  IF (TDELTA .GT. DELTA) DELTA = ABS(RDN-PDN)
               END DO
               IF (XPNTST .EQ. 0) THEN
		  OBUF3(OSAMP) = DELTA + ACONST
               ELSE 
		  OBUF4(OSAMP) = DELTA + ACONST
   	       END IF
            END IF
  200    CONTINUE

C--- Write out buffers to files
         CALL XVWRIT(OUTUNIT(1),OBUF1,STATUS,' ')
         IF (NUMOPT .EQ. 2) THEN
             CALL XVWRIT(OUTUNIT(2),OBUF2,STATUS,' ')
         ELSE IF (NUMOPT .EQ. 3) THEN
             CALL XVWRIT(OUTUNIT(2),OBUF2,STATUS,' ')
             CALL XVWRIT(OUTUNIT(3),OBUF3,STATUS,' ')
         ELSE IF (NUMOPT .EQ. 4) THEN
             CALL XVWRIT(OUTUNIT(2),OBUF2,STATUS,' ')
             CALL XVWRIT(OUTUNIT(3),OBUF3,STATUS,' ')
             CALL XVWRIT(OUTUNIT(4),OBUF4,STATUS,' ')
         END IF

C--- Close all files
  400 CONTINUE
      DO I = 1, NUMIPT
        CALL XVCLOSE(INUNIT(I),STATUS,' ')
      END DO
      DO I = 1, NUMOPT
         CALL XVCLOSE(OUTUNIT(I),STATUS,' ')
      END DO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dc.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM dc

   To Create the build file give the command:

		$ vimake dc			(VMS)
   or
		% vimake dc			(Unix)


************************************************************************/


#define PROGRAM	dc
#define R2LIB

#define MODULE_LIST dc.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create dc.pdf
process help=*      !pdf done by HBD
PARM INP      TYPE=STRING   COUNT=(2:10)
PARM OUT      TYPE=STRING   COUNT=(1:4)
PARM SIZE     TYPE=INTEGER  COUNT=0:4                DEFAULT=--
PARM EXPOSURE TYPE=REAL     COUNT=(0:10)             DEFAULT=--
PARM FORMAT   TYPE=KEYWORD  COUNT=0:1 VALID=HALF     DEFAULT=--
PARM NEGATIVE TYPE=KEYWORD  COUNT=0:1 VALID=NEGATIVE DEFAULT=--
PARM MAXDN    TYPE=INTEGER                           DEFAULT=255
PARM SCALE    TYPE=REAL				     DEFAULT=1.0
PARM X        TYPE=REAL     COUNT=0:1                DEFAULT=0.0
PARM CONS     TYPE=REAL     COUNT=0:1		     DEFAULT=0.0
END-PROC
.TITLE
VICAR1 program "dc"
     "dc" will fit a straight line to DN versus exposure at each pixel of
     several input frames and output a zero exposure or "dark current"
     picture. It will also optionally output a slope picture.
.HELP

PURPOSE:

	"dc" is a VICAR applications program which will fit a straight line
to DN versus exposure at each pixel of several input frames and output a zero
exposure or "dark current" picture. DC will also optionally output a slope
picture, a predicted Y value picture and/or a file of deviated points from
the fitted line. This allows the spatial responsivity of linear detectors to
be evaluated with two or more output pictures.

EXECUTION:

"dc" can be executed by the following statement:

	dc INP=(IN1,IN2,...) OUT=(DC,SLOPE,YPOINT,DELTA) SIZE=(SL,SS,NL,NS) PARMS

where INP, OUT, SIZE, and PARMS are required or optional parameters for DC
and are explained in TUTOR mode under TAE.

PARMS consist of the following parameters:

	EXPO	'NEGATIVE    MAXDN     SCALE 
	X       CONS

The input images may either be (all) byte or (all) halfword data.  The data 
format is taken from the VICAR label of the first input file.  The output image
has the same data format (byte or halfword) as the input image. 
.PAGE
OPERATION:

     "dc" fits a straight line on a pixel by pixel basis on two to ten input
pictures in a light transfer sequence. DC extrapolates to zero exposure. If the
analyst suspects for some reason that the zero exposure picture would be
negative, has the option of outputting the negative of the zero exposure
picture. Figure 1 graphically shows the concept involved.


		|
		|
		|			            *
		|			 .     *   .         ^DN
	    DN  |			  *    |            -----*SCALE=SLOPE
		|	             *         |            ^EXPO 
		|	.       *   .          | ^DN
		|          *___________________|
 		|     *        ^EXPO
	      DC|*
		|
		|____________________________________
                0          EXPOSURE

Explanation of FIGURE 1:
		^ - is used to represent a delta
  		* - is used to represent a straight solid line
		. - are points of exposure


  EXAMPLES:

	dc INP=(A,B,C,D,E) OUT=(DC,SLOPE) EXPO=(3.0,4.0,1.0,2.5,2.2)+
	SCALE=2.0

     DC will fit a line to these points in a light transfer sequence and
     output a zero exposure level picture and a slope picture which is
     multiplied by 2.

     RESTRICTIONS:

	1. All the input frames must have as many or more lines and samples
	   as the first input.

WRITTEN BY: Joel Mosher				1 May 1977

COGNIZANT PROGRAMMER: Helen De Rueda		  Feb 1984

Made portable for UNIX by: Alan Scop (CRI)      5 SEP 1994

.LEVEL1
.VARI INP
 Input pictures to be analyzed.
.VARI OUT
 Output pictures. OUT1 is the
 dark current picture. OUT2 is
 the optional slope picture.
.VARI SIZE
 Standard VICAR size field.
 See HELP SIZE.
.VARI EXPOSURE
 Exposure for each input 
 picture.
.VARI FORMAT
 FORMAT is ignored.
.VARI NEGATIVE
 Valid:('NEGATIVE) Specifies to
 output a negative DC picture.
.VARI MAXDN
 Specifies the maximum DN in 
 output pictures.
.VARI SCALE
 Slope picture is to be 
 multiplied by the given
 value.
.VARI X
 Value used to determine
 predicted
 Y value
.VARI CONS
 Constant to be added to
 deviated point.
.LEVEL2
.VARI INP
 INP=(IN1,IN2,...) where INn are file names of input pictures (up to ten) to be
   analyzed.  A minimum of two is required.
.VARI OUT
 OUT=(DC,SLOPE,YPOINT,DELTA) where DC, SLOPE, YPOINT, and DELTA represent
   output files names. DC is a required parmeter and is the dark current
   (or zero exposure intercept) picture. SLOPE is an optional file containing
   the slope picture. YPOINT is also an optional file containing the predicted
   Y value or what the DELTA file contains depending on if an X point is given.
   DELTA is an optional file containing the point deviating the most
   from the fitted line.
.VARI SIZE
 SIZE=(SL,SS,NL,NS) where SL, SS, NL, and NS are integer values specifying the 
   starting line, starting sample, number of line, and number of sample 
   respectively. The SIZE parameter has no real value for this program and may
   be omitted. 
.VARI EXPOSURE
 EXPO=(R1,R2,...) where Rn is a floating point number specifying the exposure
   for each of the input pictures. If the analyst is interested in only the
   dc picture or the relative slopes, the units of exposure are not important.
   There must be an equal number of values following EXPO as there are input
   pictures. There is no defaut. The exposure values must be in the same order
   as the input pictures.
.VARI FORMAT
 The format is obtained from the input image label. 
.VARI NEGATIVE
 Valid:('NEGATIVE) is a keyword specifying that the negative of the dc picture is to be
   outputted. The default is to output the positive picture.
.VARI MAXDN
 MAXDN=I1 where I1 is an integer value specifying that the maximum DN in the
   output pictures is equal to I1. The default is 255 for byte and 32767 for
   halfword picture.
.VARI SCALE
 SCALE=R1 where R1 is a floating point number specifying that the second 
   output (slope picture) is to be multiplied by R1. The default is 1.0.
.VARI X
 X=R2 where R2 is a floating point number that is used to calculate
   the predicted Y value.
.VARI CONS
 CONS=R3 where R3 is a floating point number that is used to calculate the
   point which diviates the most from the fitted line.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstdc.pdf
procedure
refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
gen a 10 10
gen b 10 10 sinc=2 linc=2
gen c 10 10 sinc=100 linc=100
write "FIND DARK CURRENT AND SLOPE PICTURE"
dc (a,b,c) (d,e) 'nega expo=(2.5,3.0,6.0) MAXDN=255
write "list calculated dark current image"
list d
write "list calculated slope image"
list e
write "FIND DARK CURRENT, SLOPE, PREDICTED Y AND DEVIATED POINT PICTURES"
dc (b,a) (c,d,e,f) expo=(2.5,3.0) x=2 scal=2 cons=2
write "list calculated dark current image"
list c
write "list calculated slope image, output should be zeroes"
list d 'zero
write "list calculated predicted-y image, output should be zeroes"
list e 'zero
write "list calculated deviated-point image"
list f
write "DO SAME AS ABOVE, BUT WITH HALFWORD"
gen aa 10 10 'half
gen bb 10 10 'half sinc=2 linc=2
gen cc 10 10 'half sinc=100 linc=100
dc (aa,bb,cc) (dd,ee) 'nega expo=(2.5,3.0,6.0) MAXDN=32767
write "list calculated dark current image"
list dd
write "list calculated slope image"
list ee
dc (bb,aa) (cc,dd,ee,ff) expo=(2.5,3.0) x=2 scal=2 cons=2 
write "list calculated dark current image"
list cc
write "list calculated slope image; output should be zeroes"
list dd 'zero
write "list calculated predicted-y image; output should be zeroes"
list ee 'zero
write "list calculated deviated-point image"
list ff
end-proc
$ Return
$!#############################################################################
