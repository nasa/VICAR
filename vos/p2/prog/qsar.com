$!****************************************************************************
$!
$! Build proc for MIPL module qsar
$! VPACK Version 1.9, Monday, December 07, 2009, 16:57:59
$!
$! Execute by entering:		$ @qsar
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
$ write sys$output "*** module qsar ***"
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
$ write sys$output "Invalid argument given to qsar.com file -- ", primary
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
$   if F$SEARCH("qsar.imake") .nes. ""
$   then
$      vimake qsar
$      purge qsar.bld
$   else
$      if F$SEARCH("qsar.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake qsar
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @qsar.bld "STD"
$   else
$      @qsar.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create qsar.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack qsar.com -mixed -
	-s qsar.f -
	-i qsar.imake -
	-p qsar.pdf -
	-t tstqsar.pdf new_session_3d.log old_session_3d.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create qsar.f
$ DECK/DOLLARS="$ VOKAGLEVE"
       PROGRAM  QSAR
C#######################################################################
C  NAME OF ROUTINE
C      QSAR ( Quadrilateral Segment Averaging Routine )
C             Quadrilateral means rectangle here. SAR part is a misnomer.
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      Program QSAR is used to add or subtract values from the data numbers in
C      rectangular sections of an image. 
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    OCTOBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C
C  ORIGINAL QSAR PROGRAM BY
C      HOWARD FRIEDEN with modifications by JOEL MOSHER
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C
C  CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C
C      QSAR INP=a OUT=b SIZE=(sl,ss,nl,ns) optional parameters
C      QSAR INP=a OUT=b SL=sl SS=ss NL=nl NS=ns optional parameters
C      QSAR a b (sl,ss,nl,ns) optional parameters
C      QSAR a b optional parameters
C
C       Here 'a' represents the input image file name,
C       'b' represents the output image file name.
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file name.
C      OUT    - Output file name.
C      SIZE   - Standard Vicar size field:  (SL,SS,NL,NS)
C               SL = Starting line number.
C               SS = Starting sample number.
C               NL = Number of lines.
C               NS = Number of samples.
C      AREA   - Specifies the rectangles and the values to be added to data 
C               numbers.
C  OUTPUT PARAMETERS
C      The output image produced is written to the output file.
C  PROGRAM LIMITATIONS
C      1. The input and output images must be byte or halfword data.
C      2. The maximum number of pixels per line is 64000.
C      3. The maximum number of rectangles that can be specified is 120,
C         as of this writing.
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44 
C#######################################################################
C  NAME OF ROUTINE
C     MAIN44 (name for top level subroutine by VICAR convention)
C
C  PURPOSE
C      MAIN44 is used to add or subtract values from the data numbers in
C      rectangular sections of an image. 
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     OCTOBER 1983
C  FOR
C      EARTH RESOURCES APPLICATIONS
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    09-03  NTT  ENABLED FOR 3D IMAGES
C    04-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C    07-85  JHR  CONVERTED TO VICAR2
C    10-83  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS CLEANUP.
C    10-83  SP   INCREASED MAXIMUM LINE SIZE TO 64000 PIXELS.
C    10-83  SP   CHANGED LABEL PROCESSING OF INPUT FILE BY USING LABELC
C                TO FIND THE DATA FORMAT AND USING FORMAT IN LABEL AS DEFAULT.
C    10-83  SP   CHANGED HALF PARAMETER TO FORMAT=HALF/BYTE SO USER CAN
C                OVERRIDE LABEL WHEN LABEL SAYS HALF.
C    10-83  SP   CHANGED LABEL PROCESSING OF OUTPUT FILE TO ALWAYS INCLUDE 
C                CORRECT DATA FORMAT IN LABEL. 
C    10-83  SP   MODIFIED PROGRAM SO THAT STARTING PIXEL VALUE IN THE  AREA
C                PARAMETER REFERS TO POSITION IN INPUT IMAGE EVEN WHEN THE
C                STARTING SAMPLE IN THE SIZE FIELD IS NOT 1. (THE STARTING
C                LINE VALUE IN THE  AREA PARAMETER ALREADY WAS SET TO REFER
C                TO THE POSITION IN INPUT IMAGE EVEN WHEN THE
C                STARTING LINE IN THE SIZE FIELD IS NOT 1. )
C    10-83  SP   CORRECTED PROGRAM TO NOT STORE VARIABLE INT INTO IBUF ARRAY
C                WHEN THE VALUE OF INT HAD NOT BEEN SET PROPERLY ( I.E. WHEN
C                LOOP INDEX K WAS NOT IN RANGE.)
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT AND OUTPUT PARAMETERS     
C      SEE UNDER PROGRAM QSAR.
C      
C  CALLED BY
C      QSAR
C  SUBROUTINES CALLED
C      The library routines  ABEND, XVOPEN, XVCLOSE, XVPARM,  
C                            XVREAD, XVWRIT, XVUNIT, XVGET
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C * 27 JUNE 1975...DAH...CHANGES FOR CONVERSION TO 360/OS
C *   25 JULY 79   ...JBS...   MAX LINE LENGTH TO 4000 BYTES
C *   5 JUNE 80   ...JAM...   ADD HALFWORD OPTION
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C   THIS PROGRAM FOLLOWS THE STANDARD FORTRAN NAMING CONVENTION FOR VARIABLES:
C   VARIABLES STARTING WITH I-N ARE INTEGERS UNLESS EXPLICITLY DECLARED.

      PARAMETER ( LSIZE_PAR = 64000 )      ! MAX NUMBER OF PIXELS PER LINE.
      INTEGER*4 OUNIT,STAT,SS,SLO,SSO,IPARM(1800),ES,ADJ,SBO,NBO,NBI,LBO
      INTEGER*4 BANDOUT,LINEOUT,BAND,LINE
      INTEGER*2 IBUF(LSIZE_PAR)
      CHARACTER*8 FORMAT
      CHARACTER*3 ORGIN

      CALL IFMESSAGE('QSAR version 08-SEP-03')
      CALL XVEACTION('SA',' ')

C          OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')

C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      IF(FORMAT.NE.'BYTE'.AND.FORMAT.NE.'HALF') THEN
         CALL XVMESSAGE('QSAR ACCEPTS BYTE AND HALFWORD DATA ONLY',' ')
         CALL ABEND
      END IF

C     CHECK FORMAT
      CALL XVGET(IUNIT,I,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')

C        GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      IF(SLO+NLO-1 .GT. NLI) THEN
         CALL XVMESSAGE(
     & 'NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE',' ')
         CALL ABEND
      END IF
      IF(SSO+NSO-1 .GT. NSI) THEN
         CALL XVMESSAGE(
     & 'NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE',' ')
         CALL ABEND
      END IF
      CALL XVBANDS(SBO,NBO,NBI)

      IF ( SBO .GT. NBO ) CALL MABEND(
     +  'SB is greater than the total number of bands')
                 
      IF ( SBO + NBO - 1 .GT. NBI) THEN
        CALL XVMESSAGE('***Number of bands truncated', ' ')
        NBO = NBI + 1 - SBO
      ENDIF

      

C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_FORMAT','HALF',
     &            'U_NL',NLO,'U_NS',NSO,'U_NB',NBO,' ')

C        PROCESS PARAMETERS
      CALL XVPARM('AREA',IPARM,ICOUNT,IDEF,1800)
      IF(MOD(ICOUNT,5).NE.0) THEN
         CALL XVMESSAGE(' NUMBER OF PARAMETERS NOT A MULTIPLE OF FIVE',
     +			' ')
         CALL ABEND
      END IF

      LLO=SLO+NLO-1                 ! LAST LINE.
      LBO=SBO+NBO-1                 ! LAST BAND.

C  COPY THE INPUT IMAGE TO OUTPUT MAKING THE REQUESTED MODIFICATIONS.
      BANDOUT=0
      DO 105 BAND=SBO,LBO
        BANDOUT=BANDOUT+1
        LINEOUT=0
        DO 100 LINE=SLO,LLO
         LINEOUT=LINEOUT+1
         CALL XVREAD(IUNIT,IBUF,STAT,'LINE',LINE,'BAND',BAND,' ')

         IF(ICOUNT.EQ.0) GO TO 55      ! IF NO MODIFICATIONS, JUST COPY.

         DO 50 J = 1,ICOUNT,5
C              CHECK FOR MODS TO THIS LINE
            IF(LINE.LT.IPARM(J).OR.LINE.GE.IPARM(J)+IPARM(J+2)) GO TO 50
            SS=IPARM(J+1)                ! STARTING PIXEL.
            IF(SS.LT.1) THEN
               CALL XVMESSAGE('INVALID SS PARAMETER SPECIFIED',' ')
               CALL ABEND
            END IF
            ES=IPARM(J+3)+SS-1           ! ENDING PIXEL.
            IF(ES.GT.NSI) THEN
               CALL XVMESSAGE('INPUT SAMPLE SIZE EXCEEDED',' ')
               CALL ABEND
            END IF
            ADJ=IPARM(J+4)               ! ADJUSTMENT TO BE ADDED.
C 
            DO K=SS,ES
               INT=IBUF(K)+ADJ
               IF(FORMAT.EQ.'HALF')  THEN 
                  IF(INT.LT.-32768) INT=-32768
                  IF(INT.GT.32767)  INT= 32767
               ELSE 
                  IF (INT.LT.0) INT=0
                  IF (INT.GT.255) INT=255
               END IF 
               IBUF(K)=INT
            END DO

50       CONTINUE

55       CALL XVWRIT(OUNIT,IBUF(SSO),STAT,'NSAMPS',NSO,'BAND',BANDOUT,
     +        'LINE',LINEOUT,' ')
100     CONTINUE
105   CONTINUE

C        CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
C
      RETURN        
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create qsar.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM qsar

   To Create the build file give the command:

		$ vimake qsar			(VMS)
   or
		% vimake qsar			(Unix)


************************************************************************/


#define PROGRAM	qsar
#define R2LIB

#define MODULE_LIST qsar.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create qsar.pdf
process help=*
!  FILE NAMES      
!
PARM INP     TYPE=STRING   COUNT=1
PARM OUT     TYPE=STRING   COUNT=1
PARM SIZE    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SS      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SB      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM NL      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NB      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM AREA    TYPE=INTEGER  COUNT=(5:600) DEFAULT=(0,0,0,0,0) 
!
END-PROC
.TITLE
qsar
.HELP
 PURPOSE:

Program qsar is used to add or subtract values from the data numbers in
rectangular sections of an image.  For each rectangle, the user specifies
the size and position of the rectangle and the (positive or negative) value
to be added to the data numbers of points in that rectangle.  qsar performs
the addition and writes the resulting image to an output file.  qsar is 
primarily used to correct flaws in images or, in conjunction with program GEN,
to generate test images.
.PAGE
 EXECUTION:

The input image may either be byte or halfword data.  The position of the 
rectangles are specified relative to the full input image.

The maximum number of rectangles that can be specified for qsar is limited
by the TAE executive.  As of this writing the limit is 120 rectangles.
If no rectangles are specified, qsar copies the image without making any
changes.  For each rectangle specified, the program adds the specified
value to the data numbers of the points of that rectangle. The resulting
data numbers are then checked for being valid for the data type (byte or
halfword) of the image and are adjusted if invalid.  For byte data,
data numbers less than 0 are set to 0, and data numbers greater than 255
are set to 255.  For halfword data, data numbers less than -32768 are set to 
-32768, and data numbers greater than 32767 are set to 32767.  Rectangles
are allowed to overlap.  For points in more than one rectangle, the additions
are performed in the order they are entered by the user, and adjustments
required for validity are applied after each addition.

The output image has the same data format  (byte or halfword) as the input 
image.  
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      qsar INP=a OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      qsar INP=a OUT=b  SL=sl SS=ss NL=nl NS=ns optional parameters
      qsar a b (sl,ss,nl,ns) optional parameters
      qsar a b optional parameters

       Here 'a' represents the input image file name, and
       'b' represents the output image file name.
.PAGE
EXAMPLES

1.    GEN A NL=250 NS=250 IVAL=0 LINC=0 SINC=0
      qsar INP=A OUT=B AREA=( 1   1  250   50   50     +
                              1  51  250   50  100     +
                              1 101  250   50  150     +
                              1 151  250   50  200     +
                              1 201  250   50  250       )

      In this example the output file B is a 250x250 byte image
      consisting of five vertical strips of different shades of gray.
.PAGE
2.    GEN QSARE NL=10 NS=10 'HALF
      qsar INP=QSARE OUT=QSAREO SIZE=(2,3,8,7) FORMAT=HALF AREA=(4,6,3,1,100)

      In this example the output file QSAREO is a halfword image that has
      eight lines with seven pixels per line.  Data numbers for a three line
      by 1 pixel rectangle are incremented by 100.
.PAGE
RESTRICTIONS
1. The input and output images must be byte or halfword data.
2. The maximum number of pixels per line is 64000.
3. The maximum number of rectangles that can be specified is 120, as of this
   writing.

 WRITTEN BY:             Steve Pohorsky              24 Oct 1983

 COGNIZANT PROGRAMMER:   Joel Mosher                 29 May 1980

 REVISION:  
     08 Sep 2003   NTT   Enabled for 3D images.

.LEVEL1
.VARIABLE INP
Input file name
.VARIABLE OUT
Output file name
.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
You can enter SL,SS,NL,
and NS together as SIZE, OR
enter the SL,SS,NL, and NS
parameters separately.
By default, the entire input
image is used if these
parameters are not entered.
.VARIABLE SL
Starting line number
.VARIABLE SS
Starting sample number
.VARIABLE SB
Starting band number
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE NB
Number of bands
.VARIABLE AREA
Specifies the rectangles and the
values to be added to data 
numbers.
.LEVEL2
.VARIABLE AREA
The values entered for the AREA parameter are entered in groups of five.
Each group corresponds to a rectangle.  The meaning of the values in each
group is as follows.

  First value  - starting line number of the rectangle.
  Second value - starting pixel number of the rectangle. 
  Third value  - number of lines in the rectangle.
  Fourth value - number of pixels per line in the rectangle.  
  Fifth value  - the integer value to be added to the data numbers for points 
                 in the rectangle.  For byte data, this value should be in the
                 range -255 to 255.  For halfword data, this value should be 
                 in the range -65535 to 65535.

The maximum number of rectangles that can be specified for qsar is limited
by the TAE executive.  As of this writing the limit is 120 rectangles.
If no rectangles are specified, qsar copies the image without making any
changes.  
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstqsar.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!  THIS IS A TEST OF PROGRAM qsar
!
!      first make a rectangle against a zero background.
!
gen qsara NL=10 NS=10 IVAL=0 LINC=0 SINC=0
qsar INP=qsara OUT=qsarao AREA=( 2 4 8 3 111 )
list qsarao 'ZEROES
!
!    try SL and SS not equal to 1.
!
gen qsarb NL=10 NS=10
list qsarb
qsar INP=qsarb OUT=qsarbo SIZE=(2,3,8,7) AREA=(4,6,3,1,100) 
list qsarbo
!
!    try with no optional parameters. should do a copy.
!
qsar qsarb qsarb1
list qsarb1
!
!    try out the cutoff at 0 and 255.
!
qsar qsarb qsarb2 AREA=( 2 2 5 5 -3,   5 5 5 5 240 )
list qsarb2     
!
!  Repeat above tests for halfword images.
!      first make a rectangle against a zero background.
!
gen qsard NL=10 NS=10 IVAL=0 LINC=0 SINC=0 'HALF
qsar INP=qsard OUT=qsardo AREA=( 2 4 8 3 111 )
list qsardo 'ZEROES
!
!    try SL and SS not equal to 1.
!
gen qsare NL=10 NS=10 'HALF
list qsare
qsar INP=qsare OUT=qsareo SIZE=(2,3,8,7) AREA=(4,6,3,1,100)
list qsareo
!
!    try with no optional parameters. should do a copy.
!
qsar qsare qsare1
list qsare1
!
!    try out the cutoff at -32768 and 32767.
!
qsar qsare qsare2 AREA=( 2 2 5 5 -32771,   5 5 5 5 32760 )
list qsare2     
!
! TEST 3D IMAGES
!
gen qsar3d 10 10 3 'HALF
list qsar3d
qsar INP=qsar3d OUT=qsar3do SIZE=(2,3,8,7) AREA=(4,6,3,1,100)
list qsar3do
!
!    clean up
!
! DCL DELETE QSAR*.Z*;*
end-proc
$!-----------------------------------------------------------------------------
$ create new_session_3d.log
tstqsar
gen qsara NL=10 NS=10 IVAL=0 LINC=0 SINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
qsar INP=qsara OUT=qsarao AREA=( 2 4 8 3 111 )
Beginning VICAR task qsar
QSAR version 08-SEP-03
list qsarao 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:29 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:03:29 2003
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0 111 111 111   0   0   0   0
      3       0   0   0 111 111 111   0   0   0   0
      4       0   0   0 111 111 111   0   0   0   0
      5       0   0   0 111 111 111   0   0   0   0
      6       0   0   0 111 111 111   0   0   0   0
      7       0   0   0 111 111 111   0   0   0   0
      8       0   0   0 111 111 111   0   0   0   0
      9       0   0   0 111 111 111   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0
gen qsarb NL=10 NS=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list qsarb
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:29 2003
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
qsar INP=qsarb OUT=qsarbo SIZE=(2,3,8,7) AREA=(4,6,3,1,100)
Beginning VICAR task qsar
QSAR version 08-SEP-03
list qsarbo
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:29 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:03:29 2003
     Samp     1       3       5       7
   Line
      1       3   4   5   6   7   8   9
      2       4   5   6   7   8   9  10
      3       5   6   7 108   9  10  11
      4       6   7   8 109  10  11  12
      5       7   8   9 110  11  12  13
      6       8   9  10  11  12  13  14
      7       9  10  11  12  13  14  15
      8      10  11  12  13  14  15  16
qsar qsarb qsarb1
Beginning VICAR task qsar
QSAR version 08-SEP-03
list qsarb1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:29 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:03:30 2003
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
qsar qsarb qsarb2 AREA=( 2 2 5 5 -3,   5 5 5 5 240 )
Beginning VICAR task qsar
QSAR version 08-SEP-03
list qsarb2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:29 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:03:30 2003
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   0   0   1   2   3   7   8   9  10
      3       2   0   1   2   3   4   8   9  10  11
      4       3   1   2   3   4   5   9  10  11  12
      5       4   2   3   4 245 246 250 251 252  13
      6       5   3   4   5 246 247 251 252 253  14
      7       6   7   8   9 250 251 252 253 254  15
      8       7   8   9  10 251 252 253 254 255  16
      9       8   9  10  11 252 253 254 255 255  17
     10       9  10  11  12  13  14  15  16  17  18
gen qsard NL=10 NS=10 IVAL=0 LINC=0 SINC=0 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
qsar INP=qsard OUT=qsardo AREA=( 2 4 8 3 111 )
Beginning VICAR task qsar
QSAR version 08-SEP-03
list qsardo 'ZEROES
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:30 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:03:31 2003
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     0     0     0     0     0     0     0     0     0
      2         0     0     0   111   111   111     0     0     0     0
      3         0     0     0   111   111   111     0     0     0     0
      4         0     0     0   111   111   111     0     0     0     0
      5         0     0     0   111   111   111     0     0     0     0
      6         0     0     0   111   111   111     0     0     0     0
      7         0     0     0   111   111   111     0     0     0     0
      8         0     0     0   111   111   111     0     0     0     0
      9         0     0     0   111   111   111     0     0     0     0
     10         0     0     0     0     0     0     0     0     0     0
gen qsare NL=10 NS=10 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list qsare
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:31 2003
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3     4     5     6     7     8     9    10    11
      4         3     4     5     6     7     8     9    10    11    12
      5         4     5     6     7     8     9    10    11    12    13
      6         5     6     7     8     9    10    11    12    13    14
      7         6     7     8     9    10    11    12    13    14    15
      8         7     8     9    10    11    12    13    14    15    16
      9         8     9    10    11    12    13    14    15    16    17
     10         9    10    11    12    13    14    15    16    17    18
qsar INP=qsare OUT=qsareo SIZE=(2,3,8,7) AREA=(4,6,3,1,100)
Beginning VICAR task qsar
QSAR version 08-SEP-03
list qsareo
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:31 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:03:31 2003
     Samp       1     2     3     4     5     6     7
   Line
      1         3     4     5     6     7     8     9
      2         4     5     6     7     8     9    10
      3         5     6     7   108     9    10    11
      4         6     7     8   109    10    11    12
      5         7     8     9   110    11    12    13
      6         8     9    10    11    12    13    14
      7         9    10    11    12    13    14    15
      8        10    11    12    13    14    15    16
qsar qsare qsare1
Beginning VICAR task qsar
QSAR version 08-SEP-03
list qsare1
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:31 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:03:31 2003
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3     4     5     6     7     8     9    10    11
      4         3     4     5     6     7     8     9    10    11    12
      5         4     5     6     7     8     9    10    11    12    13
      6         5     6     7     8     9    10    11    12    13    14
      7         6     7     8     9    10    11    12    13    14    15
      8         7     8     9    10    11    12    13    14    15    16
      9         8     9    10    11    12    13    14    15    16    17
     10         9    10    11    12    13    14    15    16    17    18
qsar qsare qsare2 AREA=( 2 2 5 5 -32771,   5 5 5 5 32760 )
Beginning VICAR task qsar
QSAR version 08-SEP-03
list qsare2
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:31 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:03:32 2003
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1-32768-32768-32767-32766-32765     7     8     9    10
      3         2-32768-32767-32766-32765-32764     8     9    10    11
      4         3-32767-32766-32765-32764-32763     9    10    11    12
      5         4-32766-32765-32764    -3    -2 32767 32767 32767    13
      6         5-32765-32764-32763    -2    -1 32767 32767 32767    14
      7         6     7     8     9 32767 32767 32767 32767 32767    15
      8         7     8     9    10 32767 32767 32767 32767 32767    16
      9         8     9    10    11 32767 32767 32767 32767 32767    17
     10         9    10    11    12    13    14    15    16    17    18
gen qsar3d 10 10 3 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list qsar3d
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:32 2003
 ***********
 Band =     1
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3     4     5     6     7     8     9    10    11
      4         3     4     5     6     7     8     9    10    11    12
      5         4     5     6     7     8     9    10    11    12    13
      6         5     6     7     8     9    10    11    12    13    14
      7         6     7     8     9    10    11    12    13    14    15
      8         7     8     9    10    11    12    13    14    15    16
      9         8     9    10    11    12    13    14    15    16    17
     10         9    10    11    12    13    14    15    16    17    18


 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:32 2003
 ***********
 Band =     2
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         1     2     3     4     5     6     7     8     9    10
      2         2     3     4     5     6     7     8     9    10    11
      3         3     4     5     6     7     8     9    10    11    12
      4         4     5     6     7     8     9    10    11    12    13
      5         5     6     7     8     9    10    11    12    13    14
      6         6     7     8     9    10    11    12    13    14    15
      7         7     8     9    10    11    12    13    14    15    16
      8         8     9    10    11    12    13    14    15    16    17
      9         9    10    11    12    13    14    15    16    17    18
     10        10    11    12    13    14    15    16    17    18    19


 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:32 2003
 ***********
 Band =     3
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         2     3     4     5     6     7     8     9    10    11
      2         3     4     5     6     7     8     9    10    11    12
      3         4     5     6     7     8     9    10    11    12    13
      4         5     6     7     8     9    10    11    12    13    14
      5         6     7     8     9    10    11    12    13    14    15
      6         7     8     9    10    11    12    13    14    15    16
      7         8     9    10    11    12    13    14    15    16    17
      8         9    10    11    12    13    14    15    16    17    18
      9        10    11    12    13    14    15    16    17    18    19
     10        11    12    13    14    15    16    17    18    19    20
qsar INP=qsar3d OUT=qsar3do SIZE=(2,3,8,7) AREA=(4,6,3,1,100)
Beginning VICAR task qsar
QSAR version 08-SEP-03
list qsar3do
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:32 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:03:32 2003
 ***********
 Band =     1
 ***********
     Samp       1     2     3     4     5     6     7
   Line
      1         3     4     5     6     7     8     9
      2         4     5     6     7     8     9    10
      3         5     6     7   108     9    10    11
      4         6     7     8   109    10    11    12
      5         7     8     9   110    11    12    13
      6         8     9    10    11    12    13    14
      7         9    10    11    12    13    14    15
      8        10    11    12    13    14    15    16


 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:32 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:03:32 2003
 ***********
 Band =     2
 ***********
     Samp       1     2     3     4     5     6     7
   Line
      1         4     5     6     7     8     9    10
      2         5     6     7     8     9    10    11
      3         6     7     8   109    10    11    12
      4         7     8     9   110    11    12    13
      5         8     9    10   111    12    13    14
      6         9    10    11    12    13    14    15
      7        10    11    12    13    14    15    16
      8        11    12    13    14    15    16    17


 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:03:32 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:03:32 2003
 ***********
 Band =     3
 ***********
     Samp       1     2     3     4     5     6     7
   Line
      1         5     6     7     8     9    10    11
      2         6     7     8     9    10    11    12
      3         7     8     9   110    11    12    13
      4         8     9    10   111    12    13    14
      5         9    10    11   112    13    14    15
      6        10    11    12    13    14    15    16
      7        11    12    13    14    15    16    17
      8        12    13    14    15    16    17    18
end-proc
disable-log
$!-----------------------------------------------------------------------------
$ create old_session_3d.log
tstqsar
gen qsara NL=10 NS=10 IVAL=0 LINC=0 SINC=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
qsar INP=qsara OUT=qsarao AREA=( 2 4 8 3 111 )
Beginning VICAR task qsar
QSAR version 02-MAY-94
list qsarao 'ZEROES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:04:00 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:04:01 2003
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0 111 111 111   0   0   0   0
      3       0   0   0 111 111 111   0   0   0   0
      4       0   0   0 111 111 111   0   0   0   0
      5       0   0   0 111 111 111   0   0   0   0
      6       0   0   0 111 111 111   0   0   0   0
      7       0   0   0 111 111 111   0   0   0   0
      8       0   0   0 111 111 111   0   0   0   0
      9       0   0   0 111 111 111   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0
gen qsarb NL=10 NS=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list qsarb
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:04:01 2003
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
qsar INP=qsarb OUT=qsarbo SIZE=(2,3,8,7) AREA=(4,6,3,1,100)
Beginning VICAR task qsar
QSAR version 02-MAY-94
list qsarbo
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:04:01 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:04:01 2003
     Samp     1       3       5       7
   Line
      1       3   4   5   6   7   8   9
      2       4   5   6   7   8   9  10
      3       5   6   7 108   9  10  11
      4       6   7   8 109  10  11  12
      5       7   8   9 110  11  12  13
      6       8   9  10  11  12  13  14
      7       9  10  11  12  13  14  15
      8      10  11  12  13  14  15  16
qsar qsarb qsarb1
Beginning VICAR task qsar
QSAR version 02-MAY-94
list qsarb1
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:04:01 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:04:01 2003
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   2   3   4   5   6   7   8   9  10
      3       2   3   4   5   6   7   8   9  10  11
      4       3   4   5   6   7   8   9  10  11  12
      5       4   5   6   7   8   9  10  11  12  13
      6       5   6   7   8   9  10  11  12  13  14
      7       6   7   8   9  10  11  12  13  14  15
      8       7   8   9  10  11  12  13  14  15  16
      9       8   9  10  11  12  13  14  15  16  17
     10       9  10  11  12  13  14  15  16  17  18
qsar qsarb qsarb2 AREA=( 2 2 5 5 -3,   5 5 5 5 240 )
Beginning VICAR task qsar
QSAR version 02-MAY-94
list qsarb2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:04:01 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:04:02 2003
     Samp     1       3       5       7       9
   Line
      1       0   1   2   3   4   5   6   7   8   9
      2       1   0   0   1   2   3   7   8   9  10
      3       2   0   1   2   3   4   8   9  10  11
      4       3   1   2   3   4   5   9  10  11  12
      5       4   2   3   4 245 246 250 251 252  13
      6       5   3   4   5 246 247 251 252 253  14
      7       6   7   8   9 250 251 252 253 254  15
      8       7   8   9  10 251 252 253 254 255  16
      9       8   9  10  11 252 253 254 255 255  17
     10       9  10  11  12  13  14  15  16  17  18
gen qsard NL=10 NS=10 IVAL=0 LINC=0 SINC=0 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
qsar INP=qsard OUT=qsardo AREA=( 2 4 8 3 111 )
Beginning VICAR task qsar
QSAR version 02-MAY-94
list qsardo 'ZEROES
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:04:02 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:04:02 2003
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     0     0     0     0     0     0     0     0     0
      2         0     0     0   111   111   111     0     0     0     0
      3         0     0     0   111   111   111     0     0     0     0
      4         0     0     0   111   111   111     0     0     0     0
      5         0     0     0   111   111   111     0     0     0     0
      6         0     0     0   111   111   111     0     0     0     0
      7         0     0     0   111   111   111     0     0     0     0
      8         0     0     0   111   111   111     0     0     0     0
      9         0     0     0   111   111   111     0     0     0     0
     10         0     0     0     0     0     0     0     0     0     0
gen qsare NL=10 NS=10 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list qsare
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:04:02 2003
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3     4     5     6     7     8     9    10    11
      4         3     4     5     6     7     8     9    10    11    12
      5         4     5     6     7     8     9    10    11    12    13
      6         5     6     7     8     9    10    11    12    13    14
      7         6     7     8     9    10    11    12    13    14    15
      8         7     8     9    10    11    12    13    14    15    16
      9         8     9    10    11    12    13    14    15    16    17
     10         9    10    11    12    13    14    15    16    17    18
qsar INP=qsare OUT=qsareo SIZE=(2,3,8,7) AREA=(4,6,3,1,100)
Beginning VICAR task qsar
QSAR version 02-MAY-94
list qsareo
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:04:02 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:04:03 2003
     Samp       1     2     3     4     5     6     7
   Line
      1         3     4     5     6     7     8     9
      2         4     5     6     7     8     9    10
      3         5     6     7   108     9    10    11
      4         6     7     8   109    10    11    12
      5         7     8     9   110    11    12    13
      6         8     9    10    11    12    13    14
      7         9    10    11    12    13    14    15
      8        10    11    12    13    14    15    16
qsar qsare qsare1
Beginning VICAR task qsar
QSAR version 02-MAY-94
list qsare1
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:04:02 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:04:03 2003
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3     4     5     6     7     8     9    10    11
      4         3     4     5     6     7     8     9    10    11    12
      5         4     5     6     7     8     9    10    11    12    13
      6         5     6     7     8     9    10    11    12    13    14
      7         6     7     8     9    10    11    12    13    14    15
      8         7     8     9    10    11    12    13    14    15    16
      9         8     9    10    11    12    13    14    15    16    17
     10         9    10    11    12    13    14    15    16    17    18
qsar qsare qsare2 AREA=( 2 2 5 5 -32771,   5 5 5 5 32760 )
Beginning VICAR task qsar
QSAR version 02-MAY-94
list qsare2
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:04:02 2003
 Task:QSAR      User:ntt       Date_Time:Mon Sep  8 13:04:03 2003
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1-32768-32768-32767-32766-32765     7     8     9    10
      3         2-32768-32767-32766-32765-32764     8     9    10    11
      4         3-32767-32766-32765-32764-32763     9    10    11    12
      5         4-32766-32765-32764    -3    -2 32767 32767 32767    13
      6         5-32765-32764-32763    -2    -1 32767 32767 32767    14
      7         6     7     8     9 32767 32767 32767 32767 32767    15
      8         7     8     9    10 32767 32767 32767 32767 32767    16
      9         8     9    10    11 32767 32767 32767 32767 32767    17
     10         9    10    11    12    13    14    15    16    17    18
gen qsar3d 10 10 3 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list qsar3d
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:04:04 2003
 ***********
 Band =     1
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0     1     2     3     4     5     6     7     8     9
      2         1     2     3     4     5     6     7     8     9    10
      3         2     3     4     5     6     7     8     9    10    11
      4         3     4     5     6     7     8     9    10    11    12
      5         4     5     6     7     8     9    10    11    12    13
      6         5     6     7     8     9    10    11    12    13    14
      7         6     7     8     9    10    11    12    13    14    15
      8         7     8     9    10    11    12    13    14    15    16
      9         8     9    10    11    12    13    14    15    16    17
     10         9    10    11    12    13    14    15    16    17    18


 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:04:04 2003
 ***********
 Band =     2
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         1     2     3     4     5     6     7     8     9    10
      2         2     3     4     5     6     7     8     9    10    11
      3         3     4     5     6     7     8     9    10    11    12
      4         4     5     6     7     8     9    10    11    12    13
      5         5     6     7     8     9    10    11    12    13    14
      6         6     7     8     9    10    11    12    13    14    15
      7         7     8     9    10    11    12    13    14    15    16
      8         8     9    10    11    12    13    14    15    16    17
      9         9    10    11    12    13    14    15    16    17    18
     10        10    11    12    13    14    15    16    17    18    19


 Task:GEN       User:ntt       Date_Time:Mon Sep  8 13:04:04 2003
 ***********
 Band =     3
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         2     3     4     5     6     7     8     9    10    11
      2         3     4     5     6     7     8     9    10    11    12
      3         4     5     6     7     8     9    10    11    12    13
      4         5     6     7     8     9    10    11    12    13    14
      5         6     7     8     9    10    11    12    13    14    15
      6         7     8     9    10    11    12    13    14    15    16
      7         8     9    10    11    12    13    14    15    16    17
      8         9    10    11    12    13    14    15    16    17    18
      9        10    11    12    13    14    15    16    17    18    19
     10        11    12    13    14    15    16    17    18    19    20
qsar INP=qsar3d OUT=qsar3do SIZE=(2,3,8,7) AREA=(4,6,3,1,100)
Beginning VICAR task qsar
QSAR version 02-MAY-94
[VIC2-GENERR] Exception in XVREAD, processing file: qsar3d
[VIC2-STRTREC] Bad starting record for read or write operation; program error.
 Current line in image = 0
 ** ABEND called **
continue
list qsar3do
Beginning VICAR task list
 ** The specified window is all zero.
end-proc
disable-log
$ Return
$!#############################################################################
