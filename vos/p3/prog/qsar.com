$!****************************************************************************
$!
$! Build proc for MIPL module qsar
$! VPACK Version 1.5, Monday, March 29, 1993, 14:50:41
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
$ vpack qsar.com -
	-s qsar.f -
	-p qsar.pdf -
	-i qsar.imake
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
C    04-91  REA  ADAPTED FOR UNIX/VICAR
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
C      The library routines  ABEND, XVOPEN, XVCLOSE, XVPARM, QPRINT, 
C                            XVREAD, XVWRIT, XVCHECK, XVUNIT, XVGET
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C * 27 JUNE 1975...DAH...CHANGES FOR CONVERSION TO 360/OS
C *   25 JULY 79   ...JBS...   MAX LINE LENGTH TO 4000 BYTES
C *   5 JUNE 80   ...JAM...   ADD HALFWORD OPTION
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C   THIS PROGRAM FOLLOWS THE STANDARD FORTRAN NAMING CONVENTION FOR VARIABLES:
C   VARIABLES STARTING WITH I-N ARE INTEGERS UNLESS EXPLICITLY DECLARED.

      PARAMETER ( LSIZE_PAR = 64000 )      ! MAX NUMBER OF PIXELS PER LINE.
      INTEGER*4 OUNIT,STAT,SS,SLO,SSO,IPARM(1800),ES,ADJ
      INTEGER*2 IBUF(LSIZE_PAR)
      CHARACTER*8 FORMAT


C          OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF','OPEN_ACT','SA',
     +            'IO_ACT','SA',' ')

C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      IF(FORMAT.NE.'BYTE'.AND.FORMAT.NE.'HALF') THEN
	 CALL XVMESSAGE(' QSAR ACCEPTS BYTE AND HALFWORD DATA ONLY',' ')
	 CALL ABEND
      ENDIF

C        GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      IF(SLO+NLO-1 .GT. NLI) THEN
	 CALL XVMESSAGE(
     +		' NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE',' ')
	 CALL ABEND
      ENDIF
      IF(SSO+NSO-1 .GT. NSI) THEN
	 CALL XVMESSAGE(
     +		' NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE',' ')
	 CALL ABEND
      ENDIF

C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_FORMAT','HALF',
     &          'U_NL',NLO,'U_NS',NSO,'OPEN_ACT','SA','IO_ACT','SA',' ')

C        PROCESS PARAMETERS
      CALL XVPARM('AREA',IPARM,ICOUNT,IDEF,1800)
      IF(MOD(ICOUNT,5).NE.0) THEN
	 CALL XVMESSAGE(
     +		'NUMBER OF PARAMETERS NOT A MULTIPLE OF FIVE',' ')
	 CALL ABEND
      ENDIF


      LLO=SLO+NLO-1                 ! LAST LINE.

C  COPY THE INPUT IMAGE TO OUTPUT MAKING THE REQUESTED MODIFICATIONS.

      DO 100 LINE=SLO,LLO
         CALL XVREAD(IUNIT,IBUF,STAT,'LINE',LINE,' ')

         IF(ICOUNT.EQ.0) GO TO 55      ! IF NO MODIFICATIONS, JUST COPY.

         DO 50 J = 1,ICOUNT,5
C              CHECK FOR MODS TO THIS LINE
            IF(LINE.LT.IPARM(J).OR.LINE.GE.IPARM(J)+IPARM(J+2)) GO TO 50
            SS=IPARM(J+1)                ! STARTING PIXEL.
            IF(SS.LT.1) THEN
		CALL XVMESSAGE(' INVALID SS PARAMETER SPECIFIED',' ')
		CALL ABEND
	    ENDIF
            ES=IPARM(J+3)+SS-1           ! ENDING PIXEL.
            IF(ES.GT.NSI) THEN
		CALL XVMESSAGE(' INPUT SAMPLE SIZE EXCEEDED',' ')
		CALL ABEND
	    ENDIF
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

55       CALL XVWRIT(OUNIT,IBUF(SSO),STAT,'NSAMPS',NSO,' ')
100   CONTINUE

C        CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
C
      RETURN        
      END
$ VOKAGLEVE
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
PARM NL      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM AREA    TYPE=INTEGER  COUNT=(5:300) DEFAULT=(0,0,0,0,0) 
!
END-PROC
.TITLE
QSAR
.HELP
 PURPOSE:

Program QSAR is used to add or subtract values from the data numbers in
rectangular sections of an image.  For each rectangle, the user specifies
the size and position of the rectangle and the (positive or negative) value
to be added to the data numbers of points in that rectangle.  QSAR performs
the addition and writes the resulting image to an output file.  QSAR is 
primarily used to correct flaws in images or, in conjunction with program GEN,
to generate test images.
.PAGE
 EXECUTION:

The input image may either be byte or halfword data.  The position of the 
rectangles are specified relative to the full input image.

The maximum number of rectangles that can be specified for QSAR is limited
by the TAE executive.  As of this writing the limit is 60 rectangles.
If no rectangles are specified, QSAR copies the image without making any
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
      QSAR INP=a OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      QSAR INP=a OUT=b  SL=sl SS=ss NL=nl NS=ns optional parameters
      QSAR a b (sl,ss,nl,ns) optional parameters
      QSAR a b optional parameters

       Here 'a' represents the input image file name, and
       'b' represents the output image file name.
.PAGE
EXAMPLES

1.    GEN A NL=250 NS=250 IVAL=0 LINC=0 SINC=0
      QSAR INP=A OUT=B AREA=( 1   1  250   50   50     +
                              1  51  250   50  100     +
                              1 101  250   50  150     +
                              1 151  250   50  200     +
                              1 201  250   50  250       )

      In this example the output file B is a 250x250 byte image
      consisting of five vertical strips of different shades of gray.
.PAGE
2.    GEN QSARE NL=10 NS=10 'HALF
      QSAR INP=QSARE OUT=QSAREO SIZE=(2,3,8,7) FORMAT=HALF AREA=(4,6,3,1,100)

      In this example the output file QSAREO is a halfword image that has
      eight lines with seven pixels per line.  Data numbers for a three line
      by 1 pixel rectangle are incremented by 100.
.PAGE
RESTRICTIONS
1. The input and output images must be byte or halfword data.
2. The maximum number of pixels per line is 64000.
3. The maximum number of rectangles that can be specified is 60, as of this
   writing.

 WRITTEN BY:             Steve Pohorsky              24 Oct 1983

 COGNIZANT PROGRAMMER:   Joel Mosher                 29 May 1980

 REVISION:               1                           29 May 1980 

.LEVEL1
.VARIABLE INP
Input file name
.VARIABLE OUT
Output file name
.VARIABLE SIZE
Standard Vicar size field:
Same as -  (SL,SS,NL,NS)
By default, the entire input
image is used if these
parameters are not entered.
.VARIABLE SL
Starting line number
.VARIABLE SS
Starting sample number
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
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

The maximum number of rectangles that can be specified for QSAR is limited
by the TAE executive.  As of this writing the limit is 60 rectangles.
If no rectangles are specified, QSAR copies the image without making any
changes.  
.END
$ Return
$!#############################################################################
$Imake_File:
$ create qsar.imake
#define  PROGRAM   qsar

#define MODULE_LIST qsar.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
