$!****************************************************************************
$!
$! Build proc for MIPL module simplify
$! VPACK Version 1.8, Wednesday, May 08, 1996, 18:07:57
$!
$! Execute by entering:		$ @simplify
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
$ write sys$output "*** module simplify ***"
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
$ write sys$output "Invalid argument given to simplify.com file -- ", primary
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
$   if F$SEARCH("simplify.imake") .nes. ""
$   then
$      vimake simplify
$      purge simplify.bld
$   else
$      if F$SEARCH("simplify.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake simplify
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @simplify.bld "STD"
$   else
$      @simplify.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create simplify.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack simplify.com -
	-s simplify.f -
	-p simplify.pdf -
	-i simplify.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create simplify.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     
C  REVISION HISTORY
C    05-96  REA  PORTED TO UNIX
C    07-85  JHR  CONVERTED TO VICAR2
C    11-83  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS CLEANUP.
C    11-83  SP   MODIFIED SO THAT RDCHEK AND WRCHEK USED FOR I/O ERROR MESSAGES
C    11-83  SP   CORRECTED SPELLING OF QVAL TO QVALUE IN STACKA CALL.
C    11-83  SP   CORRECTED LENGTHS BY A FACTOR OF TWO IN FIVE ITLA CALLS.
C    11-83  SP   CHANGED TO SKIP CHECK OF   MAX .GT. IHIST(OUT(J))  IF
C                OUT(J) IS 0.
C    12-83  SP   ADDED CHECK FOR WINDOW SIZE BEING ODD.
C     2-82  REA  NEW VERSION - FASTER, WINDOW SIZE RESTRICTION REMOVED
C     1-80  SZF  INITIAL RELEASE
C
C  PURPOSE: ELIMINATE ISOLATED PIXELS IN A CLASSIFIED IMAGE.
C  USER PARAMETERS:
C
C  WINDOW=N      THE ODD INTEGER N SPECIFIES A WINDOW SIZE.  DEFAULT IS 3
C  THRESHLD=M    IF THE MAJORITY CLASS IN A WINDOW IS LARGER THAN M
C                THEN THE CENTER PIXEL MAY BE CHANGED TO THE MAJORITY
C                CLASS
C  NCLASS,K      THE INTEGER K SPECIFIES THE NUMBER OF CLASSES IN THE
C                IMAGE.  THE CLASSES MUST BE NUMBERED FROM 1 TO K.
C  REPLACE       THIS KEYWORD CAUSES ALL 0 PIXELS TO BE REPLACED BY THE
C                VALUE IN THE WINDOW ABOUT THAT PIXEL.
C  VALUE,Q       THIS CHECKS FOR REPLACEMENT  ONLY IF THE CENTRAL DN
C                IS EQUAL TO Q
C
      EXTERNAL WORK
C
      INTEGER*4 OUNIT,SL,SS,LEN,LIMIT
      LOGICAL*4 XVPTST
      LOGICAL QZERO,QVALUE
      CHARACTER*8 FORMAT
C							     open input data set
      CALL XVUNIT(IUNIT,'INP',1,ISTAT,' ')
      CALL XVOPEN(IUNIT,ISTAT,'U_FORMAT','HALF','OPEN_ACT','SA',
     +            'IO_ACT','SA',' ')
C						       get data format and check
      CALL XVGET(IUNIT,ISTAT,'FORMAT',FORMAT,' ')
      IF(FORMAT.NE.'BYTE') THEN
	 CALL XVMESSAGE('SIMPLIFY accepts BYTE data only',' ')
	 CALL ABEND
      ENDIF
C
C						  get size information and check
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      IF (SL+NL-1 .GT. NLI) THEN
	 CALL XVMESSAGE(
     +		' number of lines requested exceeds input size',' ')
	 CALL ABEND
      ENDIF
      IF (SS+NS-1 .GT. NSI) THEN
	 CALL XVMESSAGE(
     +		  ' number of samples requested exceeds input size',' ')
	 CALL ABEND
      ENDIF
C
C							    open output data set
      CALL XVUNIT(OUNIT,'OUT',1,ISTAT,' ')
      CALL XVOPEN(OUNIT,ISTAT,'OP','WRITE','U_FORMAT','HALF',' ')
C
C							      process parameters
      CALL XVPARM('WINDOW',IWIND,ICOUNT,IDEF,1)
      IF (MOD(IWIND,2) .EQ. 0)  THEN
	 CALL XVMESSAGE(' error:window size not an odd number',' ')
	 CALL ABEND
      ENDIF
C
      CALL XVPARM('THRESHLD',LIMIT,ICOUNT,IDEF,1)
      CALL XVPARM('NCLASS',NCLASS,ICOUNT,IDEF,1)
      QZERO = XVPTST('REPLACE')
      CALL XVPARM('VALUE',IVAL,ICOUNT,IDEF,1)
      QVALUE = IDEF.EQ.0
C
      IREC = SL
C
C  Call special library subroutine STACKA to allocate the necessary buffers
C  and to call subroutine WORK. (We allocate IWIND line buffers together in a
C  two-dimensional array with LEN*IWIND halfwords.)
C
      LEN = NS+IWIND
      I = 2*LEN*IWIND
      J = 2*NS
      K = 1024
      CALL STACKA(19, WORK, 3, I, J, K, IUNIT, OUNIT, IREC, SS, NL, NS,
     &            NCLASS, LIMIT, IWIND, IVAL, LEN, QZERO, QVALUE)
C
C								 close data sets
      CALL XVCLOSE(IUNIT,ISTAT,' ')
      CALL XVCLOSE(OUNIT,ISTAT,' ')
      RETURN
      END
C*******************************************************************************
      SUBROUTINE WORK(IN,II,OUT,JJ,IHIST,KK,IUNIT,OUNIT,
     &                IREC, SS, NL, NS, NCLASS, LIMIT,
     &                IWIND, IVAL, LEN, QZERO, QVALUE)
C
C  INPUT PARAMETERS 
C      IN(I,IWIND)       - LINE BUFFERS FOR IWIND LINES WHERE IWIND IS THE
C       array              WINDOW SIZE.  THE PIXEL INDEX GOES FROM 1 TO LEN.
C                          THE DATA FROM A LINE OF THE INPUT IMAGE
C                          BEGINS WITH AT IN(2+IWIND/2,L) TO PROVIDE A BORDER
C                          OF ZERO DNS AROUND THE IMAGE.
C      II                - NUMBER OF BYTES ALLOCATED BY STACKA FOR IN.
C      OUT(J) array      - line buffer for output image.
C      JJ                - NUMBER OF BYTES ALLOCATED BY STACKA FOR OUT.
C      IHIST(NUM) array  - BUFFER FOR HISTOGRAM.
C      KK                - NUMBER OF BYTES ALLOCATED BY STACKA FOR IHIST.
C      IREC              - RECORD NUMBER IN INPUT FILE OF STARTING LINE.
C      SS               - STARTING SAMPLE.
C      NL                - NUMBER OF LINES.
C      NS                - NUMBER OF SAMPLES.
C      NCLASS            - VALUE OF NCLASS PARAMETER.
C      LIMIT             - VALUE OF THRESHLD PARAMETER.
C      IWIND             - VALUE OF WINDOW SIZE PARAMETER.
C      IVAL              - VALUE OF VALUE PARAMETER.
C      LEN               - HORIZONTAL DIMENSION OF IN ARRAY.
C      QZERO             - TRUE IF REPLACE PARAMETER SELECTED.
C      QVALUE            - TRUE IF VALUE PARAMETER SELECTED.
C
      CHARACTER*80 MSG
      INTEGER*4 OUNIT,SS
      INTEGER*4 IHIST(256)
      INTEGER*2 IN(LEN,IWIND), OUT(NS)
      LOGICAL   QZERO, QVALUE
C						  set up keys and looping limits
      N = 0
      KEYL = 0
      M = IWIND/2
      M1 = M+1
      M2 = M+2
      NEND = NL-M
      NREPLACED = 0
C
C     Read in the lines needed to fill the initial window.  The top part of
C     the window is left with zeros since it is outside the boundary of the
C     input image.
C
      CALL ITLA(0,IN,2*LEN*IWIND)
C
      DO I=1,M
          N = N+1
          CALL XVREAD(IUNIT,IN(M2,N),ISTAT,'LINE',IREC,'SAMP',SS,
     +                'NSAMPS',NS,' ')
          IREC = IREC + 1
          CALL ITLA(0,IN(1,N),M1*2)
          CALL ITLA(0,IN(NS+M2,N),M*2)
      END DO
C
C     Main loop, looping through each output line.  Since the algorithm does
C     not depend on the geometry of pixels in the window, we do not have to
C     keep the lines of the window in any order.  We just read the next line
C     into the oldest buffer.  KEYL is the line index for the in array of the
C     center line.
C
      DO I=1,NL
          N = N+1
          IF(N.GT.IWIND) N=1
          KEYL = KEYL+1
          IF(KEYL.GT.IWIND) KEYL=1
C
C	      read in a line of input; if at the image bottom, enter a line of 0
C
          IF(I.LE.NEND) THEN
              CALL XVREAD(IUNIT,IN(M2,N),ISTAT,'LINE',IREC,'SAMP',SS,
     +                    'NSAMPS',NS,' ')
              IREC = IREC + 1
              CALL ITLA(0,IN(1,N),M1*2)
              CALL ITLA(0,IN(NS+M2,N),M*2)
          ELSE
              CALL ITLA(0,IN(1,N),LEN*2)
          END IF
C					 set up the initial window for this line
          KEY2 = IWIND
          KEY3 = 0
          CALL ZIA(IHIST,256)
          DO J=1,IWIND
              DO K=M2,KEY2
                  NUM = IN(K,J)
                  IF(NUM.NE.0) IHIST(NUM)=IHIST(NUM)+1
              END DO
          END DO
C					     loop through for each output sample
          CALL MVE(2,NS,IN(M2,KEYL),OUT,1,1)
          DO J=1,NS
              KEY2 = KEY2+1
              KEY3 = KEY3+1
C
C     Update the histogram:  when we move the window to the right, we lose
C     the leftmost column of the previous window and gain a column on the 
C     right.
C
              DO K=1,IWIND
                  NUM = IN(KEY2,K)
                  IF(NUM.NE.0) IHIST(NUM)=IHIST(NUM)+1
                  NUM = IN(KEY3,K)
                  IF(NUM.NE.0) IHIST(NUM)=IHIST(NUM)-1
              END DO
C							    test for replacement
              IF (OUT(J).EQ.IVAL .OR. .NOT.QVALUE) THEN
                  CALL MINMAX(4,NCLASS,IHIST,MIN,MAX,IMIN,IMAX)
                  IF(QZERO.AND.OUT(J).EQ.0) THEN
                      IF (MAX.NE.0) THEN
                          OUT(J) = IMAX
                          NREPLACED = NREPLACED + 1
                      END IF
                  ELSE
                      IF (MAX.GE.LIMIT .AND.
     +                    (MAX.GT.IHIST(OUT(J)) .OR. OUT(J).EQ.0)) THEN
                          OUT(J) = IMAX
                          NREPLACED = NREPLACED + 1
                      END IF
                  END IF
              END IF
          END DO
          CALL XVWRIT(OUNIT,OUT,ISTAT,'NSAMPS',NS,' ')
      END DO
      WRITE (MSG,100) NREPLACED
  100 FORMAT(I10,' values replaced')
      CALL XVMESSAGE(MSG,' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create simplify.pdf
process help=*
!  FILE NAMES      
!
PARM INP     TYPE=STRING   COUNT=1
PARM OUT     TYPE=STRING   COUNT=1
!
PARM SIZE    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SS      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM NL      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=1       DEFAULT=0
!
PARM NCLASS  TYPE=INTEGER  COUNT=1       DEFAULT=255        VALID=(1:255)
PARM THRESHLD TYPE=INTEGER COUNT=1       DEFAULT=4          VALID=(1:99999)
PARM WINDOW  TYPE=INTEGER  COUNT=1       DEFAULT=3          VALID=(3:99999)
PARM REPLACE TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--         VALID=REPLACE
PARM VALUE   TYPE=INTEGER  COUNT=1       DEFAULT=0          VALID=(0:255)
!
END-PROC
.TITLE
SIMPLIFY
.HELP
 PURPOSE:

SIMPLIFY is a VICAR applications program used to remove high frequency
information (noise) components from digital images.  It is usually used
to remove visually distracting detail from classified or stratified images
such as images derived from the operation of VICAR program FASTCLAS.  Such
images usually are composed of a number of regions, where each pixel in a
region has the same DN except possibly for some isolated noise pixels.
SIMPLIFY changes the DNs of these isolated noise pixels to match the DN of
the surrounding pixels and writes the resulting image to an output file.
.PAGE
 EXECUTION:

The DNs for the regions in the input image are assumed to lie in the range
from 1 to some upper limit specified by the user through the NCLASS parameter.
No assumptions are made on the DNs of noise pixels.  Pixels with a DN of 0
are considered to be unclassified (not associated with any region).

In order to determine which DNs to change, SIMPLIFY applies a test to each
pixel in the input image.  The test involves finding the most frequently
occurring DN in a square window centered around the pixel being tested.
The exact form of the test depends on the various parameters specified by the
user, but in order to provide an overview, the test for the case where neither
the REPLACE nor the VALUE option is used is described below.  Much of the
description also applies to when REPLACE or VALUE is used.  (See under
REPLACE and VALUE.)

For the sake of determining the most frequently occurring DN, only DNs from 1
to the value of the NCLASS parameter are considered.  In case of a tie in
determining the most frequent DN, the lowest of the DNs in the tie is used.
Since the window is centered around the pixel being tested, we will refer to
the pixel being tested as the center pixel and will refer to its DN as the
center DN. When the center DN is nonzero it is changed to the most frequent DN
if the most frequent DN occurs more times than the center DN and the number of
times the most frequent DN occurs is greater than or equal to the threshold
(THRESHLD) parameter.  (This insures that a nonzero DN is not changed if no DN
occurs a substantial number of times in the window or if the center DN occurs
just as many times as the most frequent DN.)  When the center DN is zero it is
changed to the most frequent DN if the number of times the most frequent DN
occurs is greater than or equal to the threshold parameter.  When a DN is
changed, the change is not included in the subsequent windows. 
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      SIMPLIFY INP=a OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      SIMPLIFY INP=a OUT=b  SL=sl SS=ss NL=nl NS=ns optional parameters
      SIMPLIFY a b (sl,ss,nl,ns) optional parameters
      SIMPLIFY a b optional parameters

       Here 'a' represents the input image file name, and
       'b' represents the output image file name.
.PAGE
EXAMPLE

      SIMPLIFY INP=A OUT=B NCLASS=9 THRESHLD=8 WINDOW=5 'REPLACE

      In this example a five by five window is used.  The most frequent DN
      must occur at least 8 out of 25 times in a window to be used to replace
      a center DN.  Unclassified pixels will be replaced where possible.


RESTRICTIONS
1. The input and output images must be byte data.
.PAGE
 OPERATION:

The operation of SIMPLIFY is similar to the operation of any box-filtering
routine with the exception that surface smoothing DOES NOT take place.  In a
pictorial sense, a window is passed over each pixel in the input image.  For
each pixel and its surrounding neighborhood, defined by the size of the window,
a histogram is accumulated to identify the most most frequently occurring DN
value.  When the center DN is nonzero it is changed to the most frequent DN 
if the most frequent DN occurs more times than the center DN and the number of
times the most frequent DN occurs is greater than or equal to the threshold 
parameter.  When the center DN is zero it is changed to the most frequent 
DN if the number of times the most frequent DN occurs is greater than or 
equal to the threshold parameter.  

In order to accommodate the edges of the image which normally could not be
simplified due to edge overlap of the window, the pixels outside the image 
are considered unclassified (DN=0).  This action eliminates the need for 
special edge processing feature.

The following window operations will demonstrate the operation of SIMPLIFY
on various windows.

Example 1:  specifying THRESHLD=4

                    5   5   5                   5   5   5 

            INPUT:  3   2   5          OUTPUT:  3   5   5

                    4   3   3                   4   3   3

Example 2:  specifying THRESHLD=4

                    5   5   5

            INPUT:  3   2   2          OUTPUT:  no change

                    2   2   2 

Example 3:  specifying THRESHLD=4 and REPLACE

                    0   0   0                   0   0   0

            INPUT:  0   0   0          OUTPUT:  0   2   0

                    2   2   1                   2   2   1


 WRITTEN BY:             Steve Pohorsky               1 Dec 1983

 COGNIZANT PROGRAMMER:   A. L. Zobrist               14 Sept. 1984

 REVISION:               2                           21 Feb 1982

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
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE NCLASS
Largest DN other than noise
.VARIABLE THRESHLD
Threshold for number of
occurrences of most frequent
DN in window.
.VARIABLE WINDOW
Window size
.VARIABLE REPLACE
Enter to replace
unclassified pixels where
possible.
.VARIABLE VALUE
DN value when only one value
is to be checked for replacement.
.LEVEL2
.VARIABLE NCLASS
The NCLASS value gives an upper limit for DNs for the regions of the input
image.  Only DNs from 1 to NCLASS are considered in finding the most frequent
DN in a window.  The NCLASS value should be set as low as the input image 
allows in order to reduce execution time.  NCLASS must be in the range 1 to
255.  The default value is 255.
.VARIABLE THRESHLD
The THRESHLD threshold value limits the cases where a DN value will be changed.
Except for the case of unclassified DNs when the REPLACE option is selected,
a DN will not be changed if the most frequent DN does not occur at least the
THRESHLD number of times in the window.  The default THRESHLD value is 4.
.VARIABLE WINDOW
The window size gives the linear size in pixels of the moving window used in
testing DNs for changing.  The window size must be an odd integer greater than
or equal to 3.  A size of 3 means a 3x3 window with eight neighbor pixels
surrounding the center pixel.  The default size is 3.
.VARIABLE REPLACE
When the REPLACE option is specified, all pixels with a DN of zero will be
replaced with the most frequent DN (in the window) if the window around the 
pixel contains at least one DN in the range 1 to the NCLASS value. 
No replacement is the default.
.VARIABLE VALUE
This optional parameter limits the cases where a DN value will be changed.
If this parameter is entered, only DNs equal to the VALUE parameter can be
changed.  DN values equal to the VALUE parameter are changed or left the
same according to the usual rules based on the THRESHLD and REPLACE
parameters.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create simplify.imake
#define  PROGRAM   simplify

#define MODULE_LIST simplify.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
