$!****************************************************************************
$!
$! Build proc for MIPL module entropy
$! VPACK Version 1.8, Thursday, July 10, 1997, 17:56:44
$!
$! Execute by entering:		$ @entropy
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
$ write sys$output "*** module entropy ***"
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
$ write sys$output "Invalid argument given to entropy.com file -- ", primary
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
$   if F$SEARCH("entropy.imake") .nes. ""
$   then
$      vimake entropy
$      purge entropy.bld
$   else
$      if F$SEARCH("entropy.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake entropy
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @entropy.bld "STD"
$   else
$      @entropy.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create entropy.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack entropy.com -
	-s entropy.f -
	-i entropy.imake -
	-p entropy.pdf -
	-t tstentropy.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create entropy.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

         IMPLICIT NONE

         INTEGER*2 BUF(10000)
         INTEGER HIST(-4095:4095)
         INTEGER PARB(500),IND,ICNT,IDEF
         INTEGER S_IND,E_IND,NLI,NSI,SL,SS,NL,NS,JST,ISPIKE
         INTEGER IAREA(4),IST,S_PLUS_E,INPUT,IEL,LINE,ZERO,NPRP,J 

         CHARACTER*4   FORMAT
         CHARACTER*80  PROP
         CHARACTER*160 MSG
         CHARACTER*255 DISTFILE

         REAL*4 LOG2,ENTRPY,XSUM,XSUM2,AREA,XABORT,XADD,PJ,XMEAN
         REAL*4 SIGMADELDN

         LOGICAL XVPTST,DIST_FLAG

         MSG = ' '
         DIST_FLAG = .FALSE.

C        max values for histogram
         S_IND = -4095
         E_IND = 4095
         S_PLUS_E = E_IND + ABS(S_IND) + 1

         CALL IFMESSAGE ('ENTROPY version April 22, 2000')
         CALL XVUNIT(INPUT,'INP',1,IND,' ')

C     check to see whether to put the entropy value into the frame's label.
         IF (XVPTST('LABEL')) THEN
            CALL XVOPEN(INPUT,ind,'OPEN_ACT','SA','IO_ACT','SA',
     &               'U_FORMAT','HALF','OP','UPDATE',' ')
         ELSE
            CALL XVOPEN(INPUT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &               'U_FORMAT','HALF',' ')
         ENDIF

C        check for output normalized difference histogram file name.
         CALL XVPARM('DISTFILE',DISTFILE,ICNT,IDEF,' ')
         IF (ICNT .GE. 1) THEN
            OPEN (13, FILE=DISTFILE, STATUS='UNKNOWN',
     &            IOSTAT=JST,ERR=994)
            DIST_FLAG = .TRUE.
         ENDIF
 
         CALL XVGET(INPUT,IND,'NL',NLI,'NS',NSI,' ')
         CALL XVGET(INPUT,IND,'FORMAT',FORMAT,' ')
         IF ((FORMAT .NE. 'BYTE') .AND. (FORMAT .NE.'HALF')) GOTO 992

         CALL XVPARM('AREA',IAREA,ICNT,IDEF,' ')
         IF(ICNT .EQ. 0)THEN
            SL=1
            SS=1
            NL=NLI
            NS=NSI
         ELSE
            SL=IAREA(1)
            SS=IAREA(2)
            NL=IAREA(3)
            NS=IAREA(4)
         ENDIF

         IF((SL .GT. NLI) .OR. (SL+NL-1 .GT. NLI) .OR.
     &      (SS .GT. NSI) .OR. (SS+NS-1 .GT. NSI)) GOTO 995

         CALL ZIA(HIST,S_PLUS_E)

         XSUM = 0.0
         XSUM2 = 0.0
         IEL = SL + NL - 1
         DO LINE=SL, IEL
            CALL XVREAD(INPUT,BUF,IND,'LINE',LINE,'SAMP',SS,
     &                  'NSAMPS',NS,' ')
            CALL DIFFHIST(BUF,NS,HIST,XSUM,XSUM2,S_IND,E_IND,*993)
         ENDDO

         ZERO = 0
         ISPIKE = 1
         IF (XVPTST('ZEROES')) ZERO = 1
         IF (XVPTST('PHIST')) THEN 
            CALL PHIST(HIST,S_PLUS_E,S_IND,E_IND,ISPIKE,ZERO)
         ELSE IF (ZERO .EQ. 1) THEN
            WRITE (MSG,'(A30,A32)')
     &         'The ZEROES keyword is ignored,',
     &         ' because PHIST is not specified.'
            CALL XVMESSAGE (MSG,' ')
         END IF

C      ....Use the formula:          H  =  - >  p log p    to find entropy. 
C                                            -   j   2 j
         LOG2 = ALOG(2.)
         AREA = NL*(NS-1)
         ENTRPY = 0.0

         DO J=S_IND, E_IND
            PJ = HIST(J)/AREA
            IF (HIST(J).GT.0.) THEN
C               PJ = HIST(J)/AREA
               ENTRPY = ENTRPY - PJ*ALOG(PJ)/LOG2
            ENDIF
            IF (DIST_FLAG) THEN
               WRITE (13, 1001) J, PJ
1001           FORMAT (1x, I5, 3x, F7.5)
            ENDIF
         ENDDO

         IF (DIST_FLAG) CLOSE (13)

         XMEAN = XSUM/AREA
         SIGMADELDN=SQRT((XSUM2-AREA*XMEAN**2)/(AREA-1)) 
 
         WRITE(MSG,'(A9,F8.5,2x,A18,F9.5,2x,A15,I8)')
     &      'ENTROPY = ',ENTRPY,' SIGMA_DELTA_DN = ',SIGMADELDN,
     &      ' #_OF_PIXELS = ',IFIX(AREA)
         CALL XVMESSAGE(MSG,' ')

C        output value to TCL local variable
         CALL XQINI(PARB,500,XABORT)
         CALL XQREAL(PARB,'VALUE',1,ENTRPY,XADD,IST)
         CALL XVQOUT(PARB,IST)

         IF (XVPTST('NOLABEL')) RETURN

C        add a label item
C        update label only if AREA is equal to size of the image
         IF ((NL .NE. NLI) .OR. (NS .NE. NSI) .OR. 
     &       (SL .NE. 1) .OR. (SS .NE.1)) THEN
            CALL XVMESSAGE
     &      ('AREA not equal to image size.  ITEM NOT ADDED',' ')
            RETURN
         ENDIF

         CALL XLADD(INPUT,'HISTORY','ENTROPY',ENTRPY,IST,
     &         'MODE','REPLACE','FORMAT','REAL',' ')
         IF (IST .NE. 1) GOTO 991
         CALL XVMESSAGE ('ENTROPY ADDED TO HISTORY LABEL',' ')

         RETURN

991	 CALL XVMESSAGE
     &      ('***Error writing history label item',' ')
	 GOTO 999
992	 CALL XVMESSAGE
     &      ('***Input image must be byte or halfword format',' ')
	 GOTO 999
993	 CALL XVMESSAGE ('***Invalid DN values',' ')
         GOTO 999
994      CALL XVMESSAGE('***Error opening distribution output file',' ')
         GOTO 999
995      CALL XVMESSAGE ('***Selected area is outside of image',' ')
999	 CALL XVMESSAGE ('***Entropy task cancelled',' ')
         CALL ABEND
      END 


C Compute difference histogram (HIST).
      SUBROUTINE DIFFHIST(BUF,NPTS,HIST,XSUM,XSUM2,S_IND,E_IND,*)

         INTEGER S_IND,E_IND,NPTS,J,IDEL,I0,I1
         INTEGER HIST(S_IND:E_IND)
         INTEGER*2 BUF(NPTS)
         
         REAL*4 XSUM, XSUM2

	 I0 = BUF(1)
         IF ((I0 .GT. E_IND) .OR. (I0 .LT. 0)) GOTO 5

         DO J=2,NPTS  
            I1 = BUF(J)
            IF ((I1 .GT. E_IND) .OR. (I1 .LT. 0)) GOTO 5
            IDEL = I1 - I0
            XSUM = XSUM + IDEL
            XSUM2 = XSUM2 + IDEL * IDEL
            HIST(IDEL) = HIST(IDEL) + 1
            I0 = I1
         ENDDO
         RETURN
5        RETURN 1
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create entropy.imake
/***********************************************************************
 
                     IMAKE FILE FOR PROGRAM entropy
 
   To Create the build file give the command:
 
                $ vimake entropy                 (VMS)
   or
                % vimake entropy                 (Unix)
 
 
************************************************************************/
 
 
#define PROGRAM entropy
#define R2LIB
 
#define MODULE_LIST entropy.f
 
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
 
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create entropy.pdf
PROCESS HELP=*
LOCAL DUMMY TYPE=REAL
PARM INP    TYPE=STRING   COUNT=1                      
PARM AREA   TYPE=INTEGER  COUNT=0:4  VALID=(1:999999)   DEFAULT=--
PARM DISTFILE TYPE=STRING COUNT=0:1                     DEFAULT=--
PARM PHIST  TYPE=KEYWORD  COUNT=0:1  VALID=PHIST        DEFAULT=--
PARM ZEROES TYPE=KEYWORD  COUNT=0:1  VALID=ZEROES       DEFAULT=--
PARM MODE   TYPE=KEYWORD  VALID=(LABEL,NOLABEL) COUNT=0:1 DEFAULT=NOLABEL
PARM VALUE  TYPE=NAME DEFAULT=DUMMY

!# annot function="Vicar Data Compression"
!# annot keywords=(compute,"Output parameter","TAE variable",DN,SSI,ISS,+
!#   DCT,ICT,BARC,sl,ss,nl,ns,entropy)

END-PROC
.TITLE
Compute Image Entropy
.HELP
ENTROPY computes the entropy of an image providing a measure of the information
content.   It also calculates the standard deviation of the adjacent 
pixel-to-pixel DN differences.  The program can be run on any VICAR image 
and has current application to Galileo SSI and Cassini ISS.  The results can be
used as an aid in generating predictions for the effectiveness of the ICT,
DCT, BARC and other compression algorithms.
 
References:
  1) Gonzalez and Wintz, "Digital Image Processing", Addison-Wesley, 1977.
  2) JPL Publication 83-17, "Some Practical Universal Noiseless Coding
     Techniques, Part II", Robert F. Rice and Jun-Ji Lee, March 1, 1983.

.PAGE 
EXECUTION:
 
        ENTROPY  INP=PIC  [AREA=(SL,SS,NL,NS)] ...
                 [DISTFILE=DISTRIBUTION_FILE] ['PHIST] ['ZEROES]
                 ['LABEL] [VALUE=TCL_VAR]

where
        PIC is the input image.  PIC must be a byte image.  PIC may be
        arbitrarily large.
 
        (OPTIONAL) AREA specifies the area or region of the input image
        which is to be used as the basis of the entropy calculation.  If
        AREA is not specified, then the entropy of the entire image is
        used.  The area is specified as four integers:
                AREA=(SL,SS,NL,NS)
        where
                SL = starting line of the image area
                SS = starting sample of the image area
                NL = number of lines in the image area
                NS = number of samples in the image area
 
        (OPTIONAL) DISTRIBUTION_FILE is the output file name for the
        normalized difference histogram.
 
        (OPTIONAL) 'PHIST controls whether or not a histogram of the
        differences between adjacent pixels is printed to the screen.
        Note: This requires a 132 column display.
 
        (OPTIONAL) 'ZEROES will cause differences having zero frequencies to
        be included in the printout (normally these are excluded).
        Note:  This keyword does not affect the data written to
        DISTRIBUTION_FILE.

        (OPTIONAL) 'LABEL will update the VICAR label with the computed 
         entropy value.  This keyword only works with CASSINI images.

        (OPTIONAL) TCL_VAR stores the computed entropy value into a TCL 
        variable, which can be passed to another VICAR program.
 
 
RESTRICTIONS:
 
        PIC must be a byte or halfword format.
        Number of samples may not exceed 10000.
        Input DN must be in the range 0 - 4095
 
.page
OPERATION:
 
The entropy of an image is defined as follows (see ref. 2, pages 2-5).
 
        ENTROPY = - SIGMA(p log p )
                       j   j   2 j
where
        p  =  probability that two horizontally adjacent pixels have
         j    a difference j, where  -4095<j<4095.
and the summation is taken from j=-4095 to +4095.  Entropy is in units of
bits/pixel.

Specifying the optional output file, DISTFILE, will cause the program to
create a file containing the normalized pixel-to-pixel difference
distribution.  This file contains differences having zero frequencies,
i.e., it is complete.
 
If the keyword 'PHIST is specified, a histogram of the differences
between adjacent pixels is printed.  Specifying 'ZEROES will cause differences
having zero frequencies to be included in the printout (normally these are
excluded).  'ZEROES has no affect on the data written to the difference
distribution file.
 
Example:  ENTROPY  INP=input.dat AREA=(193,218,10,10)
 
Example:  ENTROPY  INP=input.dat (193,218,10,10) DISTFILE=diffhist.dat
 
Example:  ENTROPY  INP=input.dat DISTFILE=diffhist.dat  'PHIST  'ZEROES
 
Example:  ENTROPY  input.dat diffhist.dat  'PHIST  'ZEROES
 
 
.page
WRITTEN BY:  Mike Malcom, 15 July 1989
COGNIZANT PROGRAMMER:  Gary Yagi
4/20/2000 ...CCA.... made un-project-specific by putting results into
                     history label instead of property label.  Extended
                     field for reporting number of pixels used.  Fixed
                     help and test files.
7/10/97  ... TXH ... ported from VAX/VMS to UNIX and ALPHA/VMS.
                     Included changes made by CCA and combined 
                     features that were added on the MIPS' version
                     during software maintenance.  Now, the program will
                     support images from Galileo and Cassini.
  
REVISIONS: 1      ...cca...  added halfword capability (to 4095),
                             label add and output to TCL variable.

8/1/96 -- Added AREA parameter to allow specifying part of image to process.

7/9/96 -- Added calculation of SIGMA_DELTA_DN and optional capability to
write normalized difference histogram to a file.  Output also includes
number of points used in calculations.
 
3/4/94 -- Added automatic capability to print out the name of the input
file.  This is useful for identifying results when ENTROPY is used in a
large PDF file
 

.LEVEL1
.VARIABLE INP
The filename of the
input image.
.VARIABLE AREA
INTEGER-4 values specifying the
area to calculate the entropy of.
.VARIABLE DISTFILE
STRING-optional output file.
.VARIABLE PHIST
KEYWORD-print a histogram
of the differences
between adjacent pixels.
.VARIABLE ZEROES
KEYWORD-include zero frequencies
in the printout
.VARIABLE MODE
Indicates whether to put the
entropy value into the
frame's label. (LABEL,NOLABEL)
.VARIABLE VALUE
The name of the TCL
variable to contain the
entropy value.
.LEVEL2
.VARIABLE INP
STRING - The input image filename.  The file may be either byte or
halfword format.
.VARIABLE AREA
INTEGER - (SL,SS, NL,NS)-4 values specifying the area of the image to process.
        where
                SL = starting line of the image area
                SS = starting sample of the image area
                NL = number of lines in the image area
                NS = number of samples in the image area
.VARIABLE DISTFILE
STRING-optional output file name for the normalized difference histogram.
.VARIABLE PHIST
KEYWORD-controls whether or not a histogram of the differences between
adjacent pixels is printed to the screen.  Note: This requires a 132 column
display.
.VARIABLE ZEROES
KEYWORD-will cause differences having zero frequencies to be included in the
printout (normally these are excluded).  Note:  This keyword does not affect
the data written to the distribution file specified by the DISTFILE parameter.
.VARIABLE MODE
KEYWORD - Indicates whether to put the entropy value into the
frame's label.  Valid values are LABEL and NOLABEL.  NOLABEL is the
default.
.VARIABLE VALUE
REAL - The name of the TCL variable to contain the entropy value.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstentropy.pdf
procedure
refgbl $autousage
refgbl $echo
refgbl $syschar

body
local dir string

let $autousage="none"
let _onfail="continue"

let $echo="no"
write "Generic Image Test"
write ""
write "test image 1 - entropy should be 0"
let $echo="yes"
gen a.img 16 16
entropy a.img

let $echo="no"
write "test image 2 - entropy should be 1.0"
let $echo="yes"
gen b.img 16 16 mod=2
entropy b.img

let $echo="no"
write "test image 3 (byte) - entropy should be 0.811278"
let $echo="yes"
gen c.img 16 16 mod=4
entropy c.img 

let $echo="no"
write "test image 4 (half) - entropy should be 0.811278,"
write "local variable ent should be filled by that value"
let $echo="yes"
local out_ent real
cform c.img d.img  'half
entropy d.img value=out_ent
disp out_ent

let $echo="no"
write ""
write ""
write "GALILEO Test"
if ($syschar(1)="UNIX")
   let dir = "/project/test_work/testdata/mipl/gll/"
else
   let dir = "wms_test_work:[testdata.mipl.gll]"
end-if
write ""
write "Testing AREA parameter"
let $echo="yes"
entropy &"dir"s0349666345.2 area=(1,1,100,100)

let $echo="no"
write ""
write ">>> NOTE TESTERS <<<"
write "Testing PHIST keyword to generate normalized difference histogram"
write "The Histogram generated by the PHIST keyword has data range from "
write "-4095 to 4095, which would be too large to capture in a session log "
write "file.  The Tester is suggested to execute the following case to test "
write "the PHIST keyword."
write "-- RUN THE FOLLOWING --"
write "UNIX: VICAR>entropy /project/test_work/testdata/mipl/gll/s0249220600.1 +"
write "      VICAR>+ 'phist"
write "VMS : VICAR>entropy wms_test_work:[testdata.mipl.gll]s0249220600.1 +"
write "      VICAR>+ 'phist"
let $echo="yes"

let $echo="no"
write ""
write ">>> NOTE TESTERS <<<"
write "Testing ZEROES keyword"
write "-- RUN THE FOLLOWING --"
write "UNIX: VICAR>entropy /project/test_work/testdata/mipl/gll/s0249220600.1 +"
write "      VICAR>+ 'phist 'zeroes"
write "VMS : VICAR>entropy wms_test_work:[testdata.mipl.gll]s0249220600.1 +"
write "      VICAR>+ 'phist 'zeroes"
let $echo="yes"

let $echo="no"
write ""
write "Testing DISTFILE parameter to generate difference histogram file"
let $echo="yes"
entropy &"dir"s0349632122.1 distfile=gll.dist

let $echo="no"
write ""
write "Testing VALUE parameter"
entropy &"dir"s0349758952.18 value=out_ent
disp out_ent

let $echo="no"
write ""
write "Testing MODE parameter.  GLL label should not be updated."
let $echo="yes"
copy &"dir"s0349759013.8 s0349759013.8
label-list s0349759013.8
entropy s0349759013.8 'label
label-list s0349759013.8

let $echo="no"
write ""
write ""
write "CASSINI Test"
if ($syschar(1)="UNIX")
   let dir = "/project/test_work/testdata/cassini/iss/"
else
   let dir = "wms_test_work:[testdata.cassini.iss]"
end-if
write "Testing AREA parameter"
let $echo="yes"
entropy &"dir"sum2.15 area=(1,1,100,100)
 
let $echo="no"
write ""
write ">>> NOTE TO TESTERS <<<"
write "Testing PHIST keyword to generate normalized difference histogram"
write "--- RUN THE FOLLOWING ---"
write "UNIX: VICAR>entropy /project/test_work/testdata/cassini/iss/sum2.1 +"
write "      VICAR>+ 'phist"
write "VMS : VICAR>entropy wms_test_work:[testdata.cassini.iss]sum2.1 +"
write "      VICAR>+ 'phist"
let $echo="yes"
 
let $echo="no"
write ""
write ">>> NOTE TO TESTERS <<<"
write "Testing ZEROES keyword"
write "--- RUN THE FOLLOWING ---"
write "UNIX: VICAR>entropy /project/test_work/testdata/cassini/iss/sum2.2 +"
write "      VICAR>+ 'phist 'zeroes"
write "VMS : VICAR>entropy wms_test_work:[testdata.cassini.iss]sum2.2 +"
write "      VICAR>+ 'phist 'zeroes"
let $echo="yes"
 
let $echo="no"
write ""
write "Testing DISTFILE parameter to generate difference histogram file"
let $echo="yes"
entropy &"dir"sum2.3 distfile=cassini.dist

let $echo="no"
write ""
write "Testing VALUE parameter"
entropy &"dir"sum2.7 value=out_ent
disp out_ent
 
let $echo="no"
write ""
write "Testing MODE parameter.  History label should be updated."
let $echo="yes"
copy &"dir"sum2.38 sum2.38
label-list sum2.38
entropy sum2.38 'label
label-list sum2.38

let $echo="no"
if ($syschar(1)="UNIX")
   ush rm *.img
   ush rm sum2.38
   ush rm s0*.*
else
   dcl del *.img;*
   dcl del sum2.38;1
   dcl del s0*.*;*
end-if
 
end-proc

$ Return
$!#############################################################################
