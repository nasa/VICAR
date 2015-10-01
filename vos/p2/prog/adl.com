$!****************************************************************************
$!
$! Build proc for MIPL module adl
$! VPACK Version 1.9, Friday, February 17, 2006, 08:24:02
$!
$! Execute by entering:		$ @adl
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
$ write sys$output "*** module adl ***"
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
$ write sys$output "Invalid argument given to adl.com file -- ", primary
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
$   if F$SEARCH("adl.imake") .nes. ""
$   then
$      vimake adl
$      purge adl.bld
$   else
$      if F$SEARCH("adl.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake adl
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @adl.bld "STD"
$   else
$      @adl.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create adl.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack adl.com -mixed -
	-s adl.f -
	-i adl.imake -
	-p adl.pdf -
	-t tstadl.pdf new_3d_session.log old_3d_session.log tstadl.log_solos -
	   tstadl.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create adl.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      COMMON/C1/  LE,PE,S,A,BUF
      REAL*4      SL,SS,EL,ES
      INTEGER     NLO,NSO,NBO,NLI,NSI,NBI,ICOUNT,IDEF
      INTEGER     SLO,SSO,SBO,ADJ,OUNIT,STAT,IPARM(150)
      INTEGER     BANDOUT, LINEOUT, BAND
      INTEGER*2   PE(12000),S(12000),A(12000),BUF(10000),LE(10000)
      CHARACTER*8 FORMAT
      CHARACTER*3 ORGIN

C        SET DEFAULTS AND INITIALIZE
      NPTS   = 0
      FORMAT = ' '
      SL    = 0.0
      SS    = 0.0
      EL    = 0.0
      ES    = 0.0
      SLO   = 0
      SSO   = 0
      ADJ   = 0
      NLO   = 0
      NSO   = 0
      NLI   = 0
      NSI   = 0 
      OUNIT = 0 
      IUNIT = 0
      STAT  = 0
      ICOUNT= 0
      IDEF  = 0 
      CALL  ZIA(IPARM,150)
      CALL  ZIA(LE,10000/2)
      CALL  ZIA(PE,12000/2)
      CALL  ZIA(S ,12000/2)
      CALL  ZIA(A ,12000/2)
      CALL  ZIA(BUF,10000/2)

      call ifmessage ('ADL version 16-Feb-06')
      call xveaction ('SA', ' ')
C
C          OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')
C
C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      IF(FORMAT.NE.'BYTE') THEN
         CALL mabend('ADL HANDLES BYTE DATA ONLY')
      END IF

c     Check organization of image, prohibit BIP
      CALL XVGET(IUNIT,STAT,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')      

C
C        GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SLO,SSO,NLO,NSO,NLI,NSI)
      IF(NLI.GT.10000.OR.NSI.GT.10000) THEN
         CALL mabend('MAXIMUM IMAGE SIZE IS 10000 BY 10000')
      END IF
      IF(SLO+NLO-1 .GT. NLI) THEN
         CALL mabend('NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE')
      END IF
      IF(SSO+NSO-1 .GT. NSI) THEN
         CALL mabend('NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE')
      END IF

      CALL XVBANDS(SBO,NBO,NBI)

      IF ( SBO .GT. NBI ) CALL MABEND(
     +  'START BAND INDEX EXCEEDS INPUT SIZE')
      IF (SBO+NBO-1 .GT. NBI) THEN 
         CALL XVMESSAGE('***Number of bands truncated', ' ')
         NBO = NBI + 1 - SBO
      ENDIF
      

C
C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_FORMAT','HALF',
     &            'U_NL',NLO,'U_NS',NSO,'U_NB',NBO,' ')
C
C        PROCESS PARAMETERS
      CALL XVPARM('ADD',IPARM,ICOUNT,IDEF,150)
      DO J=1,ICOUNT,5
         ADJ = IPARM(J)
         SL = IPARM(J+1)
         SS = IPARM(J+2)
         EL = IPARM(J+3)
         ES = IPARM(J+4)
C           MORE OR LESS VERTICAL LINES
         IF(ABS(EL-SL).GE.ABS(ES-SS)) THEN
               IF(SL.NE.EL) THEN
                       SLOPE=(ES-SS)/(EL-SL)
                   ELSE
                       SLOPE = 0.0
               END IF
               OFFSET = SS-SLOPE*SL
               ISTART = NINT(MIN(SL,EL))
               IEND = NINT(MAX(SL,EL))
               DO I=ISTART,IEND
                   IL = I
                   IS = NINT(SLOPE*IL+OFFSET)
                   IF(IS.GE.SSO.AND.IS.LE.SSO+NSO-1.AND.
     &                IL.GE.SLO.AND.IL.LE.SLO+NLO-1)
     &                CALL ENTER(IL,IS,ADJ,NPTS)
               END DO
C
C             MORE OR LESS HORIZONTAL LINES
           ELSE
               SLOPE = (EL-SL)/(ES-SS)
               OFFSET = SL-SLOPE*SS
               ISTART = NINT(MIN(SS,ES))
               IEND = NINT(MAX(SS,ES))
               DO I=ISTART,IEND
                   IS = I
                   IL = NINT(SLOPE*IS+OFFSET)
                   IF(IS.GE.SSO.AND.IS.LE.SSO+NSO-1.AND.
     &                IL.GE.SLO.AND.IL.LE.SLO+NLO-1)
     &                CALL ENTER(IL,IS,ADJ,NPTS)
               END DO
         END IF
      END DO
C
      BANDOUT = 0
      DO BAND = SBO,SBO+NBO-1
         LINEOUT = 0
         BANDOUT = BANDOUT + 1
C          READ EACH LINE
        DO  LINE = SLO,SLO+NLO-1
           LINEOUT = LINEOUT + 1
          CALL XVREAD(IUNIT,BUF,STAT,'LINE',LINE,'BAND',BAND,' ')
          N=LE(LINE)
C          CHANGE PIXELS WHERE NECESSARY
          DO WHILE (N.NE.0)
              INT = BUF(S(N))+A(N)
              IF(INT.LT.0) INT=0
              IF(INT.GT.255) INT=255
              BUF(S(N))=INT
              N=PE(N)
          END DO
C            WRITE OUTPUT LINE
          CALL XVWRIT(OUNIT,BUF(SSO),STAT,'NSAMPS',NSO,
     +         'BAND', BANDOUT, 'LINE', LINEOUT, ' ')
        ENDDO
      END DO
C
C        CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
C
      RETURN
      END
C
C
C *********************************************************************
C
C
      SUBROUTINE ENTER(IL,IS,IADJ,N)
      COMMON/C1/  LE,PE,S,A,BUF
      INTEGER*2   PE(12000),S(12000),A(12000),BUF(10000),LE(10000)
C                            ENTER THE PIXEL INTO THE COMMON ARRAYS
C                                LE(I) IS THE INDEX TO THE LAST ENTRY
C                                      IN THE PE, S, AND A ARRAYS
C                                      THAT RELATES TO LINE 'I'
C                                PE(J) IS THE INDEX TO THE PREVIOUS
C                                      ENTRY IN THE PE, S AND A ARRAYS
C                                      THAT RELATES TO THE SAME INPUT
C                                      LINE
C                                S(K)  IS THE SAMPLE TO BE CHANGED
C                                A(K)  IS THE AMOUNT OF ADJUSTMENT
      N=N+1
      IF(N.GT.12000) THEN
          CALL mabend('ADL IS UNABLE TO CHANGE MORE THAN 12000 PIXELS')
      END IF

      PE(N) = LE(IL)
      LE(IL)=N
      S(N)=IS
      A(N)=IADJ
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create adl.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM adl
   To Create the build file give the command:
		$ vimake adl			(VMS)
   or
		% vimake adl			(Unix)
************************************************************************/
#define PROGRAM	adl
#define R2LIB

#define MODULE_LIST adl.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
/* #define INCLUDE_LIST adl.fin */
/* #define FTNINC_LIST fortport */

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create adl.pdf
process help=*
PARM INP TYPE=STRING COUNT=1
PARM OUT TYPE=STRING COUNT=1
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER COUNT=1 DEFAULT=1
PARM SS TYPE=INTEGER COUNT=1 DEFAULT=1
PARM SB TYPE=INTEGER COUNT=1 DEFAULT=1
PARM NL TYPE=INTEGER COUNT=1 DEFAULT=0
PARM NS TYPE=INTEGER COUNT=1 DEFAULT=0
PARM NB TYPE=INTEGER COUNT=1 DEFAULT=0
PARM ADD TYPE=INTEGER COUNT=(5:125)
END-PROC
.TITLE
ADL
.HELP
ADL adds a constant value to pixels that are on lines (oriented
in any direction) that have been specified by their end points.
Several different lines may be drawn simultaneously, using differing
constants. The lines drawn by ADL are one pixel wide. If the DN value
of a pixel on a line is to be adjusted to a value less than 0, it is set
to 0; if a pixel is to be adjusted to a value greater than 255, it is
set to 255.
RESTRICTIONS;
1. Maximum image size is 10000 lines and 10000 samples.
2. Maximum number of lines to be drawn is 25.
3. Maximum number of pixels to be changed is 12000.
4. Byte data only.
HISTORY
Made portable for UNIX ... J. Turner (CRI).............. 31 Oct. 1994
Enabled for 3D images  ... N. Toole .................... 16 Sep. 2003
16-Feb-06 -lwk- corrected treatment of SIZE field in processing of ADD
.LEVEL1
.VARIABLE INP
input dataset
.VARIABLE OUT
output dataset
.VARIABLE SIZE
standard VICAR size field
.VARIABLE SL
starting line
.VARIABLE SS
starting sample
.VARIABLE SB
starting band
.VARIABLE NL
number of lines
.VARIABLE NS
number of samples
.VARIABLE NB
number of bands
.VARIABLE ADD
add x to sl,ss,el,es
.LEVEL2
.VARIABLE ADD
For each line that is to be added to the image, a set of 5 values
follow the keyword ADD. These 5 values are:
     1  The value added to all pixels on the line. (positive or negative)
     2  The line number of the first end point.
     3  The sample number of the first end point.
     4  The line number of the other end point.
     5  The sample number of the other end point.
Up to 25 sets of 5 values may follow the ADD keyword. End points may be
specified in any order.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstadl.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage = "none"
let _onfail="continue"
let $echo="yes"

!
!  THIS IS A TEST OF PROGRAM ADL
!
gen a1010   10 10
list a1010
!
!  make 2 flat images:
gen a0 nl=10 ns=10 ival=0  sinc=0 linc=0
list a0 'zero
gen a99 nl=10 ns=10 ival=99 sinc=0 linc=0
list a99
!
!  simplest case:  use adl to copy an image
!
adl a1010 b1010 add=(0  1,1  1,1)
write "Should get 0 differences on this and all other difpics in this pdf"
difpic (a1010 b1010)
!
!  simple case:  use adl to copy an image with SIZE field.
!
adl a1010 b1010 size=(3,2,7,8) add=(0  1,1  1,1)
copy a1010 c1010 size=(3,2,7,8) 
difpic (c1010 b1010)
!
!  maximal case:  125 values for ADD parameter (25 line segments)
!
gen b0 25 10 ival=0 linc=0 sinc=0
adl b0 b1 +
    add=(0  1,1  1,10        1  2,1  2,10           2  3,1  3,10   +
         3  4,1  4,10        4  5,1  5,10           5  6,1  6,10   +
         6  7,1  7,10        7  8,1  8,10           8  9,1  9,10   +
         9 10,1 10,10       10 11,1 11,10          11 12,1 12,10   +
        12 13,1 13,10       13 14,1 14,10          14 15,1 15,10 +
        15 16,1 16,10       16 17,1 17,10          17 18,1 18,10 +
        18 19,1 19,10       19 20,1 20,10          20 21,1 21,10 +
        21 22,1 22,10       22 23,1 23,10          23 24,1 24,10 +
        24 25,1 25,10 )
gen c1 25 10 sinc=0
difpic (b1 c1)
!
!  simple case:  one diagonal line
!
adl a0 b0 add=(1  1,2  10,1)
list b0
!
!  simple case:  one pixel with SIZE field.
!
adl a1010 b1010 size=(3,2,7,8) add=(1 3,2  3,2)
list b1010
!
!  simple case:  one horizontal line with DN truncation to 255.
!
adl a99 b99 add=(200  1,1  1,10)
list b99
!
!  simple case:  one vertical line with DN truncation to 0.
!
adl a99 b99 add=(-100  1,1  10,1)
list b99
!
!  combined case:  one horizontal line and one diagonal line
!
adl a99 b99 add=(200  1,1  1,10     -100  1,10  10,1)
list b99
!
!  maximal case:  125 values for ADD parameter (1 line segment 25 times)
!
adl a0 b0 +
    add=(1  1,1  10,1       1  1,1  10,1        1  1,1  10,1   +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1   +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1   +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1   +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1   +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1   +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1   +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1   +
         1  1,1  10,1 )
list b0
!
!  maximal case:  12*1000 = 12000 pixels modified
!
gen a1000 12 1000 ival=0 linc=0 sinc=0
adl a1000 b1000 +
    add=(0  1,1  1,1000      1  2,1  2,1000         2  3,1  3,1000 +
         3  4,1  4,1000      4  5,1  5,1000         5  6,1  6,1000 +
         6  7,1  7,1000      7  8,1  8,1000         8  9,1  9,1000 +
         9 10,1 10,1000     10 11,1 11,1000        11 12,1 12,1000 )
gen c1000 12 1000 sinc=0
difpic (b1000  c1000)

!
!  3D Image test case
!
gen A3D1000 12 1000  3 ival=0 linc=0 sinc=0
adl A3D1000 B3D1000 +
    add=(0  1,1  1,1000      1  2,1  2,1000         2  3,1  3,1000 +
         3  4,1  4,1000      4  5,1  5,1000         5  6,1  6,1000 +
         6  7,1  7,1000      7  8,1  8,1000         8  9,1  9,1000 +
         9 10,1 10,1000     10 11,1 11,1000        11 12,1 12,1000 )
gen C3D1000 12 1000 3 sinc=0
difpic (B3D1000  C3D1000)
!

!
!  case of problematical size field
!
adl a1010 c1010 size=(5,5,4,4) add=(200,7,6,7,8)
list c1010

end-proc
$!-----------------------------------------------------------------------------
$ create new_3d_session.log
tstadl
gen a1010   10 10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a1010
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:36:50 2003
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
gen a0 nl=10 ns=10 ival=0  sinc=0 linc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a0 'zero
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:36:50 2003
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0   0   0   0
      7       0   0   0   0   0   0   0   0   0   0
      8       0   0   0   0   0   0   0   0   0   0
      9       0   0   0   0   0   0   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0
gen a99 nl=10 ns=10 ival=99 sinc=0 linc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a99
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:36:50 2003
     Samp     1       3       5       7       9
   Line
      1      99  99  99  99  99  99  99  99  99  99
      2      99  99  99  99  99  99  99  99  99  99
      3      99  99  99  99  99  99  99  99  99  99
      4      99  99  99  99  99  99  99  99  99  99
      5      99  99  99  99  99  99  99  99  99  99
      6      99  99  99  99  99  99  99  99  99  99
      7      99  99  99  99  99  99  99  99  99  99
      8      99  99  99  99  99  99  99  99  99  99
      9      99  99  99  99  99  99  99  99  99  99
     10      99  99  99  99  99  99  99  99  99  99
adl a1010 b1010 add=(0  1,1  1,1)
Beginning VICAR task adl
ADL version 16-Sep-03
write "Should get 0 differences on this and all other difpics in this pdf"
Should get 0 differences on this and all other difpics in this pdf
difpic (a1010 b1010)
Beginning VICAR task difpic
DIFPIC version 10-11-95
 NUMBER OF DIFFERENCES =   0
adl a1010 b1010 size=(3,2,7,8) add=(0  1,1  1,1)
Beginning VICAR task adl
ADL version 16-Sep-03
copy a1010 c1010 size=(3,2,7,8)
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
difpic (c1010 b1010)
Beginning VICAR task difpic
DIFPIC version 10-11-95
 NUMBER OF DIFFERENCES =   0
gen b0 25 10 ival=0 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
adl b0 b1  +
    add=(0  1,1  1,10        1  2,1  2,10           2  3,1  3,10    +
         3  4,1  4,10        4  5,1  5,10           5  6,1  6,10    +
         6  7,1  7,10        7  8,1  8,10           8  9,1  9,10    +
         9 10,1 10,10       10 11,1 11,10          11 12,1 12,10    +
        12 13,1 13,10       13 14,1 14,10          14 15,1 15,10  +
        15 16,1 16,10       16 17,1 17,10          17 18,1 18,10  +
        18 19,1 19,10       19 20,1 20,10          20 21,1 21,10  +
        21 22,1 22,10       22 23,1 23,10          23 24,1 24,10  +
        24 25,1 25,10 )
Beginning VICAR task adl
ADL version 16-Sep-03
gen c1 25 10 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (b1 c1)
Beginning VICAR task difpic
DIFPIC version 10-11-95
 NUMBER OF DIFFERENCES =   0
adl a0 b0 add=(1  1,2  10,1)
Beginning VICAR task adl
ADL version 16-Sep-03
list b0
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:36:50 2003
 Task:ADL       User:ntt       Date_Time:Wed Oct  8 16:36:53 2003
     Samp     1       3       5       7       9
   Line
      1       0   1   0   0   0   0   0   0   0   0
      2       0   1   0   0   0   0   0   0   0   0
      3       0   1   0   0   0   0   0   0   0   0
      4       0   1   0   0   0   0   0   0   0   0
      5       0   1   0   0   0   0   0   0   0   0
      6       1   0   0   0   0   0   0   0   0   0
      7       1   0   0   0   0   0   0   0   0   0
      8       1   0   0   0   0   0   0   0   0   0
      9       1   0   0   0   0   0   0   0   0   0
     10       1   0   0   0   0   0   0   0   0   0
adl a1010 b1010 size=(3,2,7,8) add=(1 3,2  3,2)
Beginning VICAR task adl
ADL version 16-Sep-03
list b1010
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:36:50 2003
 Task:ADL       User:ntt       Date_Time:Wed Oct  8 16:36:54 2003
     Samp     1       3       5       7
   Line
      1       4   4   5   6   7   8   9  10
      2       4   5   6   7   8   9  10  11
      3       5   6   7   8   9  10  11  12
      4       6   7   8   9  10  11  12  13
      5       7   8   9  10  11  12  13  14
      6       8   9  10  11  12  13  14  15
      7       9  10  11  12  13  14  15  16
adl a99 b99 add=(200  1,1  1,10)
Beginning VICAR task adl
ADL version 16-Sep-03
list b99
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:36:50 2003
 Task:ADL       User:ntt       Date_Time:Wed Oct  8 16:36:54 2003
     Samp     1       3       5       7       9
   Line
      1     255 255 255 255 255 255 255 255 255 255
      2      99  99  99  99  99  99  99  99  99  99
      3      99  99  99  99  99  99  99  99  99  99
      4      99  99  99  99  99  99  99  99  99  99
      5      99  99  99  99  99  99  99  99  99  99
      6      99  99  99  99  99  99  99  99  99  99
      7      99  99  99  99  99  99  99  99  99  99
      8      99  99  99  99  99  99  99  99  99  99
      9      99  99  99  99  99  99  99  99  99  99
     10      99  99  99  99  99  99  99  99  99  99
adl a99 b99 add=(-100  1,1  10,1)
Beginning VICAR task adl
ADL version 16-Sep-03
list b99
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:36:50 2003
 Task:ADL       User:ntt       Date_Time:Wed Oct  8 16:36:55 2003
     Samp     1       3       5       7       9
   Line
      1       0  99  99  99  99  99  99  99  99  99
      2       0  99  99  99  99  99  99  99  99  99
      3       0  99  99  99  99  99  99  99  99  99
      4       0  99  99  99  99  99  99  99  99  99
      5       0  99  99  99  99  99  99  99  99  99
      6       0  99  99  99  99  99  99  99  99  99
      7       0  99  99  99  99  99  99  99  99  99
      8       0  99  99  99  99  99  99  99  99  99
      9       0  99  99  99  99  99  99  99  99  99
     10       0  99  99  99  99  99  99  99  99  99
adl a99 b99 add=(200  1,1  1,10     -100  1,10  10,1)
Beginning VICAR task adl
ADL version 16-Sep-03
list b99
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:36:50 2003
 Task:ADL       User:ntt       Date_Time:Wed Oct  8 16:36:55 2003
     Samp     1       3       5       7       9
   Line
      1     255 255 255 255 255 255 255 255 255 200
      2      99  99  99  99  99  99  99  99   0  99
      3      99  99  99  99  99  99  99   0  99  99
      4      99  99  99  99  99  99   0  99  99  99
      5      99  99  99  99  99   0  99  99  99  99
      6      99  99  99  99   0  99  99  99  99  99
      7      99  99  99   0  99  99  99  99  99  99
      8      99  99   0  99  99  99  99  99  99  99
      9      99   0  99  99  99  99  99  99  99  99
     10       0  99  99  99  99  99  99  99  99  99
adl a0 b0  +
    add=(1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1 )
Beginning VICAR task adl
ADL version 16-Sep-03
list b0
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:36:50 2003
 Task:ADL       User:ntt       Date_Time:Wed Oct  8 16:36:56 2003
     Samp     1       3       5       7       9
   Line
      1      25   0   0   0   0   0   0   0   0   0
      2      25   0   0   0   0   0   0   0   0   0
      3      25   0   0   0   0   0   0   0   0   0
      4      25   0   0   0   0   0   0   0   0   0
      5      25   0   0   0   0   0   0   0   0   0
      6      25   0   0   0   0   0   0   0   0   0
      7      25   0   0   0   0   0   0   0   0   0
      8      25   0   0   0   0   0   0   0   0   0
      9      25   0   0   0   0   0   0   0   0   0
     10      25   0   0   0   0   0   0   0   0   0
gen a1000 12 1000 ival=0 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
adl a1000 b1000  +
    add=(0  1,1  1,1000      1  2,1  2,1000         2  3,1  3,1000  +
         3  4,1  4,1000      4  5,1  5,1000         5  6,1  6,1000  +
         6  7,1  7,1000      7  8,1  8,1000         8  9,1  9,1000  +
         9 10,1 10,1000     10 11,1 11,1000        11 12,1 12,1000 )
Beginning VICAR task adl
ADL version 16-Sep-03
gen c1000 12 1000 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (b1000  c1000)
Beginning VICAR task difpic
DIFPIC version 10-11-95
 NUMBER OF DIFFERENCES =   0
gen A3D1000 12 1000  3 ival=0 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
adl A3D1000 B3D1000  +
    add=(0  1,1  1,1000      1  2,1  2,1000         2  3,1  3,1000  +
         3  4,1  4,1000      4  5,1  5,1000         5  6,1  6,1000  +
         6  7,1  7,1000      7  8,1  8,1000         8  9,1  9,1000  +
         9 10,1 10,1000     10 11,1 11,1000        11 12,1 12,1000 )
Beginning VICAR task adl
ADL version 16-Sep-03
gen C3D1000 12 1000 3 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (B3D1000  C3D1000)
Beginning VICAR task difpic
DIFPIC version 10-11-95
  Number of bands to process =   3
 NUMBER OF DIFFERENCES =   0
end-proc
disable-log
$!-----------------------------------------------------------------------------
$ create old_3d_session.log
tstadl
gen a1010   10 10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a1010
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:37:38 2003
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
gen a0 nl=10 ns=10 ival=0  sinc=0 linc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a0 'zero
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:37:39 2003
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0   0   0   0
      7       0   0   0   0   0   0   0   0   0   0
      8       0   0   0   0   0   0   0   0   0   0
      9       0   0   0   0   0   0   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0
gen a99 nl=10 ns=10 ival=99 sinc=0 linc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a99
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:37:39 2003
     Samp     1       3       5       7       9
   Line
      1      99  99  99  99  99  99  99  99  99  99
      2      99  99  99  99  99  99  99  99  99  99
      3      99  99  99  99  99  99  99  99  99  99
      4      99  99  99  99  99  99  99  99  99  99
      5      99  99  99  99  99  99  99  99  99  99
      6      99  99  99  99  99  99  99  99  99  99
      7      99  99  99  99  99  99  99  99  99  99
      8      99  99  99  99  99  99  99  99  99  99
      9      99  99  99  99  99  99  99  99  99  99
     10      99  99  99  99  99  99  99  99  99  99
adl a1010 b1010 add=(0  1,1  1,1)
Beginning VICAR task adl
ADL version 31-Oct-94
write "Should get 0 differences on this and all other difpics in this pdf"
Should get 0 differences on this and all other difpics in this pdf
difpic (a1010 b1010)
Beginning VICAR task difpic
DIFPIC version 10-11-95
 NUMBER OF DIFFERENCES =   0
adl a1010 b1010 size=(3,2,7,8) add=(0  1,1  1,1)
Beginning VICAR task adl
ADL version 31-Oct-94
copy a1010 c1010 size=(3,2,7,8)
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
difpic (c1010 b1010)
Beginning VICAR task difpic
DIFPIC version 10-11-95
 NUMBER OF DIFFERENCES =   0
gen b0 25 10 ival=0 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
adl b0 b1  +
    add=(0  1,1  1,10        1  2,1  2,10           2  3,1  3,10    +
         3  4,1  4,10        4  5,1  5,10           5  6,1  6,10    +
         6  7,1  7,10        7  8,1  8,10           8  9,1  9,10    +
         9 10,1 10,10       10 11,1 11,10          11 12,1 12,10    +
        12 13,1 13,10       13 14,1 14,10          14 15,1 15,10  +
        15 16,1 16,10       16 17,1 17,10          17 18,1 18,10  +
        18 19,1 19,10       19 20,1 20,10          20 21,1 21,10  +
        21 22,1 22,10       22 23,1 23,10          23 24,1 24,10  +
        24 25,1 25,10 )
Beginning VICAR task adl
ADL version 31-Oct-94
gen c1 25 10 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (b1 c1)
Beginning VICAR task difpic
DIFPIC version 10-11-95
 NUMBER OF DIFFERENCES =   0
adl a0 b0 add=(1  1,2  10,1)
Beginning VICAR task adl
ADL version 31-Oct-94
list b0
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:37:39 2003
 Task:ADL       User:ntt       Date_Time:Tue Sep 16 11:37:41 2003
     Samp     1       3       5       7       9
   Line
      1       0   1   0   0   0   0   0   0   0   0
      2       0   1   0   0   0   0   0   0   0   0
      3       0   1   0   0   0   0   0   0   0   0
      4       0   1   0   0   0   0   0   0   0   0
      5       0   1   0   0   0   0   0   0   0   0
      6       1   0   0   0   0   0   0   0   0   0
      7       1   0   0   0   0   0   0   0   0   0
      8       1   0   0   0   0   0   0   0   0   0
      9       1   0   0   0   0   0   0   0   0   0
     10       1   0   0   0   0   0   0   0   0   0
adl a1010 b1010 size=(3,2,7,8) add=(1 3,2  3,2)
Beginning VICAR task adl
ADL version 31-Oct-94
list b1010
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:37:38 2003
 Task:ADL       User:ntt       Date_Time:Tue Sep 16 11:37:41 2003
     Samp     1       3       5       7
   Line
      1       4   4   5   6   7   8   9  10
      2       4   5   6   7   8   9  10  11
      3       5   6   7   8   9  10  11  12
      4       6   7   8   9  10  11  12  13
      5       7   8   9  10  11  12  13  14
      6       8   9  10  11  12  13  14  15
      7       9  10  11  12  13  14  15  16
adl a99 b99 add=(200  1,1  1,10)
Beginning VICAR task adl
ADL version 31-Oct-94
list b99
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:37:39 2003
 Task:ADL       User:ntt       Date_Time:Tue Sep 16 11:37:42 2003
     Samp     1       3       5       7       9
   Line
      1     255 255 255 255 255 255 255 255 255 255
      2      99  99  99  99  99  99  99  99  99  99
      3      99  99  99  99  99  99  99  99  99  99
      4      99  99  99  99  99  99  99  99  99  99
      5      99  99  99  99  99  99  99  99  99  99
      6      99  99  99  99  99  99  99  99  99  99
      7      99  99  99  99  99  99  99  99  99  99
      8      99  99  99  99  99  99  99  99  99  99
      9      99  99  99  99  99  99  99  99  99  99
     10      99  99  99  99  99  99  99  99  99  99
adl a99 b99 add=(-100  1,1  10,1)
Beginning VICAR task adl
ADL version 31-Oct-94
list b99
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:37:39 2003
 Task:ADL       User:ntt       Date_Time:Tue Sep 16 11:37:42 2003
     Samp     1       3       5       7       9
   Line
      1       0  99  99  99  99  99  99  99  99  99
      2       0  99  99  99  99  99  99  99  99  99
      3       0  99  99  99  99  99  99  99  99  99
      4       0  99  99  99  99  99  99  99  99  99
      5       0  99  99  99  99  99  99  99  99  99
      6       0  99  99  99  99  99  99  99  99  99
      7       0  99  99  99  99  99  99  99  99  99
      8       0  99  99  99  99  99  99  99  99  99
      9       0  99  99  99  99  99  99  99  99  99
     10       0  99  99  99  99  99  99  99  99  99
adl a99 b99 add=(200  1,1  1,10     -100  1,10  10,1)
Beginning VICAR task adl
ADL version 31-Oct-94
list b99
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:37:39 2003
 Task:ADL       User:ntt       Date_Time:Tue Sep 16 11:37:42 2003
     Samp     1       3       5       7       9
   Line
      1     255 255 255 255 255 255 255 255 255 200
      2      99  99  99  99  99  99  99  99   0  99
      3      99  99  99  99  99  99  99   0  99  99
      4      99  99  99  99  99  99   0  99  99  99
      5      99  99  99  99  99   0  99  99  99  99
      6      99  99  99  99   0  99  99  99  99  99
      7      99  99  99   0  99  99  99  99  99  99
      8      99  99   0  99  99  99  99  99  99  99
      9      99   0  99  99  99  99  99  99  99  99
     10       0  99  99  99  99  99  99  99  99  99
adl a0 b0  +
    add=(1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1 )
Beginning VICAR task adl
ADL version 31-Oct-94
list b0
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 11:37:39 2003
 Task:ADL       User:ntt       Date_Time:Tue Sep 16 11:37:43 2003
     Samp     1       3       5       7       9
   Line
      1      25   0   0   0   0   0   0   0   0   0
      2      25   0   0   0   0   0   0   0   0   0
      3      25   0   0   0   0   0   0   0   0   0
      4      25   0   0   0   0   0   0   0   0   0
      5      25   0   0   0   0   0   0   0   0   0
      6      25   0   0   0   0   0   0   0   0   0
      7      25   0   0   0   0   0   0   0   0   0
      8      25   0   0   0   0   0   0   0   0   0
      9      25   0   0   0   0   0   0   0   0   0
     10      25   0   0   0   0   0   0   0   0   0
gen a1000 12 1000 ival=0 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
adl a1000 b1000  +
    add=(0  1,1  1,1000      1  2,1  2,1000         2  3,1  3,1000  +
         3  4,1  4,1000      4  5,1  5,1000         5  6,1  6,1000  +
         6  7,1  7,1000      7  8,1  8,1000         8  9,1  9,1000  +
         9 10,1 10,1000     10 11,1 11,1000        11 12,1 12,1000 )
Beginning VICAR task adl
ADL version 31-Oct-94
gen c1000 12 1000 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (b1000  c1000)
Beginning VICAR task difpic
DIFPIC version 10-11-95
 NUMBER OF DIFFERENCES =   0
gen A3D1000 12 1000  3 ival=0 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
adl A3D1000 B3D1000  +
    add=(0  1,1  1,1000      1  2,1  2,1000         2  3,1  3,1000  +
         3  4,1  4,1000      4  5,1  5,1000         5  6,1  6,1000  +
         6  7,1  7,1000      7  8,1  8,1000         8  9,1  9,1000  +
         9 10,1 10,1000     10 11,1 11,1000        11 12,1 12,1000 )
Beginning VICAR task adl
ADL version 31-Oct-94
[VIC2-GENERR] Exception in XVREAD, processing file: A3D1000
[VIC2-STRTREC] Bad starting record for read or write operation; program error.
 Current line in image = 0
 ** ABEND called **
continue
gen C3D1000 12 1000 3 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (B3D1000  C3D1000)
Beginning VICAR task difpic
DIFPIC version 10-11-95
  Number of bands to process =   3
 NUMBER OF DIFFERENCES =      22576
end-proc
disable-log
$!-----------------------------------------------------------------------------
$ create tstadl.log_solos
tstadl
gen a1010   10 10
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a1010
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Feb 16 18:21:27 2006
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
gen a0 nl=10 ns=10 ival=0  sinc=0 linc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a0 'zero
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Feb 16 18:21:27 2006
     Samp     1       3       5       7       9
   Line
      1       0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0   0   0   0
      7       0   0   0   0   0   0   0   0   0   0
      8       0   0   0   0   0   0   0   0   0   0
      9       0   0   0   0   0   0   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0
gen a99 nl=10 ns=10 ival=99 sinc=0 linc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
list a99
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Feb 16 18:21:28 2006
     Samp     1       3       5       7       9
   Line
      1      99  99  99  99  99  99  99  99  99  99
      2      99  99  99  99  99  99  99  99  99  99
      3      99  99  99  99  99  99  99  99  99  99
      4      99  99  99  99  99  99  99  99  99  99
      5      99  99  99  99  99  99  99  99  99  99
      6      99  99  99  99  99  99  99  99  99  99
      7      99  99  99  99  99  99  99  99  99  99
      8      99  99  99  99  99  99  99  99  99  99
      9      99  99  99  99  99  99  99  99  99  99
     10      99  99  99  99  99  99  99  99  99  99
adl a1010 b1010 add=(0  1,1  1,1)
Beginning VICAR task adl
ADL version 16-Feb-06
write "Should get 0 differences on this and all other difpics in this pdf"
Should get 0 differences on this and all other difpics in this pdf
difpic (a1010 b1010)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
adl a1010 b1010 size=(3,2,7,8) add=(0  1,1  1,1)
Beginning VICAR task adl
ADL version 16-Feb-06
copy a1010 c1010 size=(3,2,7,8)
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
difpic (c1010 b1010)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
gen b0 25 10 ival=0 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
adl b0 b1  +
    add=(0  1,1  1,10        1  2,1  2,10           2  3,1  3,10    +
         3  4,1  4,10        4  5,1  5,10           5  6,1  6,10    +
         6  7,1  7,10        7  8,1  8,10           8  9,1  9,10    +
         9 10,1 10,10       10 11,1 11,10          11 12,1 12,10    +
        12 13,1 13,10       13 14,1 14,10          14 15,1 15,10  +
        15 16,1 16,10       16 17,1 17,10          17 18,1 18,10  +
        18 19,1 19,10       19 20,1 20,10          20 21,1 21,10  +
        21 22,1 22,10       22 23,1 23,10          23 24,1 24,10  +
        24 25,1 25,10 )
Beginning VICAR task adl
ADL version 16-Feb-06
gen c1 25 10 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (b1 c1)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
adl a0 b0 add=(1  1,2  10,1)
Beginning VICAR task adl
ADL version 16-Feb-06
list b0
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Feb 16 18:21:27 2006
 Task:ADL       User:lwk       Date_Time:Thu Feb 16 18:21:32 2006
     Samp     1       3       5       7       9
   Line
      1       0   1   0   0   0   0   0   0   0   0
      2       0   1   0   0   0   0   0   0   0   0
      3       0   1   0   0   0   0   0   0   0   0
      4       0   1   0   0   0   0   0   0   0   0
      5       0   1   0   0   0   0   0   0   0   0
      6       1   0   0   0   0   0   0   0   0   0
      7       1   0   0   0   0   0   0   0   0   0
      8       1   0   0   0   0   0   0   0   0   0
      9       1   0   0   0   0   0   0   0   0   0
     10       1   0   0   0   0   0   0   0   0   0
adl a1010 b1010 size=(3,2,7,8) add=(1 3,2  3,2)
Beginning VICAR task adl
ADL version 16-Feb-06
list b1010
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Feb 16 18:21:27 2006
 Task:ADL       User:lwk       Date_Time:Thu Feb 16 18:21:33 2006
     Samp     1       3       5       7
   Line
      1       4   4   5   6   7   8   9  10
      2       4   5   6   7   8   9  10  11
      3       5   6   7   8   9  10  11  12
      4       6   7   8   9  10  11  12  13
      5       7   8   9  10  11  12  13  14
      6       8   9  10  11  12  13  14  15
      7       9  10  11  12  13  14  15  16
adl a99 b99 add=(200  1,1  1,10)
Beginning VICAR task adl
ADL version 16-Feb-06
list b99
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Feb 16 18:21:28 2006
 Task:ADL       User:lwk       Date_Time:Thu Feb 16 18:21:33 2006
     Samp     1       3       5       7       9
   Line
      1     255 255 255 255 255 255 255 255 255 255
      2      99  99  99  99  99  99  99  99  99  99
      3      99  99  99  99  99  99  99  99  99  99
      4      99  99  99  99  99  99  99  99  99  99
      5      99  99  99  99  99  99  99  99  99  99
      6      99  99  99  99  99  99  99  99  99  99
      7      99  99  99  99  99  99  99  99  99  99
      8      99  99  99  99  99  99  99  99  99  99
      9      99  99  99  99  99  99  99  99  99  99
     10      99  99  99  99  99  99  99  99  99  99
adl a99 b99 add=(-100  1,1  10,1)
Beginning VICAR task adl
ADL version 16-Feb-06
list b99
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Feb 16 18:21:28 2006
 Task:ADL       User:lwk       Date_Time:Thu Feb 16 18:21:35 2006
     Samp     1       3       5       7       9
   Line
      1       0  99  99  99  99  99  99  99  99  99
      2       0  99  99  99  99  99  99  99  99  99
      3       0  99  99  99  99  99  99  99  99  99
      4       0  99  99  99  99  99  99  99  99  99
      5       0  99  99  99  99  99  99  99  99  99
      6       0  99  99  99  99  99  99  99  99  99
      7       0  99  99  99  99  99  99  99  99  99
      8       0  99  99  99  99  99  99  99  99  99
      9       0  99  99  99  99  99  99  99  99  99
     10       0  99  99  99  99  99  99  99  99  99
adl a99 b99 add=(200  1,1  1,10     -100  1,10  10,1)
Beginning VICAR task adl
ADL version 16-Feb-06
list b99
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Feb 16 18:21:28 2006
 Task:ADL       User:lwk       Date_Time:Thu Feb 16 18:21:37 2006
     Samp     1       3       5       7       9
   Line
      1     255 255 255 255 255 255 255 255 255 200
      2      99  99  99  99  99  99  99  99   0  99
      3      99  99  99  99  99  99  99   0  99  99
      4      99  99  99  99  99  99   0  99  99  99
      5      99  99  99  99  99   0  99  99  99  99
      6      99  99  99  99   0  99  99  99  99  99
      7      99  99  99   0  99  99  99  99  99  99
      8      99  99   0  99  99  99  99  99  99  99
      9      99   0  99  99  99  99  99  99  99  99
     10       0  99  99  99  99  99  99  99  99  99
adl a0 b0  +
    add=(1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1       1  1,1  10,1        1  1,1  10,1    +
         1  1,1  10,1 )
Beginning VICAR task adl
ADL version 16-Feb-06
list b0
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Feb 16 18:21:27 2006
 Task:ADL       User:lwk       Date_Time:Thu Feb 16 18:21:38 2006
     Samp     1       3       5       7       9
   Line
      1      25   0   0   0   0   0   0   0   0   0
      2      25   0   0   0   0   0   0   0   0   0
      3      25   0   0   0   0   0   0   0   0   0
      4      25   0   0   0   0   0   0   0   0   0
      5      25   0   0   0   0   0   0   0   0   0
      6      25   0   0   0   0   0   0   0   0   0
      7      25   0   0   0   0   0   0   0   0   0
      8      25   0   0   0   0   0   0   0   0   0
      9      25   0   0   0   0   0   0   0   0   0
     10      25   0   0   0   0   0   0   0   0   0
gen a1000 12 1000 ival=0 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
adl a1000 b1000  +
    add=(0  1,1  1,1000      1  2,1  2,1000         2  3,1  3,1000  +
         3  4,1  4,1000      4  5,1  5,1000         5  6,1  6,1000  +
         6  7,1  7,1000      7  8,1  8,1000         8  9,1  9,1000  +
         9 10,1 10,1000     10 11,1 11,1000        11 12,1 12,1000 )
Beginning VICAR task adl
ADL version 16-Feb-06
gen c1000 12 1000 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (b1000  c1000)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
gen A3D1000 12 1000  3 ival=0 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
adl A3D1000 B3D1000  +
    add=(0  1,1  1,1000      1  2,1  2,1000         2  3,1  3,1000  +
         3  4,1  4,1000      4  5,1  5,1000         5  6,1  6,1000  +
         6  7,1  7,1000      7  8,1  8,1000         8  9,1  9,1000  +
         9 10,1 10,1000     10 11,1 11,1000        11 12,1 12,1000 )
Beginning VICAR task adl
ADL version 16-Feb-06
gen C3D1000 12 1000 3 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (B3D1000  C3D1000)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
adl a1010 c1010 size=(5,5,4,4) add=(200,7,6,7,8)
Beginning VICAR task adl
ADL version 16-Feb-06
list c1010
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Feb 16 18:21:27 2006
 Task:ADL       User:lwk       Date_Time:Thu Feb 16 18:21:41 2006
     Samp     1       3
   Line
      1       8   9  10  11
      2       9  10  11  12
      3      10 211 212 213
      4      11  12  13  14
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$!-----------------------------------------------------------------------------
$ create tstadl.log_linux
OUTPUT OF TSTADL.PDF ON LINUX IS IDENTICAL TO THAT ON SOLOS
-lwk- 17feb06
$ Return
$!#############################################################################
