$!****************************************************************************
$!
$! Build proc for MIPL module mss
$! VPACK Version 1.9, Monday, September 11, 2000, 15:20:30
$!
$! Execute by entering:		$ @mss
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
$ write sys$output "*** module mss ***"
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
$ write sys$output "Invalid argument given to mss.com file -- ", primary
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
$   if F$SEARCH("mss.imake") .nes. ""
$   then
$      vimake mss
$      purge mss.bld
$   else
$      if F$SEARCH("mss.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mss
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mss.bld "STD"
$   else
$      @mss.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mss.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mss.com -
	-s mss.f -
	-i mss.imake -
	-p mss.pdf -
	-t tstmss.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mss.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C   REVISION HISTORY
C
C     8-26-83    ASM MODIFIED FOR VAX CONVERSION 
C     8-10-84    SP  CONVERTED TO USE VICAR2 CALLS.
C     8-10-84    SP  CHANGED TO HANDLE SL NOT EQUAL TO 1.
C     8-10-84    SP  CHANGED TO HANDLE UP TO 48 INPUT FILES.
C     9-24-87    FFM UPDATED TEST FILE.
C     6-21-93    GAM PORTED TO UNIX
C     10-10-1997 TXH Corrected uninitialized variable problem.  This 
C                    problem caused the program to ABEND under SGI.
C                    The variable was removed, because it is no longer
C                    needed under the current MIPS environment.
C     09-11-2000 AXC Eliminated argument list terminator on RTL calling
C                    sequence XVPCNT to prevent program ABENDing under 
C                    Linux. (AR-104433)

      INCLUDE 'VICMAIN_FOR'
C**********************************************************************
C
C     'MSS'   PICTURE INTERLEAVING PROGRAM
C     MSS will put up to 48 input images side-by-side, left-to-right.
C
      SUBROUTINE MAIN44

         EXTERNAL WORK

         INTEGER IDSN(48), OUTFILE, IPIXSIZE
         INTEGER NS(48),LOC(48)

         CHARACTER*200 BUF
         CHARACTER*8 FMT

         LOGICAL QSIZE

C============START OF EXECUTABLE CODE==========================
         CALL XVEACTION( 'SA', ' ' )

         CALL XVPCNT( 'INP', NIN)
         IF (NIN .GT. 48) 
     &      CALL MABEND('** MAXIMUM # INPUT FILES = 48')

C  OPEN DATA SETS AND GET LENGTH OF LINE FOR EACH FILE.
         DO I = 1,NIN
            CALL XVUNIT(IDSN(I), 'INP', I, IND, ' ' )
            CALL XVOPEN(IDSN(I), IND, 'OP', 'READ', ' ' )
            CALL XVGET(IDSN(I), IND, 'NS', NS(I), ' ' )
         END DO

         CALL XVGET( IDSN(1), IND, 'FORMAT', FMT, ' ' )  ! BYTES PER PIXEL.
         IND = XVPIXSIZEU( IPIXSIZE, FMT, IDSN(1) )

         CALL XVSIZE( ISL, ISSAMP, NL, NS1, NLL, NSL )   ! GET SIZE PARAMETER.
         QSIZE = NS1 .NE. NSL

C     COMPUTE OUTPUT LINE LENGTH
         N = 1
         NSO = 0

         DO 100 I=1,NIN
           IF (QSIZE)   NS(I)=NS1
           LOC(I) = N
           N = N+NS(I)*IPIXSIZE
           NSO = NSO + NS(I)
  100    CONTINUE

C   OPEN OUTPUT FILE
         CALL XVUNIT( OUTFILE, 'OUT', 1, IND, ' ' )
         CALL XVOPEN
     &      (OUTFILE,IND,'OP','WRITE','U_NL',NL,'U_NS',NSO,' ')


C     REPORT NSO,NIN AND CALL STACKA
         BUF(1:47) = '** OUTPUT CONTAINS    INTERLEAVED DATA SETS **'
         WRITE (BUF(21:22),'(I2)') NIN
         CALL XVMESSAGE(BUF(2:47),' ')
         BUF(1:48) = '** ACTUAL OUTPUT RECORD LENGTH       SAMPLES **'
         WRITE (BUF(33:37),'(I5)') NSO
         CALL XVMESSAGE(BUF(2:48),' ')
         CALL STACKA(11,WORK,1,N,NL,NIN,NS,LOC,IDSN,ISL,ISSAMP,
     &               OUTFILE)
         RETURN
      END
C**********************************************************************


      SUBROUTINE WORK(OUT,NN,NL,NIN,NS,LOC,IDSN,ISL,ISSAMP,OUTFILE)

         INTEGER NS(NIN),LOC(NIN),IDSN(NIN)
         BYTE OUT(*)


C     MAIN LOOP
         DO 500 I= ISL, ISL + NL - 1
            DO 300 J=1,NIN
               CALL XVREAD( IDSN(J), OUT( LOC(J) ), IND, 'LINE', I,
     &                   'SAMP', ISSAMP, 'NSAMPS', NS(J), ' ' )
  300       CONTINUE
            CALL XVWRIT( OUTFILE, OUT, IND, ' ' )
  500    CONTINUE
         RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mss.imake
#define  PROGRAM   mss

#define MODULE_LIST mss.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create mss.pdf
process help=*
PARM INP TYPE=STRING COUNT=2:48
PARM OUT TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
!# parm inp(3-48) hints=default
END-PROC
.TITLE
mss
.HELP
PURPOSE:
mss combines up to 48 datasets into a single dataset with mss format.
This is equivalent to concatenating the input images in a left to right
fashion.

EXECUTION:

Example

mss INP=(A,B,C) OUT=D  will put images A, B, and C side-by-side to form D.

If the size parameter is used (SIZE=(SL,SS,NL,NS)), only the defined area in
each input image will be used to create the new file.  


OPERATION:
mss combines datasets in the following manner:  
Each line is made up of the corresponding input lines laid end to end in
a concatenated manner.  That is, the first pixel of each input is placed
to the right of the last pixel of the previous input.  The line thus
formed will have the same number of samples per line as the sum of the
inputs. (If the SIZE field is used, it will be NS * #-inputs.)

HISTORY:
WRITTEN BY:  J.D. Addington, 23 July 1974
COGNIZANT PROGRAMMER:  Steven Pohorsky
DOCUMENTATION AUTHOR:  J.D. Addington
REVISION:  1, 25 January 1982

   10 Oct. 1997  Thomas Huang      Corrected problem on uninitialized variable,
                                   which casued the problem to ABEND under SGI.
                                   The variable was removed, because it is no
                                   longer needed under the current MIPS 
                                   environment.  

   21 June 1993  G. A. Madrid Jr.  PORTED TO UNIX

.LEVEL1
.VARIABLE INP
STRING - Input image files
.VARIABLE OUT
STRING - Output image file
.VARIABLE SIZE
INTEGER - Region of input files
to be concatenated
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE NS
INTEGER - Number of lines
.VARIABLE NL
INTEGER - Number of samples
.LEVEL2
.VARIABLE INP
INP specifies the input data sets.  Up to 48 are allowed.
.VARIABLE SIZE
The SIZE parameter may be used when only a sub-region of each image is to
be concatenated; it has the format SIZE=(SL,SS,NL,NS), where the parameters
are starting line, starting sample, number of lines, and number of samples,
respectively.  SIZE=(1,1,10,10), for example, will cause mss to only look
at the first ten samples of each of the first ten lines in each image, when
performing the concatenation. If NS is equal to the number of samples in the
first input file, NS from the SIZE parameter is not used; the actual number
of samples in each input file will be used instead.
.VARIABLE SL
INTEGER - Starting line (see SIZE)
.VARIABLE SS
INTEGER - Starting sample (see SIZE)
.VARIABLE NS
INTEGER - Number of lines (see SIZE)
.VARIABLE NL
INTEGER - Number of samples (see SIZE)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmss.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let $autousage = "none"
let _onfail="continue"
let $echo="yes"

! TEST SCRIPT FOR mss
!
write "This is a small example to show how to use mss. This example takes"
write "3 datasets with different number of samples and concatenates them"
write "side by side to result in a 10x28 size dataset. NOTE: Each dataset"
write "MUST have the same number of lines"
gen ba.img 10 10
gen bb.img 10 8 
gen bc.img 10 10
mss (ba.img, bb.img, bc.img) bd.img
list bd.img

write "This example shows the use of the SIZE parameter. If NS of"
write "the size field is equal to the number of samples in the"
write "first input file, NS of the SIZE parameter is not used;"
write "the actual number of samples in each input file will be used instead."
mss (ba.img, bb.img, bc.img) bd.img SIZE=(1,1,10,10)
list bd.img

! try SL and SS not equal to 1.
mss (ba.img, bb.img, bc.img) be.img SIZE=(2,3,8,5)
list be.img
 

! TRY HALFWORD DATA
gen ha.img 10 10 'HALF
gen hb.img 10 8  'HALF
gen hc.img 10 10 'HALF
mss (ha.img, hb.img, hc.img) hd.img 
list hd.img
 
! try SL and SS not equal to 1.
mss (ha.img, hb.img, hc.img) he.img SIZE=(2,3,8,5)
list he.img

! TRY REAL*4 DATA
gen ra.img 10 10 'REAL4
gen rb.img 10 8  'REAL4
gen rc.img 10 10 'REAL4
mss (ra.img, rb.img, rc.img) rd.img 
list rd.img

! try SL and SS not equal to 1.
mss (ra.img, rb.img, rc.img) re.img SIZE=(2,3,8,5)
list re.img 'REAL4

write "This is a small example to show how to use mss. This example takes"
write "3 datasets with the same number of samples and concatenates them side"
write "by side to result in a 10x30 size dataset. NOTE: Each dataset "
write "MUST have the same number of lines"
gen a.img 10 10 IVAL=1 SINC=0 LINC=0
gen b.img 10 10 IVAL=2 SINC=0 LINC=0
gen c.img 10 10 IVAL=3 SINC=0 LINC=0
mss INP=(a.img, b.img, c.img) OUT=d.img
list d.img

write "This example shows the use of the SIZE parameter. mss can also take"
write "a portion of the input pictures and concatenate them side by side."
gen a2.img 20 20
gen b2.img 20 20
gen c2.img 20 20
mss (a2.img, b2.img, c2.img) d2.img SIZE=(1,1,10,10)
list d2.img

write "Now, clean up!!"
if ($syschar(1)="UNIX")
   ush rm *.img
else
   dcl del *.img;*
end-if
!
!
end-proc
$ Return
$!#############################################################################
