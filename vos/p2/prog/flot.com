$!****************************************************************************
$!
$! Build proc for MIPL module flot
$! VPACK Version 1.7, Friday, July 09, 1993, 14:08:21
$!
$! Execute by entering:		$ @flot
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
$ write sys$output "*** module flot ***"
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
$ write sys$output "Invalid argument given to flot.com file -- ", primary
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
$   if F$SEARCH("flot.imake") .nes. ""
$   then
$      vimake flot
$      purge flot.bld
$   else
$      if F$SEARCH("flot.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake flot
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @flot.bld "STD"
$   else
$      @flot.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create flot.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack flot.com -
	-s flot.f flot_vms.f flot_unix.f -
	-p flot.pdf -
	-t tstflot.pdf -
	-i flot.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create flot.f
$ DECK/DOLLARS="$ VOKAGLEVE"
       PROGRAM  FLOT
C#######################################################################
C  NAME OF ROUTINE
C      FLOT ( FLip or rOTate )
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      Program FLOT is a VICAR applications program which is used to flip or 
C      rotate an image.  
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    AUGUST 1984
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C
C  ORIGINAL FLOT PROGRAM BY
C      T. C. RINDFLEISCH with modifications by GARY YAGI
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     8-84  SP   CONVERTED FROM IBM VICAR VERSION: CHANGED TO USE VICAR 2
C                (XVREAD...) ROUTINES. MISCELLANEOUS CLEANUP.
C     9-84  SP   IMPOSED LIMIT OF 200000 TO STACKA FOR ROTATE TO PREVENT
C                PAGING UNDER WORKING SET LIMIT OF 200000.
C     9-84  SP   ADDED ROT180 KEYWORD TO PROVIDE 180 DEGREE ROTATION MODE.
C    10-92  sp   USE WORKING SET EXTENT TO ALLOCATE SPACE.
C     6-93  GM	 PORTED TO UNIX.
C  PROGRAM LIMITATIONS
C      SEE HLP FILE.
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
C      MAIN44 processes parameters entered by user and calls routines
C      that operate (flip, rotate,...) on the image.
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     AUGUST 1984
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR1 EXECUTIVE       FORTRAN-77
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  CALLED BY
C      FLOT
C
C*   27 JAN 77   ... GMY ...  INITIAL RELEASE
C  INPUT MUST BE A DISK DATA SET (PREFERABLY BLOCKED)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT INTEGER(A-Z)

      LOGICAL XVPTST                       ! DECLARES XVPTST A LOG. FUNCTION.
      EXTERNAL FLIP,ROTATE
      CHARACTER*8 FMT

      COMMON /C1/    INFILE, OUTFILE, ISL, ISSAMP, NL, NSAMP, NLO, NSO,
     .               DCODE,  MODE

C
C=================START OF EXECUTABLE CODE===============================

C          DCODE=1 FOR BYTE DATA,       =2 FOR HALFWORD
C               =4 FOR FULL OR REAL*4,  =8 FOR REAL*8 OR COMPLEX
C          MODE=0 FOR 180 DEGREE ROTATION
C              =1 FOR VERTICAL FLIP
C              =2 FOR HORIZONTAL FLIP
C              =3 FOR CLOCKWISE ROTATION
C              =4 FOR COUNTER CLOCKWISE ROTATION
C              =5 FOR TRANSPOSE MATRIX
      CALL XVEACTION( 'SA', ' ' )

      MODE = 3              ! DEFAULT IS CLOCKWISE.

C  OPEN INPUT FILE

      CALL XVUNIT( INFILE, 'INP', 1, IND, ' ' )
      CALL XVOPEN( INFILE, IND, 'OP', 'READ', ' ' )

      CALL XVSIZE( ISL, ISSAMP, NL, NSAMP, NLI, NSI )  ! SIZE PARAMETERS.

      CALL XVGET(  INFILE, IND, 'FORMAT', FMT, ' ' )   ! BYTES PER PIXEL.
      IND = XVPIXSIZEU( DCODE, FMT, INFILE )

      IF ( XVPTST('ROT180') )         MODE = 0
      IF ( XVPTST('VERT') )           MODE = 1
      IF ( XVPTST('HORIZ') )          MODE = 2
      IF ( XVPTST('CLOCK') )          MODE = 3
      IF ( XVPTST('COUNTER') )        MODE = 4
      IF ( XVPTST('TRANS') )          MODE = 5

      IF ( MODE .LT. 3 )  THEN
           NLO = NL                   ! OUTPUT IMAGE SIZE.
           NSO = NSAMP
      ELSE
           NLO = NSAMP
           NSO = NL
      END IF

C  OPEN OUTPUT FILE.

      CALL XVUNIT( OUTFILE, 'OUT', 1, IND, ' ' )
      CALL XVOPEN( OUTFILE, IND, 'OP', 'WRITE', 'U_NL', NLO,'U_NS', NSO,
     . ' ')

C  USE STACKA TO ALLOCATE BUFFER AND CALL APPROPRIATE ROUTINE.

      IF ( MODE .LT. 3 )   THEN
           NBYT = NSAMP * DCODE                      ! HORIZONTAL OR
           CALL STACKA( 4, FLIP, 1, 2*NBYT, NBYT )   ! VERTICAL FLIP.
      ELSE
           CALL GET_MEM_SIZE(MAXBYT,NSAMP,DCODE)   ! TRY TO INCREASE WORKING SET
                                       ! LEAVE A LITTLE ROOM FOR DISK BUFFERS
                                       ! AND ANYTHING ELSE.
           NBYT = NSAMP * DCODE * ( NL+1 )        ! CLOCKWISE, COUNTERCLOCK.,
           NBYT = MIN0( NBYT, MAXBYT )                        ! (AVOID PAGING.)
           CALL STACKA( 3, ROTATE, 1, NBYT )         ! OR TRANSPOSE.
      END IF

      RETURN
      END
      SUBROUTINE ROTATE(BUF,NBUF)
C ROUTINE TO ROTATE A PICTURE 90 DEGREES CLOCKWISE OR COUNTER-CLOCKWISE
C OR TRANSPOSE

      IMPLICIT INTEGER(A-Z)
      BYTE BUF(1)

      COMMON /C1/    INFILE, OUTFILE, ISL, ISSAMP, NL, NSAMP, NLO, NSO,
     .               DCODE,  MODE

C
C=================START OF EXECUTABLE CODE===============================

C          OBI = OUTPUT BUFFER INDEX
C          LI = LINE INCREMENT
C          SI = SAMPLE INCREMENT
C          NLB = NUMBER OF LINES IN OUTPUT BUFFER
C          NPASS = NUMBER OF PASSES THROUGH INPUT PICTURE

      BUFL = NBUF / DCODE                ! BUFFER LENGTH IN PIXELS.
      NLB =  MIN0( BUFL/(NSO+1),  NLO )
      NPASS = ( NSAMP + NLB -1 ) / NLB

C   BUF BUFFER IS ORGANIZED AS FOLLOWS:
C
C             NLB*DCODE bytes as a XVREAD buffer
C                 |||||||
C
C                 |  |  |  |   NLB lines with NSO pixels per line.
C                 |  |  |  |   Each column in this rectangle is
C                 |  |  |  |   loaded from the XVREAD buffer by 
C                 |  |  |  |   one call to MVE.
C                 |  |  |  |
C                 |  |  |  |
C                 |  |  |  |
C

      IF ( MODE .EQ. 3 )  THEN       ! CLOCKWISE LOOP CONTROL VALUES **
           OBI = (NLB+NSO-1)*DCODE + 1
           LI = NSO
           SI = -DCODE
           ISS = ISSAMP
           ISSINC = NLB
      ELSE IF ( MODE .EQ. 4 ) THEN   ! COUNTER CLOCKWISE LOOP CONTROL VALUES**
           OBI = (NLB+ (NLB-1)*NSO )*DCODE + 1
           LI = -NSO
           SI = DCODE
           ISS = ISSAMP + NSAMP - NLB
           ISSINC = -NLB
      ELSE                           ! **TRANSPOSE MATRIX**
           OBI = (NLB)*DCODE + 1
           LI = NSO
           SI = DCODE
           ISS = ISSAMP
           ISSINC = NLB
      END IF

      NREM = MOD( NSAMP, NLB )
      IF ( NREM .EQ. 0 )   NREM = NLB
      NLB1 = NLB*DCODE + 1

      DO  IPASS = 1, NPASS

          IPT = OBI
          JPT = NLB1
          IF (IPASS .EQ. NPASS)  THEN   ! DO JUST REMAINING LINES ON LAST PASS.
              NLB = NREM
              IF (MODE .EQ. 4)  THEN
                  ISS = ISSAMP
                  IPT = (NLB + (NLB-1)*NSO ) * DCODE  + 1
                  JPT = NLB * DCODE + 1
              END IF
          END IF

          DO  L = 1, NL
              LINE = ISL + L - 1
              CALL XVREAD( INFILE, BUF, IND, 'LINE', LINE, 'SAMP', ISS,
     .                     'NSAMPS', NLB, ' ' )
              CALL MVE( DCODE, NLB, BUF, BUF(IPT), 1, LI )
              IPT = IPT + SI
          END DO

          DO  L = 1, NLB
              CALL XVWRIT( OUTFILE, BUF(JPT), IND, ' ' )
              JPT = JPT + NSO*DCODE
          END DO

          ISS = ISS + ISSINC

      END DO

      RETURN
      END
      SUBROUTINE FLIP(BUF,NBUF, NBYT)
C          ROUTINE TO FLIP A PICTURE ON VERTICAL OR HORIZONTAL AXIS
C          OR ROTATE BY 180 DEGREES.
      IMPLICIT INTEGER(A-Z)
      BYTE BUF(1)
C
      COMMON /C1/    INFILE, OUTFILE, ISL, ISSAMP, NL, NSAMP, NLO, NSO,
     .               DCODE,  MODE

C
C=================START OF EXECUTABLE CODE===============================

      IF(NBUF .LT. 2*NBYT )  THEN
        CALL QPRINT('0**INSUFFICIENT SPACE--STACKA',29)
        CALL ABEND
      END IF

      IPT = 2*NBYT - DCODE + 1

C          IF HORIZONTAL, LINC = 1
C          IF VERTICAL, LINC = -1

      
      IF ( MODE .EQ. 0)  THEN              ! ROTATE 180 DEGREES
           JPT = 1 + NBYT
           LINC = -1
           REC  = ISL + NL - 1
      ELSE IF ( MODE .EQ. 1)  THEN         ! VERT
           JPT = 1
           LINC = -1
           REC  = ISL + NL - 1
      ELSE
           JPT = 1 + NBYT                  ! HORIZ
           LINC = 1
           REC  = ISL 
      END IF
C
      DO 120 L=1,NL
         CALL XVREAD( INFILE, BUF, IND, 'LINE', REC, 'SAMP', ISSAMP,
     .                'NSAMPS', NSAMP, ' ' )
         REC = REC + LINC
         IF(MODE.EQ.2 .OR. MODE .EQ. 0) 
     .       CALL MVE(DCODE,NSAMP,BUF,BUF(IPT),1,-1)
         CALL XVWRIT( OUTFILE, BUF(JPT), IND, ' ' )
120   CONTINUE

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create flot_vms.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C SUBROUTINE TO GET PHYSICAL MEMORY SIZE
        SUBROUTINE GET_MEM_SIZE(MEMSIZE,SAMP,CODE)
        INTEGER MEMSIZE,WSLIM 		! WSA IS ADJUSTMENT FOR BUFFERS ETC 
	INTEGER SAMP, CODE

	CALL SYS$ADJWSL(6000,WSLIM) ! TRY TO INCREASE WORKING SET
	MEMSIZE = MAX(200000, WSLIM*512 - 3*SAMP*CODE)
        RETURN
        END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create flot_unix.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C SUBROUTINE TO GET PHYSICAL MEMORY SIZE
        SUBROUTINE GET_MEM_SIZE(MEMSIZE,SAMP,CODE)
        INTEGER MEMSIZE
	INTEGER SAMP, CODE			  ! DUMMY VARIABLES
        REAL    PMEMORY
		CALL XVPARM('PMEM',PMEMORY,I,K,0) ! GET AMOUNT OF PHYSICAL MEMORY.
        MEMSIZE = PMEMORY*1024*1024       ! CONVERT MEGABYTES TO BYTES
        RETURN
        END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create flot.pdf
process help=*
!  PDF FILE FOR flot
!
PARM INP     TYPE=STRING   COUNT=1
PARM OUT     TYPE=STRING   COUNT=1
!
PARM SIZE    TYPE=INTEGER  COUNT=4   	 DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER  COUNT=0:1     DEFAULT=1
PARM SS      TYPE=INTEGER  COUNT=0:1     DEFAULT=1
PARM NL      TYPE=INTEGER  COUNT=0:1     DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=0:1     DEFAULT=0
!
PARM MODE    TYPE=KEYWORD  COUNT=(0:1)			    +
             VALID=(CLOCK,COUNTER,VERT,HORIZ,TRANS,ROT180)  +
	     DEFAULT=(CLOCK)
!
PARM PMEM    TYPE=REAL    COUNT=1 VALID=(0.1:2047) DEFAULT=16.0
!
END-PROC
.TITLE
flot
.HELP
 PURPOSE:

Program flot is a VICAR applications program which is used to flip or rotate 
an image.  The MODE parameter is used to select the operation that will be
performed on the image.  The six operations available are:
   1) horizontal flip (reflection through vertical axis).
   2) vertical flip (reflection through horizontal axiz).
   3) 90 degrees clockwise rotation.
   4) 90 degrees counterclockwise rotation.
   5) transpose (in the matrix sense).
   6) 180 degrees rotation.
.PAGE
 EXECUTION:

The input image may have any valid data format (byte, halfword, ...).
The data format is obtained from the label of the input file.
The output image has the same data format  (byte or halfword) as the input 
image.  No size restrictions are imposed on the input image.
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      flot INP=a OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      flot INP=a OUT=b  SL=sl SS=ss NL=nl NS=ns optional parameters
      flot a b (sl,ss,nl,ns) optional parameters
      flot a b optional parameters

       Here 'a' represents the input image file name, and
       'b' represents the output image file name.
.PAGE
EXAMPLE

      flot INP=A OUT=B 'CLOCK

      In this example the output file B is produced by rotating the input
      image A 90 degrees in the clockwise direction.
.PAGE
RESTRICTIONS
1. The input file should not be on magnetic tape unless a horizontal flip
   is being performed.

 WRITTEN BY:             Steve Pohorsky              19 Sep 1984

 COGNIZANT PROGRAMMER:   Steve Pohorsky              19 Sep 1984

 REVISION:               2                           19 Sep 1984

 PORTED TO UNIX:	 George A. Madrid Jr.	      9 Jul 1993
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
.VARIABLE MODE
Operation performed on image:
CLOCK,COUNTER,VERT,HORIZ,TRANS,
ROT180
.VARIABLE PMEM
Physical memory available
(megabytes). non-VMS systems
.LEVEL2
.VARIABLE MODE
The MODE parameter is used to select the operation that will be
performed on the image.  The six operations available are:
   1) horizontal flip (reflection through vertical axis).
   2) vertical flip (reflection through horizontal axiz).
   3) 90 degrees clockwise rotation.
   4) 90 degrees counterclockwise rotation.
   5) transpose (in the matrix sense).
   6) 180 degrees rotation.
The default mode is a 90 degrees clockwise rotation.
.VARIABLE PMEM
Physical memory (megabytes) available for flot dynamically allocated buffers.
This applies to non-VMS systems only.  The default is 16.0.  (The maximum value
is 2047 (megabytes), to insure that the number of bytes can be stored as a
32-bit integer.)

This parameter can usually be defaulted.  If the amount of memory available
is less than the default, then PMEM should be set accordingly to prevent the
program from crashing.  If the amount of memory available
is more than the default, then PMEM may be set accordingly to allow larger
images to be handled in one pass.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstflot.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!  THIS IS A TEST OF PROGRAM flot
!
!      byte image - first test defaults.
!
gen FLOTA NL=10 NS=12
flot INP=FLOTA OUT=FLOTAO 
list FLOTAO 
!
!    try SL and SS not equal to 1 with 'HORIZ.
!
flot INP=FLOTA OUT=FLOTAO2 SIZE=(2,3,8,7) 'HORIZ
list FLOTAO2
!
!    try other modes
!
flot FLOTA FLOTAO3 'COUNTER
list FLOTAO3
!
flot FLOTA FLOTAO4 'VERT
list FLOTAO4
!
flot FLOTA FLOTAO5 'ROT180
list FLOTAO5
!
!      halfword image - first test defaults.
!
gen FLOTB NL=10 NS=12 'HALF
flot INP=FLOTB OUT=FLOTBO 
list FLOTBO 
!
!    try SL and SS not equal to 1 with 'VERT.
!
flot INP=FLOTB OUT=FLOTBO2 SIZE=(2,3,8,7) 'VERT
list FLOTBO2
!
!    try other modes
!
flot FLOTB FLOTBO3 'CLOCK
list FLOTBO3
!
flot FLOTB FLOTBO4 'TRANS
list FLOTBO4
!
!      fullword image - first test defaults.
!
gen FLOTC NL=10 NS=12 'FULL
flot INP=FLOTC OUT=FLOTCO 
list FLOTCO 
!
!    try SL and SS not equal to 1 with 'CLOCK.
!
flot INP=FLOTC OUT=FLOTCO2 SIZE=(2,3,8,7) 'CLOCK
list FLOTCO2
!
!    try other modes
!
flot FLOTC FLOTCO3 'COUNTER
list FLOTCO3
!
flot FLOTC FLOTCO4 'VERT
list FLOTCO4
!
!      REAL*4 image - first test defaults.
!
gen FLOTD NL=10 NS=12 'REAL4
flot INP=FLOTD OUT=FLOTDO 
list FLOTDO 
!
!    try SL and SS not equal to 1 with 'HORIZ.
!
flot INP=FLOTD OUT=FLOTDO2 SIZE=(2,3,8,7) 'HORIZ
list FLOTDO2
!
!    try other modes
!
flot FLOTD FLOTDO3 'COUNTER
list FLOTDO3
!
flot FLOTD FLOTDO4 'TRANS
list FLOTDO4
!
!    clean up
!
END-PROC
$ Return
$!#############################################################################
$Imake_File:
$ create flot.imake
#define PROGRAM flot

#if VMS_OS
#define MODULE_LIST flot.f flot_vms.f 
#define CLEAN_OTHER_LIST flot_unix.f 
#else
#define MODULE_LIST flot.f flot_unix.f 
#define CLEAN_OTHER_LIST flot_vms.f 
#endif

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
