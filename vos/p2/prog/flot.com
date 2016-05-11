$!****************************************************************************
$!
$! Build proc for MIPL module flot
$! VPACK Version 1.9, Thursday, May 21, 2015, 14:58:30
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
$ vpack flot.com -mixed -
	-s flot.f flot_unix.f -
	-p flot.pdf -
	-t tstflot.pdf tstflot.log -
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
      CHARACTER*32 ORGIN

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

      CALL IFMESSAGE('FLOT version May 21 2015')

      CALL XVEACTION( 'SA', ' ' )

      MODE = 3              ! DEFAULT IS CLOCKWISE.

C  OPEN INPUT FILE

      CALL XVUNIT( INFILE, 'INP', 1, IND, ' ' )
      CALL XVOPEN( INFILE, IND, 'OP', 'READ', ' ' )

      CALL XVSIZE( ISL, ISSAMP, NL, NSAMP, NLI, NSI )  ! SIZE PARAMETERS.
      CALL XVBANDS( SB, NB, NBI)                       ! INPUT BAND COUNT NBI.
      CALL XVP( 'BAND', BAND, BANDSEL)                 ! SELECTED BAND.

      CALL XVGET( INFILE, IND, 'FORMAT', FMT, ' ' )   ! BYTES PER PIXEL.
      CALL XVGET( INFILE, IND, 'ORG', ORGIN, ' ' )    ! INPUT BAND ORG
      IND = XVPIXSIZEU( DCODE, FMT, INFILE )

      IF ( XVPTST('ROT180') )         MODE = 0
      IF ( XVPTST('VERT') )           MODE = 1
      IF ( XVPTST('HORIZ') )          MODE = 2
      IF ( XVPTST('CLOCK') )          MODE = 3
      IF ( XVPTST('COUNTER') )        MODE = 4
      IF ( XVPTST('TRANS') )          MODE = 5

C     IF BAND SELECTED, OUTPUT ONLY THAT BAND
      IF ( BANDSEL .EQ. 1 ) THEN
        NBO = 1               ! OUTPUT BAND COUNT
        FRSTBAND = BAND
        LASTBAND = BAND

        IF ( BAND .LT. 1 .OR. BAND .GT. NBI )  THEN
          CALL QPRINT('BAND OUT OF RANGE',17)
          CALL ABEND
        END IF

        IF ( ORGIN .NE. 'BSQ' ) THEN
          CALL QPRINT('ORG MUST BE BSQ',15)
          CALL ABEND
        END IF

      ELSE
        NBO = NBI
        FRSTBAND = 1
        LASTBAND = NBI
      END IF

C     DEBUGGING INSTRUMENTATION
C      WRITE(*,'(A,I5,A,I5,A,I5,A,I5,A,I5,A,I5)')
C     . 'ISL',ISL,' ISSAMP',ISSAMP,' NL',NL,' NSAMP',NSAMP,
C     . 'NLI', NLI,' NSI',NSI
C      WRITE(*,'(A,I2,A,I2,A,I2,A,I2,A,A)')
C     . 'SB',SB,' NB',NB,' NBI',NBI,' BAND',BAND,' ORG ',ORGIN
C      WRITE(*,'(A,I2,A,I2)') 'PIXSIZE', DCODE, ' MODE', MODE
C      WRITE(*,'(A,I2,A,I2,A,I2)') 'BANDSEL', BANDSEL,
C     . ' FRSTBAND', FRSTBAND, ' LASTBAND', LASTBAND

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
     . 'U_NB', NBO, ' ')

C  USE STACKA TO ALLOCATE BUFFER AND CALL APPROPRIATE ROUTINE.

      IF ( MODE .LT. 3 )   THEN
           NBYT = NSAMP * DCODE                      ! HORIZONTAL OR
           DO BANDI = FRSTBAND, LASTBAND
             CALL STACKA( 5, FLIP, 1, 2*NBYT, NBYT, BANDI ) ! VERTICAL FLIP.
           END DO
      ELSE
           CALL GET_MEM_SIZE(MAXBYT,NSAMP,DCODE)   ! TRY TO INCREASE WORKING SET
                                       ! LEAVE A LITTLE ROOM FOR DISK BUFFERS
                                       ! AND ANYTHING ELSE.
           NBYT = NSAMP * DCODE * ( NL+1 )        ! CLOCKWISE, COUNTERCLOCK.,
           NBYT = MIN0( NBYT, MAXBYT )                    ! (AVOID PAGING.)
           DO BANDI = FRSTBAND, LASTBAND
             CALL STACKA( 4, ROTATE, 1, NBYT, BANDI )       ! OR TRANSPOSE.
           END DO
      END IF

      RETURN
      END
      SUBROUTINE ROTATE(BUF,NBUF,BANDI)
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
     .                     'NSAMPS', NLB, 'BAND', BANDI, ' ' )
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
      SUBROUTINE FLIP(BUF,NBUF, NBYT, BANDI)
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
     .                'NSAMPS', NSAMP, 'BAND', BANDI, ' ' )
         REC = REC + LINC
         IF(MODE.EQ.2 .OR. MODE .EQ. 0) 
     .       CALL MVE(DCODE,NSAMP,BUF,BUF(IPT),1,-1)
         CALL XVWRIT( OUTFILE, BUF(JPT), IND, ' ' )
120   CONTINUE

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
PARM BAND    TYPE=INTEGER  COUNT=0:1     DEFAULT=1
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
image.  No size restrictions are imposed on the input image.  If BAND is
provided, then the output will include only that band.
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
2. BAND must be in the range 1 .. NB
3. Multi-band input must be organized BSQ

 WRITTEN BY:             Steve Pohorsky              19 Sep 1984
 COGNIZANT PROGRAMMER:   Walt Bunch                  May 21 2015

 PROGRAM HISTORY:
  1984-09-19 Steve Pohorsky - Initial version
  1993-07-09 George A. Madrid Jr. - Ported to UNIX
  2015-05-21 Walt Bunch - Added BAND parm and support for multi-band input

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
.VARIABLE BAND
Single band to rotate and output
.VARIABLE MODE
Operation performed on image:
CLOCK,COUNTER,VERT,HORIZ,TRANS,
ROT180
.VARIABLE PMEM
Physical memory available
(megabytes). non-VMS systems
.LEVEL2
.VARIABLE BAND
The BAND parameter, defaulting to 1, must be in the range 0 .. inp NB,
else abend. If not specified, all bands will be rotated and output, so
NB out = NB inp. If BAND is specified, then NB out = 1 and BAND
selects the input band to be rotated and output.
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
!    try multi-band input
!
gen FLOTRED NL=10 NS=12 IVAL=1
gen FLOTGRN NL=10 NS=12 IVAL=101
gen FLOTBLU NL=10 NS=12 IVAL=201
viccub (FLOTRED,FLOTGRN,FLOTBLU) FLOTCOLOR
flot FLOTCOLOR FLOTCOLORROT
list FLOTCOLORROT
flot FLOTCOLOR FLOTCOLORROT BAND=2
list FLOTCOLORROT
!
!    clean up
!
ush rm FLOTA* FLOTB* FLOTC* FLOTD* FLOTG* FLOTR*
END-PROC
$!-----------------------------------------------------------------------------
$ create tstflot.log
                Version 5C/16C

      ***********************************************************
      *                                                         *
      * VICAR Supervisor version 5C, TAE V5.2                   *
      *   Debugger is now supported on all platforms            *
      *   USAGE command now implemented under Unix              *
      *                                                         *
      * VRDI and VIDS now support X-windows and Unix            *
      * New X-windows display program: xvd (for all but VAX/VMS)*
      *                                                         *
      * VICAR Run-Time Library version 16C                      *
      *   '+' form of temp filename now avail. on all platforms *
      *   ANSI C now fully supported                            *
      *                                                         *
      * See B.Deen(RGD059) with problems                        *
      *                                                         *
      ***********************************************************

  --- Type NUT for the New User Tutorial ---

  --- Type MENU for a menu of available applications ---

gen FLOTA NL=10 NS=12
Beginning VICAR task gen
GEN Version 6
GEN task completed
flot INP=FLOTA OUT=FLOTAO
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTAO
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp     1       3       5       7       9
   Line
      1       9   8   7   6   5   4   3   2   1   0
      2      10   9   8   7   6   5   4   3   2   1
      3      11  10   9   8   7   6   5   4   3   2
      4      12  11  10   9   8   7   6   5   4   3
      5      13  12  11  10   9   8   7   6   5   4
      6      14  13  12  11  10   9   8   7   6   5
      7      15  14  13  12  11  10   9   8   7   6
      8      16  15  14  13  12  11  10   9   8   7
      9      17  16  15  14  13  12  11  10   9   8
     10      18  17  16  15  14  13  12  11  10   9
     11      19  18  17  16  15  14  13  12  11  10
     12      20  19  18  17  16  15  14  13  12  11
flot INP=FLOTA OUT=FLOTAO2 SIZE=(2,3,8,7) 'HORIZ
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTAO2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp     1       3       5       7
   Line
      1       9   8   7   6   5   4   3
      2      10   9   8   7   6   5   4
      3      11  10   9   8   7   6   5
      4      12  11  10   9   8   7   6
      5      13  12  11  10   9   8   7
      6      14  13  12  11  10   9   8
      7      15  14  13  12  11  10   9
      8      16  15  14  13  12  11  10
flot FLOTA FLOTAO3 'COUNTER
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTAO3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp     1       3       5       7       9
   Line
      1      11  12  13  14  15  16  17  18  19  20
      2      10  11  12  13  14  15  16  17  18  19
      3       9  10  11  12  13  14  15  16  17  18
      4       8   9  10  11  12  13  14  15  16  17
      5       7   8   9  10  11  12  13  14  15  16
      6       6   7   8   9  10  11  12  13  14  15
      7       5   6   7   8   9  10  11  12  13  14
      8       4   5   6   7   8   9  10  11  12  13
      9       3   4   5   6   7   8   9  10  11  12
     10       2   3   4   5   6   7   8   9  10  11
     11       1   2   3   4   5   6   7   8   9  10
     12       0   1   2   3   4   5   6   7   8   9
flot FLOTA FLOTAO4 'VERT
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTAO4
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp     1       3       5       7       9      11
   Line
      1       9  10  11  12  13  14  15  16  17  18  19  20
      2       8   9  10  11  12  13  14  15  16  17  18  19
      3       7   8   9  10  11  12  13  14  15  16  17  18
      4       6   7   8   9  10  11  12  13  14  15  16  17
      5       5   6   7   8   9  10  11  12  13  14  15  16
      6       4   5   6   7   8   9  10  11  12  13  14  15
      7       3   4   5   6   7   8   9  10  11  12  13  14
      8       2   3   4   5   6   7   8   9  10  11  12  13
      9       1   2   3   4   5   6   7   8   9  10  11  12
     10       0   1   2   3   4   5   6   7   8   9  10  11
flot FLOTA FLOTAO5 'ROT180
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTAO5
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp     1       3       5       7       9      11
   Line
      1      20  19  18  17  16  15  14  13  12  11  10   9
      2      19  18  17  16  15  14  13  12  11  10   9   8
      3      18  17  16  15  14  13  12  11  10   9   8   7
      4      17  16  15  14  13  12  11  10   9   8   7   6
      5      16  15  14  13  12  11  10   9   8   7   6   5
      6      15  14  13  12  11  10   9   8   7   6   5   4
      7      14  13  12  11  10   9   8   7   6   5   4   3
      8      13  12  11  10   9   8   7   6   5   4   3   2
      9      12  11  10   9   8   7   6   5   4   3   2   1
     10      11  10   9   8   7   6   5   4   3   2   1   0
gen FLOTB NL=10 NS=12 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
flot INP=FLOTB OUT=FLOTBO
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTBO
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         9     8     7     6     5     4     3     2     1     0
      2        10     9     8     7     6     5     4     3     2     1
      3        11    10     9     8     7     6     5     4     3     2
      4        12    11    10     9     8     7     6     5     4     3
      5        13    12    11    10     9     8     7     6     5     4
      6        14    13    12    11    10     9     8     7     6     5
      7        15    14    13    12    11    10     9     8     7     6
      8        16    15    14    13    12    11    10     9     8     7
      9        17    16    15    14    13    12    11    10     9     8
     10        18    17    16    15    14    13    12    11    10     9
     11        19    18    17    16    15    14    13    12    11    10
     12        20    19    18    17    16    15    14    13    12    11
flot INP=FLOTB OUT=FLOTBO2 SIZE=(2,3,8,7) 'VERT
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTBO2
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp       1     2     3     4     5     6     7
   Line
      1        10    11    12    13    14    15    16
      2         9    10    11    12    13    14    15
      3         8     9    10    11    12    13    14
      4         7     8     9    10    11    12    13
      5         6     7     8     9    10    11    12
      6         5     6     7     8     9    10    11
      7         4     5     6     7     8     9    10
      8         3     4     5     6     7     8     9
flot FLOTB FLOTBO3 'CLOCK
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTBO3
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         9     8     7     6     5     4     3     2     1     0
      2        10     9     8     7     6     5     4     3     2     1
      3        11    10     9     8     7     6     5     4     3     2
      4        12    11    10     9     8     7     6     5     4     3
      5        13    12    11    10     9     8     7     6     5     4
      6        14    13    12    11    10     9     8     7     6     5
      7        15    14    13    12    11    10     9     8     7     6
      8        16    15    14    13    12    11    10     9     8     7
      9        17    16    15    14    13    12    11    10     9     8
     10        18    17    16    15    14    13    12    11    10     9
     11        19    18    17    16    15    14    13    12    11    10
     12        20    19    18    17    16    15    14    13    12    11
flot FLOTB FLOTBO4 'TRANS
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTBO4
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
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
     11        10    11    12    13    14    15    16    17    18    19
     12        11    12    13    14    15    16    17    18    19    20
gen FLOTC NL=10 NS=12 'FULL
Beginning VICAR task gen
GEN Version 6
GEN task completed
flot INP=FLOTC OUT=FLOTCO
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTCO
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              9          8          7          6          5          4          3          2          1          0
      2             10          9          8          7          6          5          4          3          2          1
      3             11         10          9          8          7          6          5          4          3          2
      4             12         11         10          9          8          7          6          5          4          3
      5             13         12         11         10          9          8          7          6          5          4
      6             14         13         12         11         10          9          8          7          6          5
      7             15         14         13         12         11         10          9          8          7          6
      8             16         15         14         13         12         11         10          9          8          7
      9             17         16         15         14         13         12         11         10          9          8
     10             18         17         16         15         14         13         12         11         10          9
     11             19         18         17         16         15         14         13         12         11         10
     12             20         19         18         17         16         15         14         13         12         11
flot INP=FLOTC OUT=FLOTCO2 SIZE=(2,3,8,7) 'CLOCK
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTCO2
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp            1          2          3          4          5          6          7          8
   Line
      1             10          9          8          7          6          5          4          3
      2             11         10          9          8          7          6          5          4
      3             12         11         10          9          8          7          6          5
      4             13         12         11         10          9          8          7          6
      5             14         13         12         11         10          9          8          7
      6             15         14         13         12         11         10          9          8
      7             16         15         14         13         12         11         10          9
flot FLOTC FLOTCO3 'COUNTER
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTCO3
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1             11         12         13         14         15         16         17         18         19         20
      2             10         11         12         13         14         15         16         17         18         19
      3              9         10         11         12         13         14         15         16         17         18
      4              8          9         10         11         12         13         14         15         16         17
      5              7          8          9         10         11         12         13         14         15         16
      6              6          7          8          9         10         11         12         13         14         15
      7              5          6          7          8          9         10         11         12         13         14
      8              4          5          6          7          8          9         10         11         12         13
      9              3          4          5          6          7          8          9         10         11         12
     10              2          3          4          5          6          7          8          9         10         11
     11              1          2          3          4          5          6          7          8          9         10
     12              0          1          2          3          4          5          6          7          8          9
flot FLOTC FLOTCO4 'VERT
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTCO4
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp            1          2          3          4          5          6          7          8          9         10
   Line
      1              9         10         11         12         13         14         15         16         17         18
      2              8          9         10         11         12         13         14         15         16         17
      3              7          8          9         10         11         12         13         14         15         16
      4              6          7          8          9         10         11         12         13         14         15
      5              5          6          7          8          9         10         11         12         13         14
      6              4          5          6          7          8          9         10         11         12         13
      7              3          4          5          6          7          8          9         10         11         12
      8              2          3          4          5          6          7          8          9         10         11
      9              1          2          3          4          5          6          7          8          9         10
     10              0          1          2          3          4          5          6          7          8          9

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp           11         12
   Line
      1             19         20
      2             18         19
      3             17         18
      4             16         17
      5             15         16
      6             14         15
      7             13         14
      8             12         13
      9             11         12
     10             10         11
gen FLOTD NL=10 NS=12 'REAL4
Beginning VICAR task gen
GEN Version 6
GEN task completed
flot INP=FLOTD OUT=FLOTDO
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTDO
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:14 2015
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       9.000E+00   8.000E+00   7.000E+00   6.000E+00   5.000E+00   4.000E+00   3.000E+00   2.000E+00   1.000E+00   0.000E+00
      2       1.000E+01   9.000E+00   8.000E+00   7.000E+00   6.000E+00   5.000E+00   4.000E+00   3.000E+00   2.000E+00   1.000E+00
      3       1.100E+01   1.000E+01   9.000E+00   8.000E+00   7.000E+00   6.000E+00   5.000E+00   4.000E+00   3.000E+00   2.000E+00
      4       1.200E+01   1.100E+01   1.000E+01   9.000E+00   8.000E+00   7.000E+00   6.000E+00   5.000E+00   4.000E+00   3.000E+00
      5       1.300E+01   1.200E+01   1.100E+01   1.000E+01   9.000E+00   8.000E+00   7.000E+00   6.000E+00   5.000E+00   4.000E+00
      6       1.400E+01   1.300E+01   1.200E+01   1.100E+01   1.000E+01   9.000E+00   8.000E+00   7.000E+00   6.000E+00   5.000E+00
      7       1.500E+01   1.400E+01   1.300E+01   1.200E+01   1.100E+01   1.000E+01   9.000E+00   8.000E+00   7.000E+00   6.000E+00
      8       1.600E+01   1.500E+01   1.400E+01   1.300E+01   1.200E+01   1.100E+01   1.000E+01   9.000E+00   8.000E+00   7.000E+00
      9       1.700E+01   1.600E+01   1.500E+01   1.400E+01   1.300E+01   1.200E+01   1.100E+01   1.000E+01   9.000E+00   8.000E+00
     10       1.800E+01   1.700E+01   1.600E+01   1.500E+01   1.400E+01   1.300E+01   1.200E+01   1.100E+01   1.000E+01   9.000E+00
     11       1.900E+01   1.800E+01   1.700E+01   1.600E+01   1.500E+01   1.400E+01   1.300E+01   1.200E+01   1.100E+01   1.000E+01
     12       2.000E+01   1.900E+01   1.800E+01   1.700E+01   1.600E+01   1.500E+01   1.400E+01   1.300E+01   1.200E+01   1.100E+01
flot INP=FLOTD OUT=FLOTDO2 SIZE=(2,3,8,7) 'HORIZ
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTDO2
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:15 2015
     Samp             1           2           3           4           5           6           7
   Line
      1       9.000E+00   8.000E+00   7.000E+00   6.000E+00   5.000E+00   4.000E+00   3.000E+00
      2       1.000E+01   9.000E+00   8.000E+00   7.000E+00   6.000E+00   5.000E+00   4.000E+00
      3       1.100E+01   1.000E+01   9.000E+00   8.000E+00   7.000E+00   6.000E+00   5.000E+00
      4       1.200E+01   1.100E+01   1.000E+01   9.000E+00   8.000E+00   7.000E+00   6.000E+00
      5       1.300E+01   1.200E+01   1.100E+01   1.000E+01   9.000E+00   8.000E+00   7.000E+00
      6       1.400E+01   1.300E+01   1.200E+01   1.100E+01   1.000E+01   9.000E+00   8.000E+00
      7       1.500E+01   1.400E+01   1.300E+01   1.200E+01   1.100E+01   1.000E+01   9.000E+00
      8       1.600E+01   1.500E+01   1.400E+01   1.300E+01   1.200E+01   1.100E+01   1.000E+01
flot FLOTD FLOTDO3 'COUNTER
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTDO3
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:15 2015
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
      2       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
      3       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
      4       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
      5       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      6       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      7       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      8       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      9       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
     10       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
     11       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
     12       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
flot FLOTD FLOTDO4 'TRANS
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTDO4
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:14 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:15 2015
     Samp             1           2           3           4           5           6           7           8           9          10
   Line
      1       0.000E+00   1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00
      2       1.000E+00   2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01
      3       2.000E+00   3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01
      4       3.000E+00   4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01
      5       4.000E+00   5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01
      6       5.000E+00   6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01
      7       6.000E+00   7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01
      8       7.000E+00   8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01
      9       8.000E+00   9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01
     10       9.000E+00   1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01
     11       1.000E+01   1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01
     12       1.100E+01   1.200E+01   1.300E+01   1.400E+01   1.500E+01   1.600E+01   1.700E+01   1.800E+01   1.900E+01   2.000E+01
gen FLOTRED NL=10 NS=12 IVAL=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FLOTGRN NL=10 NS=12 IVAL=101
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen FLOTBLU NL=10 NS=12 IVAL=201
Beginning VICAR task gen
GEN Version 6
GEN task completed
viccub (FLOTRED,FLOTGRN,FLOTBLU) FLOTCOLOR
Beginning VICAR task viccub
flot FLOTCOLOR FLOTCOLORROT
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTCOLORROT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:15 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:15 2015
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9
   Line
      1      10   9   8   7   6   5   4   3   2   1
      2      11  10   9   8   7   6   5   4   3   2
      3      12  11  10   9   8   7   6   5   4   3
      4      13  12  11  10   9   8   7   6   5   4
      5      14  13  12  11  10   9   8   7   6   5
      6      15  14  13  12  11  10   9   8   7   6
      7      16  15  14  13  12  11  10   9   8   7
      8      17  16  15  14  13  12  11  10   9   8
      9      18  17  16  15  14  13  12  11  10   9
     10      19  18  17  16  15  14  13  12  11  10
     11      20  19  18  17  16  15  14  13  12  11
     12      21  20  19  18  17  16  15  14  13  12


 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:15 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:15 2015
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9
   Line
      1     110 109 108 107 106 105 104 103 102 101
      2     111 110 109 108 107 106 105 104 103 102
      3     112 111 110 109 108 107 106 105 104 103
      4     113 112 111 110 109 108 107 106 105 104
      5     114 113 112 111 110 109 108 107 106 105
      6     115 114 113 112 111 110 109 108 107 106
      7     116 115 114 113 112 111 110 109 108 107
      8     117 116 115 114 113 112 111 110 109 108
      9     118 117 116 115 114 113 112 111 110 109
     10     119 118 117 116 115 114 113 112 111 110
     11     120 119 118 117 116 115 114 113 112 111
     12     121 120 119 118 117 116 115 114 113 112


 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:15 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:15 2015
 ***********
 Band =     3
 ***********
     Samp     1       3       5       7       9
   Line
      1     210 209 208 207 206 205 204 203 202 201
      2     211 210 209 208 207 206 205 204 203 202
      3     212 211 210 209 208 207 206 205 204 203
      4     213 212 211 210 209 208 207 206 205 204
      5     214 213 212 211 210 209 208 207 206 205
      6     215 214 213 212 211 210 209 208 207 206
      7     216 215 214 213 212 211 210 209 208 207
      8     217 216 215 214 213 212 211 210 209 208
      9     218 217 216 215 214 213 212 211 210 209
     10     219 218 217 216 215 214 213 212 211 210
     11     220 219 218 217 216 215 214 213 212 211
     12     221 220 219 218 217 216 215 214 213 212
flot FLOTCOLOR FLOTCOLORROT BAND=2
Beginning VICAR task flot
FLOT version May 21 2015
list FLOTCOLORROT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:wlb       Date_Time:Thu May 21 14:58:15 2015
 Task:FLOT      User:wlb       Date_Time:Thu May 21 14:58:15 2015
     Samp     1       3       5       7       9
   Line
      1     110 109 108 107 106 105 104 103 102 101
      2     111 110 109 108 107 106 105 104 103 102
      3     112 111 110 109 108 107 106 105 104 103
      4     113 112 111 110 109 108 107 106 105 104
      5     114 113 112 111 110 109 108 107 106 105
      6     115 114 113 112 111 110 109 108 107 106
      7     116 115 114 113 112 111 110 109 108 107
      8     117 116 115 114 113 112 111 110 109 108
      9     118 117 116 115 114 113 112 111 110 109
     10     119 118 117 116 115 114 113 112 111 110
     11     120 119 118 117 116 115 114 113 112 111
     12     121 120 119 118 117 116 115 114 113 112
ush rm FLOTA* FLOTB* FLOTC* FLOTD* FLOTG* FLOTR*
END-PROC
$ Return
$!#############################################################################
$Imake_File:
$ create flot.imake
#define PROGRAM flot

#define MODULE_LIST flot.f flot_unix.f 

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
