$!****************************************************************************
$!
$! Build proc for MIPL module flot
$! VPACK Version 1.8, Wednesday, February 04, 1998, 02:15:12
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
$ write sys$output "Invalid argument given to flot.com file -- ", primary
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
	-s flot.f -
	-p flot.pdf -
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
C     4-91  REA  CONVERTED TO UNIX/VICAR
C     2-98  REA  ADDED 3-D FILE CAPABILITY
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
C      SUN4 SunOS/UNIX  with TAE v4.1/VICAR
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  CALLED BY
C      FLOT
C
C*   27 JAN 77   ... GMY ...  INITIAL RELEASE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT INTEGER(A-Z)

      LOGICAL XVPTST                       ! DECLARES XVPTST A LOG. FUNCTION.
      CHARACTER*3 ORG
      EXTERNAL FLIP,ROTATE

      COMMON /C1/    INFILE, OUTFILE, ISL, ISSAMP, NL, NSAMP, NLO, NSO,
     .               DCODE,  MODE, NBANDS

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

      MODE = 3              ! DEFAULT IS CLOCKWISE.

C  OPEN INPUT FILE

      CALL XVUNIT( INFILE, 'INP', 1, IND, ' ')
      CALL XVOPEN( INFILE, IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     .             'IO_ACT', 'SA', ' ')
      CALL XVSIZE( ISL, ISSAMP, NL, NSAMP, NLIN, NSIN)       ! SIZE PARAMETERS.
      CALL XVGET(INFILE,IND,'PIX_SIZE',DCODE,'NB',NBANDS,'ORG',ORG,' ')
C
      IF (ORG .EQ. 'BIP') THEN
	 CALL XVMESSAGE('FLOT does not support BIP format',' ')
	 CALL ABEND
      ENDIF
C
      IF ( XVPTST('ROT180') )         MODE = 0
      IF ( XVPTST('VERT') )           MODE = 1
      IF ( XVPTST('HORIZ') )          MODE = 2
      IF ( XVPTST('CLOCK') )          MODE = 3
      IF ( XVPTST('COUNTER') )        MODE = 4
      IF ( XVPTST('TRANS') )          MODE = 5
C
      IF ( MODE .LT. 3 )  THEN
           NLO = NL                   ! OUTPUT IMAGE SIZE.
           NSO = NSAMP
      ELSE
           NLO = NSAMP
           NSO = NL
      END IF

C  OPEN OUTPUT FILE.

      CALL XVUNIT( OUTFILE, 'OUT', 1, IND, ' ')
      CALL XVOPEN( OUTFILE, IND, 'OP', 'WRITE', 'U_NL', NLO,'U_NS', NSO,
     .             'U_ORG','BSQ','OPEN_ACT', 'SA', 'IO_ACT', 'SA', ' ')

C  USE STACKA TO ALLOCATE BUFFER AND CALL APPROPRIATE ROUTINE.

      IF ( MODE .LT. 3 )   THEN
           NBYT = NSAMP * DCODE                     ! HORIZONTAL OR
           CALL STACKA(4, FLIP, 1, 2*NBYT, NBYT )   ! VERTICAL FLIP.
      ELSE
           NBYT = NSAMP * DCODE * ( NL+1 )          ! CLOCKWISE, COUNTERCLOCK.,
           NBYT = MIN0( NBYT, 200000 )                         ! (AVOID PAGING.)
           CALL STACKA(3, ROTATE, 1, NBYT )         ! OR TRANSPOSE.
      END IF

      RETURN
      END
C******************************************************************************
      SUBROUTINE ROTATE(BUF,NBUF)
C ROUTINE TO ROTATE A PICTURE 90 DEGREES CLOCKWISE OR COUNTER-CLOCKWISE
C OR TRANSPOSE

      IMPLICIT INTEGER(A-Z)
      LOGICAL*1 BUF(*)

      COMMON /C1/    INFILE, OUTFILE, ISL, ISSAMP, NL, NSAMP, NLO, NSO,
     .               DCODE,  MODE, NBANDS

C
C=================START OF EXECUTABLE CODE===============================
C
      DO IBAND = 1,NBANDS
C
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
          ELSE IF ( MODE .EQ. 4 ) THEN   !COUNTERCLOCKWISE LOOP CONTROL VALUES**
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
C
          NREM = MOD( NSAMP, NLB )
          IF ( NREM .EQ. 0 )   NREM = NLB
          NLB1 = NLB*DCODE + 1
C
          DO  IPASS = 1, NPASS
C
              IPT = OBI
              JPT = NLB1
              IF (IPASS .EQ. NPASS) THEN  ! DO JUST REMAINING LINES ON LAST PASS
                  NLB = NREM
                  IF (MODE .EQ. 4)  THEN
                      ISS = ISSAMP
                      IPT = (NLB + (NLB-1)*NSO ) * DCODE  + 1
                      JPT = NLB * DCODE + 1
                  END IF
              END IF
C
              DO  L = 1, NL
                  LINE = ISL + L - 1
                  CALL XVREAD(INFILE,BUF,IND,'BAND',IBAND,'LINE',LINE,
     +                        'SAMP',ISS,'NSAMPS',NLB,' ')
                  CALL MVE(DCODE, NLB, BUF, BUF(IPT), 1, LI )
                  IPT = IPT + SI
              END DO
C
              DO  L = 1, NLB
                  CALL XVWRIT( OUTFILE, BUF(JPT), IND, ' ')
                  JPT = JPT + NSO*DCODE
              END DO
C
              ISS = ISS + ISSINC
          END DO
C
      END DO
C
      RETURN
      END
C*******************************************************************************
      SUBROUTINE FLIP(BUF,NBUF, NBYT)
C          ROUTINE TO FLIP A PICTURE ON VERTICAL OR HORIZONTAL AXIS
C          OR ROTATE BY 180 DEGREES.
      IMPLICIT INTEGER(A-Z)
      LOGICAL*1 BUF(*)
C
      COMMON /C1/    INFILE, OUTFILE, ISL, ISSAMP, NL, NSAMP, NLO, NSO,
     .               DCODE,  MODE, NBANDS

C
C=================START OF EXECUTABLE CODE===============================

      IF(NBUF .LT. 2*NBYT ) THEN
	 CALL XVMESSAGE(' **INSUFFICIENT SPACE--STACKA',' ')
	 CALL ABEND
      ENDIF
      IPT = 2*NBYT - DCODE + 1

C          IF HORIZONTAL, LINC = 1
C          IF VERTICAL, LINC = -1

      DO IBAND=1,NBANDS
      
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
          DO L=1,NL
              CALL XVREAD(INFILE,BUF,IND,'BAND',IBAND,'LINE',REC,
     +                    'SAMP',ISSAMP,'NSAMPS',NSAMP,' ')
              REC = REC + LINC
              IF(MODE.EQ.2 .OR. MODE .EQ. 0) 
     .                       CALL MVE(DCODE,NSAMP,BUF,BUF(IPT),1,-1)
              CALL XVWRIT( OUTFILE, BUF(JPT), IND, ' ')
          END DO
      END DO
C
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create flot.pdf
process help=*
!  PDF FILE FOR FLOT
!
PARM INP     TYPE=STRING   COUNT=1
PARM OUT     TYPE=STRING   COUNT=1
!
PARM SIZE    TYPE=INTEGER  COUNT=(0:4)   DEFAULT=--
PARM SL      TYPE=INTEGER  COUNT=0:1     DEFAULT=--
PARM SS      TYPE=INTEGER  COUNT=0:1     DEFAULT=--
PARM NL      TYPE=INTEGER  COUNT=0:1     DEFAULT=--
PARM NS      TYPE=INTEGER  COUNT=0:1     DEFAULT=--
!
PARM MODE    TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=CLOCK    +
             VALID=(CLOCK,COUNTER,VERT,HORIZ,TRANS,ROT180)
!
END-PROC
.TITLE
FLOT
.HELP
 PURPOSE:

Program FLOT is a VICAR applications program which is used to flip or rotate 
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

The input image may have any valid data format (byte, halfword, ...),
and either BSQ or BIL data organization.  The data format is obtained from
the label of the input file, and the output image organization is always BSQ.
The output image has the same data format  (byte or halfword) as the input 
image.  No size restrictions are imposed on the input image.
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      FLOT INP=a OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      FLOT INP=a OUT=b  SL=sl SS=ss NL=nl NS=ns optional parameters
      FLOT a b (sl,ss,nl,ns) optional parameters
      FLOT a b optional parameters

       Here 'a' represents the input image file name, and
       'b' represents the output image file name.
.PAGE
EXAMPLE

      FLOT INP=A OUT=B 'CLOCK

      In this example the output file B is produced by rotating the input
      image A 90 degrees in the clockwise direction.
.PAGE
RESTRICTIONS
1. The input file should not be on magnetic tape unless a horizontal flip
   is being performed.
2. BIP input file organization is not supported.  Output file organization
   is BSQ regardless of input.

 WRITTEN BY:             Steve Pohorsky              19 Sep 1984

 COGNIZANT PROGRAMMER:   Ron Alley                    4 Feb 1998

 REVISION:               2                           19 Sep 1984
.LEVEL1
.VARIABLE INP
Input file name
.VARIABLE OUT
Output file name
.VARIABLE SIZE
Vicar size field for input:
Same as  (SL,SS,NL,NS)
By default, the entire input
image is used if these
parameters are not entered.
.VARIABLE SL
Starting line of input
.VARIABLE SS
Starting sample of input
.VARIABLE NL
Number of lines from input
.VARIABLE NS
Number of samples from input
.VARIABLE MODE
Operation performed on image:
CLOCK,COUNTER,VERT,HORIZ,TRANS,
ROT180
.LEVEL2
.VARIABLE SIZE
   This is the standard VICAR size field with the following exception: the
location refers to the input image, not the output.
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
.END
$ Return
$!#############################################################################
$Imake_File:
$ create flot.imake
#define  PROGRAM   flot

#define MODULE_LIST flot.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
