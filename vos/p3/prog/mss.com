$!****************************************************************************
$!
$! Build proc for MIPL module mss
$! VPACK Version 1.5, Monday, March 29, 1993, 14:50:40
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
	-p mss.pdf -
	-i mss.imake
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
C     8-26-83  ASM MODIFIED FOR VAX CONVERSION 
C     8-10-84  SP  CONVERTED TO USE VICAR2 CALLS.
C     8-10-84  SP  CHANGED TO HANDLE SL NOT EQUAL TO 1.
C     8-10-84  SP  CHANGED TO HANDLE UP TO 50 INPUT FILES.
      INCLUDE 'VICMAIN_FOR'
C**********************************************************************
C
C     'MSS'   PICTURE INTERLEAVING PROGRAM
C     MSS will put up to 50 input images side-by-side, left-to-right.
C
      SUBROUTINE MAIN44
      EXTERNAL WORK
      INTEGER IDSN(50), OUTFILE 
      INTEGER NS(50),LOC(50)
      LOGICAL*1 QSIZE
      CHARACTER*80 BUF
C============START OF EXECUTABLE CODE==========================

      CALL XVPCNT( 'INP', NIN )
      IF (NIN.GT.30) THEN
	 CALL XVMESSAGE(' ** MAXIMUM # INPUT FILES = 50',' ')
	 CALL ABEND
      ENDIF

C  OPEN DATA SETS AND GET LENGTH OF LINE FOR EACH FILE.
      DO I = 1,NIN
         CALL XVUNIT( IDSN(I), 'INP', I, IND, 0 )
         CALL XVOPEN( IDSN(I), IND, 'OPEN_ACT','SA','IO_ACT','SA', 0)
         CALL XVGET( IDSN(I), IND, 'NS', NS(I), 0)
      END DO

      CALL XVGET( IDSN(1), IND, 'PIX_SIZE', IPIXSIZE, 0)
      CALL XVSIZE( ISL, ISSAMP, NL, NS1, NLL, NSL)
      QSIZE = NS1 .NE. NSL
C
C     COMPUTE OUTPUT LINE LENGTH
C
      N = 1
      NSO = 0

      DO 100 I=1,NIN
        IF (QSIZE)   NS(I)=NS1
        LOC(I) = N
        N = N+NS(I)*IPIXSIZE
        NSO = NSO + NS(I)
  100 CONTINUE

C   OPEN OUTPUT FILE

      CALL XVUNIT( OUTFILE, 'OUT', 1, IND, 0)
      CALL XVOPEN(OUTFILE,IND,'OP','WRITE','U_NL',NL,'U_NS',NSO,
     +		  'OPEN_ACT','SA','IO_ACT','SA','U_NB',1,0)
C
C     REPORT NSO,NIN AND CALL STACKA
C
      WRITE (BUF,200) NIN
  200 FORMAT(' ** OUTPUT CONTAINS',I3,' INTERLEAVED DATA SETS **')
      CALL XVMESSAGE(BUF,' ')
      WRITE (BUF,300) NSO
  300 FORMAT(' ** ACTUAL OUTPUT RECORD LENGTH',I6,' SAMPLES **')
      CALL XVMESSAGE(BUF,' ')
      CALL STACKA(12,WORK,1,N,NL,NIN,NBYT,NS,LOC,IDSN,ISL,ISSAMP,
     .            OUTFILE)
      RETURN
      END
C**********************************************************************
      SUBROUTINE WORK(OUT,NN,NL,NIN,NBYT,NS,LOC,IDSN,ISL,ISSAMP,OUTFILE)
      INTEGER NS(NIN),LOC(NIN),IDSN(NIN)
      LOGICAL*1 OUT(*)
C
      IF(NN .LT. NBYT) GO TO 800
C
      DO I= ISL, ISL + NL - 1
          DO J=1,NIN
              CALL XVREAD( IDSN(J), OUT( LOC(J) ), IND, 'LINE', I,
     .                'SAMP', ISSAMP, 'NSAMPS', NS(J), 0)
          END DO
          CALL XVWRIT( OUTFILE, OUT, IND, 0)
      END DO
      RETURN
C
  800 CONTINUE
      CALL XVMESSAGE(' NOT ENOUGH SPACE FOR STACKA BUFFERS',' ')
      CALL ABEND
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create mss.pdf
process help=*
PARM INP TYPE=STRING COUNT=2:50
PARM OUT TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
END-PROC
.TITLE
MSS
.HELP
PURPOSE:
MSS combines up to 50 datasets into a single dataset with MSS format.
This is equivalent to concatenating the input images in a left to right
fashion.

EXECUTION:

Example

MSS INP=(A,B,C) OUT=D  will put images A, B, and C side-by-side to form D.

If the size parameter is used (SIZE=(SL,SS,NL,NS)), only the defined area in
each input image will be used to create the new file.  


OPERATION:
MSS combines datasets in the following manner:  
Each line is made up of the corresponding input lines laid end to end in
a concatenated manner.  That is, the first pixel of each input is placed
to the right of the last pixel of the previous input.  The line thus
formed will have the same number of samples per line as the sum of the
inputs. (If the SIZE field is used, it will be NS * #-inputs.)

WRITTEN BY:  J.D. Addington, 23 July 1974
COGNIZANT PROGRAMMER:  Ron Alley
DOCUMENTATION AUTHOR:  J.D. Addington
REVISION:  1, 25 January 1982

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
INP specifies the input data sets.  Up to 50 are allowed.
.VARIABLE SIZE
The SIZE parameter may be used when only a sub-region of each image is to
be concatenated; it has the format SIZE=(SL,SS,NL,NS), where the parameters
are starting line, starting sample, number of lines, and number of samples,
respectively.  SIZE=(1,1,10,10), for example, will cause MSS to only look
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
$Imake_File:
$ create mss.imake
#define  PROGRAM   mss

#define MODULE_LIST mss.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
