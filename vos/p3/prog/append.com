$!****************************************************************************
$!
$! Build proc for MIPL module append
$! VPACK Version 1.9, Wednesday, March 10, 2010, 11:50:42
$!
$! Execute by entering:		$ @append
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
$ write sys$output "*** module append ***"
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
$ write sys$output "Invalid argument given to append.com file -- ", primary
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
$   if F$SEARCH("append.imake") .nes. ""
$   then
$      vimake append
$      purge append.bld
$   else
$      if F$SEARCH("append.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake append
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @append.bld "STD"
$   else
$      @append.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create append.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack append.com -mixed -
	-s append.f -
	-i append.imake -
	-p append.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create append.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C
C  91-04-24   ...REA... CONVERT TO UNIX/VICAR
C  87-12-01   ...REA..  FIX BUG IN BSQ FORMAT PROCESSING
C  84-11-26   ...LWK... CONVERTED TO VICAR2
C MODIFIED FOR VAX CONVERSION BY ASM, 26 AUG 1983
C
      SUBROUTINE MAIN44
C
      IMPLICIT INTEGER (A-Z)
      INTEGER*4 NLI(10), NSI(10), IUNIT(10), OUNIT, NBANDS(10)
      CHARACTER*4 FMT(4)/ 'BYTE', 'HALF', '    ', 'FULL'/, RFMT/ 'REAL'/
      CHARACTER*4 FMT0
      BYTE BUF(100000)
      CHARACTER*80 PRT
      CHARACTER*4 CFMT(4)/ 'BYTE', 'HALF', '    ', 'FULL'/,
     &            FCOD
      CHARACTER*3 ORG(10)
C
C						        GET NUMBER OF INPUTS
      CALL XVPCNT( 'INP', NI)
C
C READ ALL VICAR LABELS TO FIND SIZE OF OUTPUT PIC
C SAVE THE NUMBER OF LINES AND SAMPLES IN EACH INPUT PIC

      NLINE = 0
      ISAMP = 0
      NSAMP = 0
      FLAG = 0		! 0 = UNKNOWN, 1 = INTEGER, 2 = REAL, 3 = MIXED
      BPS = 4		! LARGEST POSSIBLE PIX_SIZE (CURRENTLY)

      DO 300 I=1,NI
      CALL XVUNIT( IUNIT(I), 'INP', I, STAT, ' ')
      CALL XVOPEN( IUNIT(I), STAT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',' ')
      CALL XVGET( IUNIT(I), STAT, 'NL',NLI(I), 'NS',NSI(I), 'PIX_SIZE',
     &         IBYTES, 'FORMAT', FCOD, 'NB',NBANDS(I), 'ORG',ORG(I),' ')
      IF ((FCOD.EQ.'REAL' .AND. FLAG.EQ.1) .OR.
     &	  (FCOD.NE.'REAL' .AND. FLAG.EQ.2)) THEN
	CALL XVMESSAGE('** WARNING: REALS MIXED WITH INTEGERS **',' ')
	FLAG = 3
      ELSEIF (FLAG.NE.3) THEN
	IF (FCOD.EQ.'REAL') THEN
	  FLAG = 2
	ELSE
	  FLAG = 1
	ENDIF
      ENDIF
      NLINE = NLINE+NLI(I)
      NBYT = MAX0( NBYT, NSI(I)*IBYTES)
      BPS = MIN0( BPS, IBYTES)
      NSAMP = MAX0( NSAMP, NBYT/BPS)
  300 CONTINUE
C
C			   CHECK THAT ORGANIZATIONS AND NUMBER OF BANDS ARE SAME
      DO I=2,NI
         IF(ORG(I).NE.ORG(1)) THEN
	    CALL XVMESSAGE(
     +            ' ** ALL INPUTS MUST HAVE SAME ORGANIZATION **',' ')
	    CALL ABEND
	 ENDIF
         IF(NBANDS(I).NE.NBANDS(1)) THEN
	    CALL XVMESSAGE(
     +            ' ** ALL INPUTS MUST HAVE SAME NUMBER BANDS **',' ')
	    CALL ABEND
	 ENDIF
         IF(ORG(1).EQ.'BIP' .AND. NSI(I).NE.NSI(1)) THEN
	    CALL XVMESSAGE(
     + ' FOR BIP ORGANIZATION ALL INPUTS MUST HAVE SAME NUMBER SAMPS',
     + ' ')
	    CALL ABEND
	 ENDIF
      END DO
C         
      NB=NBANDS(1)
C
C OUTPUT FORMAT MAY NOT BE REAL IN MIXED CASE, TO AVOID RESERVED FLOATING
C OPERAND EXCEPTIONS.
C
      NWORDS=(NBYT-1)/4+1
      FMT0 = FMT(BPS)
      IF (FLAG.EQ.2) FMT0 = RFMT
      WRITE (PRT,400) NLINE,NSAMP,FMT0
  400 FORMAT(' OUTPUT: NL=',I6,', NS=',I6,', FORMAT=',A4,'.')
      CALL XVMESSAGE(PRT,' ')
C
C							    OPEN OUTPUT DATA SET
      CALL XVUNIT( OUNIT, 'OUT', 1, STAT, ' ')
      FCOD = CFMT(BPS)
      IF (FLAG.EQ.2) FCOD = 'REAL'
      CALL XVOPEN( OUNIT, STAT, 'OP', 'WRITE', 'U_NL', NLINE, 'U_NS',
     &             NSAMP, 'O_FORMAT', FCOD, 'U_FORMAT', FCOD,
     &            'U_NB',NB,'U_ORG',ORG(1),
     &            'OPEN_ACT', 'SA', 'IO_ACT', 'SA', ' ')

C					       PASTE TOGETHER THE OUTPUT DATASET
      IF (ORG(1).EQ.'BSQ') THEN
	 DO IBAND=1,NB
	    DO I=1,NI
               DO ILINE=1,NLI(I)
                  CALL ZIA(BUF,NWORDS)
          	  CALL XVREAD(IUNIT(I),BUF,STAT,
     &                        'BAND',IBAND,'LINE',ILINE,' ')
     	          CALL XVWRIT(OUNIT,BUF,STAT,' ')
               END DO
            END DO
	 END DO
      ELSE IF(ORG(1) .EQ. 'BIL') THEN
	 DO I=1,NI
            DO ILINE=1,NLI(I)
               DO IBAND=1,NB
                  CALL ZIA(BUF,NWORDS)
         	  CALL XVREAD(IUNIT(I),BUF,STAT,
     &                        'BAND',IBAND,'LINE',ILINE,' ')
    	          CALL XVWRIT(OUNIT,BUF,STAT,' ')
               END DO
            END DO
	 END DO
      ELSE IF(ORG(1) .EQ. 'BIP') THEN
	 DO I=1,NI
            DO ILINE=1,NLI(I)
               DO ISAMP=1,NSI(I)
                 CALL ZIA(BUF,NWORDS)
       	         CALL XVREAD(IUNIT(I),BUF,STAT,'BAND',1,'NBANDS',NB,
     &                       'SAMP',ISAMP,'LINE',ILINE,' ')
  	         CALL XVWRIT(OUNIT,BUF,STAT,' ')
               END DO
            END DO
	 END DO
      END IF
C
C							        CLOSE DATA SETS
      DO I=1,NI
         CALL XVCLOSE( IUNIT(I), STAT, ' ')
      END DO
      CALL XVCLOSE( OUNIT, STAT, ' ')
C
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create append.imake
#define  PROGRAM   append

#define MODULE_LIST append.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
$PDF_File:
$ create append.pdf
process help=*
PARM INP TYPE=STRING COUNT=2:10
PARM OUT TYPE=STRING
END-PROC
.TITLE
VICAR Program APPEND
.HELP
PURPOSE:
APPEND accepts up to ten data sets and writes them out, one after
another, as a single data set.  All 3-D organizations are handled
in the logically correct manner.

EXECUTION:

Example
	APPEND INP=(A,B,C) OUT=D    will write A, B, and C out to D.

Size and parameter fields are not used.

Note:  APPEND outputs the total number of lines and samples written;
if input images are of varying data types, the "number of samples written"
will be the number of samples of the smallest data type. If real input 
is mixed with integer, the data type of the output will be integer.
.PAGE
OPERATION:
After opening all the inputs, APPEND computes the size of the output 
data set. Each input is then copied into the output data set, in the 
same order as specified in the command.  If the inputs are of varying 
sample length, the output lines are padded with zeroes on the end.  
All output lines are of the same length, the length of the longest 
input line.

Currently BYTE, HALF, FULL, and REAL formats are supported.

WRITTEN BY:  Ron Alley, 25 October 1978
COGNIZANT PROGRAMMER:  L. W. Kamp
REVISION:  Converted to Vicar2, 26 November 1984.

.LEVEL1
.VARIABLE INP
STRING - Input image files
.VARIABLE OUT
STRING - Output image file
.LEVEL2
.VARIABLE INP
INP specifies the input data sets.  Up to ten are allowed.
.VARIABLE OUT
OUT specifies the output data set.  The number of lines in the output will 
be the sum of the number of lines in all the inputs.  The number of samples 
will be the maximum of the number of samples of all the inputs.
.END
$ Return
$!#############################################################################
