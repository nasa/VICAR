$!****************************************************************************
$!
$! Build proc for MIPL module genthis
$! VPACK Version 1.5, Monday, March 29, 1993, 14:50:37
$!
$! Execute by entering:		$ @genthis
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
$ write sys$output "*** module genthis ***"
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
$   if F$SEARCH("genthis.imake") .nes. ""
$   then
$      vimake genthis
$      purge genthis.bld
$   else
$      if F$SEARCH("genthis.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake genthis
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @genthis.bld "STD"
$   else
$      @genthis.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create genthis.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack genthis.com -
	-s genthis.f -
	-p genthis.pdf -
	-i genthis.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create genthis.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C#######################################################################
C  NAME OF ROUTINE
C      GENTHIS
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      GENTHIS generates small exactly-defined test files.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    APRIL 1986
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C
C  DERIVED FROM CODE FOR VICAR PROGRAM GEN
C
C     
C  REVISION HISTORY
C      Converted to UNIX/VICAR May 2, 1991  ---  Ron Alley
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

C           GENTHIS A NL NS PARAMS

C     GENTHIS DOES NOT DISTINGUISH BETWEEN INTEGER AND REAL 
C     NUMBER DN VALUES. ALL VALUES ARE NOW PROCESSED AS REAL NUMBERS. 

      CALL XVMESSAGE(' GENTHIS Version 1.1',' ')
      CALL GENTHISLAB
      CALL GENTHIS1

      RETURN
      END
C*****************************************************************************
      SUBROUTINE GENTHISLAB

      IMPLICIT NONE

      COMMON /C1XX/ NL,NS,PIXSIZ,NCODE,BUF
      INTEGER*4 NL,NS,PIXSIZ,NCODE
      LOGICAL*1 BUF(80000)

      COMMON /FORMATS/ DATA
      COMMON /UNIT/ OUTUNIT
C
      INTEGER DEF,CNT,STATUS,OUTUNIT, I
      REAL PAR(1000)
      INTEGER*4 KAR(1000)
      EQUIVALENCE ( PAR,KAR )
      CHARACTER*5 FORMAT, DATA

C          Label processor
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')

C--- Determine data type and pixel size and add to label
      CALL XVP('FORMAT',DATA,CNT)

      IF (DATA .EQ. 'HALF ') THEN
	FORMAT(1:4) = 'HALF'
        NCODE = -6
        PIXSIZ = 2
      ELSE IF (DATA .EQ. 'FULL ') THEN
	FORMAT(1:4) = 'FULL'
        NCODE = 4
        PIXSIZ = 4
      ELSE IF (DATA .EQ. 'REAL4' .OR. DATA.EQ.'REAL') THEN
	FORMAT(1:4) = 'REAL'
        NCODE = 4
        PIXSIZ = 4
      ELSE IF (DATA .EQ. 'REAL8') THEN
	FORMAT(1:4) = 'DOUB'
        NCODE = 9
        PIXSIZ = 8
      ELSE 
	FORMAT(1:4) = 'BYTE'
        NCODE = -5
        PIXSIZ = 1
      END IF

C--- Open output file with specified values
      CALL XVPARM('NL',NL,CNT,DEF,0)
      CALL XVPARM('NS',NS,CNT,DEF,0)
      CALL XVOPEN(OUTUNIT,STATUS,'U_FORMAT',FORMAT,'O_FORMAT',
     +            FORMAT,'OP','WRITE','U_NL',NL,'U_NS',NS,
     +		  'IO_ACT','SA','OPEN_ACT','SA',' ')

C--- Get DN VALUES.
      CALL XVP('DN', PAR, CNT)
      IF (CNT .NE. NL*NS) THEN
         CALL XVMESSAGE(
     +            ' **PARAMETER ERR...DN COUNT DOES NOT MATCH SIZE',' ')
         CALL XVMESSAGE(' **GENTHIS TASK CANCELLED',' ')
	 CALL ABEND
      END IF
      
C--- CONVERT DN VALUES TO SPECIFIED FORMAT

      IF (DATA(:1).EQ.'B' .OR. DATA(:1).EQ.'H' .OR. 
     +                                  DATA(:1).EQ.'F') THEN
        DO I = 1, CNT
           KAR(I) = PAR(I)          ! CONVERT TO INTEGER.
        END DO
      END IF
      CALL MVE (NCODE, CNT, PAR, BUF, 1, 1 )

      RETURN
      END
C************************************************************************
      SUBROUTINE GENTHIS1

      IMPLICIT NONE

      COMMON /C1XX/ NL,NS,PIXSIZ,NCODE,BUF
      INTEGER*4 NL,NS,PIXSIZ,NCODE
      LOGICAL*1 BUF(80000)

      COMMON /FORMATS/ DATA
      CHARACTER*5 DATA
      COMMON /UNIT/ OUTUNIT
      INTEGER OUTUNIT,STATUS

      INTEGER I, N, NBYTES

        NBYTES = NS* PIXSIZ
        N = 1

	DO I = 1, NL
	  CALL XVWRIT(OUTUNIT,BUF(N),STATUS,' ')
          N = N + NBYTES
	END DO

C--- Close output file
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      CALL XVMESSAGE(' GENTHIS TASK COMPLETED',' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create genthis.pdf
PROCESS HELP=*
PARM OUT    TYPE=STRING		               DEFAULT=GENTHIS
PARM NL     TYPE=INTEGER		       DEFAULT=10
PARM NS     TYPE=INTEGER		       DEFAULT=10
PARM FORMAT TYPE=KEYWORD VALID=(BYTE,HALF,FULL,REAL,REAL4,REAL8) +
 DEFAULT=BYTE
PARM DN     TYPE=REAL     COUNT=(1:300)
END-PROC
.TITLE
VICAR2 Program GENTHIS -- generates small exactly-defined test files.
.HELP

PURPOSE:
 GENTHIS generates a picture, given a list of DN values.  The list contains
 the DNs for each pixel in the picture.

EXECUTION:

	GENTHIS can be invoked by printing
		GENTHIS OUT NL NS PARAMS
	where PARAMS consists of the following parameters:
		FORMAT	DN
	Each is described in their respective  parameter section.


WRITTEN BY:             Steve Pohorsky                          4-14-86

COGNIZANT PROGRAMMER:   Steve Pohorsky
.LEVEL1
.VARIABLE NL
Number of Lines
.VARIABLE NS
Number of samples.
.VARIABLE OUT
Output filename.
.VARIABLE FORMAT
Data format. Valid: 
HALF,FULL,REAL(4),REAL8.
.VARIABLE DN
List of DNs for the output file.
.LEVEL2
.VARIABLE NL
NL (integer) specifies the size of the image in the line direction. Default
is 10.
.VARIABLE NS
NS (integer) specifies the size of the image in the sample direction, i.e.,
the number of samples per line. Default is 10.
.VARIABLE OUT
OUT is the standard VICAR output filename. It is a string of form
"name.type",where "name" is a string of alphanumeric characters,
starting with an alphabetic, and "type" is an optional string of 
alphanumeric characters. Default is GENTHIS.
.VARIABLE FORMAT
This parameter specifies the data format. If it is
omitted, BYTE (unsigned INTEGER*1) data is assumed.
Valid values are:
HALF: specifies INTEGER*2 output. 
FULL: specifies INTEGER*4 output. 
REAL or REAL4: specifies REAL*4 output.
REAL8: specifies REAL*8 output. 
.VARI DN
 The DNs are listed line after line (sequentially) with NS values for
 each line.  
.END
$ Return
$!#############################################################################
$Imake_File:
$ create genthis.imake
#define  PROGRAM   genthis

#define MODULE_LIST genthis.f

#define MAIN_LANG_FORTRAN
#define R3LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_P3SUB
$ Return
$!#############################################################################
