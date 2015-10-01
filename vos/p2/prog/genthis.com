$!****************************************************************************
$!
$! Build proc for MIPL module genthis
$! VPACK Version 1.5, Wednesday, April 14, 1993, 15:44:33
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
$ write sys$output "*** module genthis ***"
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
	-i genthis.imake -
	-t tstgenthis.pdf
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
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C       14 April 1993   NDR: Ported to UNIX
C
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

C           GENTHIS A NL NS PARAMS

C     GENTHIS DOES NOT DISTINGUISH BETWEEN INTEGER AND REAL 
C     NUMBER DN VALUES. ALL VALUES ARE NOW PROCESSED AS REAL NUMBERS. 

      CALL XVMESSAGE(' GENTHIS VERSION 2',' ')
      CALL GENTHISLAB
      CALL GENTHIS1

      RETURN
      END


      SUBROUTINE GENTHISLAB

      IMPLICIT NONE

      INTEGER DEF,CNT,STATUS
      INTEGER*4  MAXC
      CHARACTER*5  DATA

      REAL*4 BUF(1000)
      INTEGER*4 NL,NS,OUTUNIT
      COMMON /UNIT/ OUTUNIT
      COMMON /C1/ NL,NS,BUF
C

C          Label processor
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      if (status .ne. 1) then
	 call xvmessage(' *** XVUNIT:Invalid unit number ***',' ')
	 call abend
      end if

C--- Determine data type
      call xvp('FORMAT',data,cnt)
      IF (data(1:5) .eq. 'REAL4') THEN
	data = 'REAL'
      ELSE IF (data(1:5) .eq. 'REAL8') THEN
        data = 'DOUB'
      ENDIF

C--- Open output file with specified values
      call xvparm('NL',nl,cnt,def,maxc)
      call xvparm('NS',ns,cnt,def,maxc)
      CALL XVOPEN(OUTUNIT,STATUS,'U_FORMAT','REAL','O_FORMAT',
     +            DATA,'OP','WRITE','U_NL',nl,'U_NS',ns,
     +		  'IO_ACT','SA','OPEN_ACT','SA',' ')

C--- Get DN VALUES.
      CALL XVP('DN', BUF, CNT)
      IF (CNT .NE. NL*NS) THEN
         CALL XVMESSAGE(
     +    'PARAMETER ERR...DN COUNT DOES NOT MATCH SIZE',' ')
         CALL XVMESSAGE('**GENTHIS TASK CANCELLED',' ')
         CALL ABEND
      END IF

      RETURN
      END
C
C
      SUBROUTINE GENTHIS1

      IMPLICIT NONE

      INTEGER STATUS, I,N
 
      REAL*4 BUF(1000)
      INTEGER*4 NL,NS,OUTUNIT
      COMMON /UNIT/ OUTUNIT
      COMMON /C1/ NL,NS,BUF

        N = 1
	DO I = 1, NL
	  CALL XVWRIT(OUTUNIT,BUF(N),STATUS,' ')
          N = N + NS
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
PARM FORMAT TYPE=KEYWORD VALID=(BYTE,HALF,FULL,REAL4,REAL8,DOUB) +
 DEFAULT=BYTE
PARM DN     TYPE=REAL     COUNT=(1:600)

!# annot function="Generating Synthetic Images"
!# annot keywords=(DN,pixel,picture,variable)
END-PROC
.TITLE
Creates image from input DN list
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

TIMING:
	 None available for the VAX

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
/***********************************************************************

                     IMAKE FILE FOR PROGRAM genthis

   To Create the build file give the command:

		$ vimake genthis			(VMS)
   or
		% vimake genthis			(Unix)


************************************************************************/


#define PROGRAM	genthis
#define R2LIB

#define MODULE_LIST genthis.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
$ Return
$!#############################################################################
$Test_File:
$ create tstgenthis.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!  THIS IS A TEST FILE FOR genthis
!
genthis GENA NL=2 NS=3 DN= ( 5,6,8  15,16,18)
list GENA
!
genthis GENA NL=2 NS=3 DN= ( 5,6,8  15,16,18) 'BYTE
list GENA
!
genthis GENA NL=2 NS=3 DN= ( 5,6,8  15,16,18) 'HALF
list GENA
!
genthis GENA NL=2 NS=3 DN= ( 5,6,8  15,16,18) 'REAL4
list GENA
!
genthis GENA NL=2 NS=3 DN= ( 5,6,8  15,16,18) 'FULL
list GENA
!
genthis GENA NL=2 NS=3 DN= ( 5,6,8  15,16,18) 'REAL8
list GENA
!
genthis GENA NL=2 NS=3 DN= ( 5,6,8  15,16,18) 'DOUB
list GENA
end-proc
$ Return
$!#############################################################################
