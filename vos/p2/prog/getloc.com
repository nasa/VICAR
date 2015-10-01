$!****************************************************************************
$!
$! Build proc for MIPL module getloc
$! VPACK Version 1.7, Friday, September 02, 1994, 09:36:13
$!
$! Execute by entering:		$ @getloc
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
$ write sys$output "*** module getloc ***"
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
$ write sys$output "Invalid argument given to getloc.com file -- ", primary
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
$   if F$SEARCH("getloc.imake") .nes. ""
$   then
$      vimake getloc
$      purge getloc.bld
$   else
$      if F$SEARCH("getloc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake getloc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @getloc.bld "STD"
$   else
$      @getloc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create getloc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack getloc.com -
	-s getloc.f -
	-i getloc.imake -
	-p getloc.pdf -
	-t tstgetloc.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create getloc.f
$ DECK/DOLLARS="$ VOKAGLEVE"
       PROGRAM  GETLOC
C#######################################################################
C  NAME OF ROUTINE
C      GETLOC ( GET LOCation)
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      Program GETLOC is a VICAR applications program which is used to 
C      extract a subarea from a tiepoint file containing theodolite 
C      measurements of grid intersections.  
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    SEPTEMBER 1984
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C
C  ORIGINAL GETLOC PROGRAM BY
C      GARY YAGI
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C    10-94  AS   (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C     4-86  SP   CONVERTED TO USE VICAR2 CALLS.
C     9-84  SP   CONVERTED FROM IBM VICAR VERSION
C     9-84  SP   CHANGED MVE CALL TO USE DCODE OF 7.  DCODE OF 8 DOES  NOT
C                WORK WHEN THE LINE COORDINATE IS 0.0
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
C      MAIN44 is used to extract a subarea from a tiepoint 
C      file containing theodolite measurements of grid intersections.  
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     SEPTEMBER 1984
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR EXECUTIVE       FORTRAN-77
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  CALLED BY
C      GETLOC
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IMPLICIT INTEGER(A-Z)
      REAL*8 BUF(27,27),OBUF(729)
C
C======================START OF EXECUTABLE CODE======================

C..OPEN INPUT FILE

      CALL IFMESSAGE('GETLOC version 31-OCT-94')
      CALL XVEACTION('SA',' ')
      CALL XVUNIT(IUNIT,'INP',1,ISTAT,' ')
      IRETURN = 1
      CALL XVSIGNAL(IUNIT, ISTAT, IRETURN)
      CALL XVOPEN(IUNIT,ISTAT, 'OP', 'READ',' ')

C..READ THE SR,SC,NR,NC PARAMETERS.

      CALL XVPARM('SR',SR,ICOUNT,IDEF,1)
      CALL XVPARM('SC',SC,ICOUNT,IDEF,1)
      CALL XVPARM('NR',NR,ICOUNT,IDEF,1)
      CALL XVPARM('NC',NC,ICOUNT,IDEF,1)

      NLOC = NR*NC
      NLO = 1
      NSO = 2*NLOC

C..OPEN THE OUTPUT FILE.

      CALL XVUNIT(OUNIT,'OUT',1,ISTAT,' ')
      IRETURN = 1
      CALL XVSIGNAL(OUNIT, ISTAT, IRETURN)
      CALL XVOPEN(OUNIT, ISTAT, 'OP','WRITE', 'U_NL',NLO,
     .        'U_NS',NSO, 'U_FORMAT','REAL', 'O_FORMAT','REAL',' ')

C..EXTRACT THE SUBAREA OF THE INPUT FILE.

      CALL XVREAD( IUNIT, BUF, ISTAT,' ')

      K = 1
C
      DO I=1,NR
        CALL MVE(7,2*NC,BUF(SC,SR+I-1),OBUF(K),1,1)
        K = K + NC
      END DO
C
      CALL XVWRIT( OUNIT, OBUF, ISTAT,' ' )

      CALL XVCLOSE(IUNIT,ISTAT,' ')
      CALL XVCLOSE(OUNIT,ISTAT,' ')

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create getloc.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM getloc

   To Create the build file give the command:

		$ vimake getloc			(VMS)
   or
		% vimake getloc			(Unix)


************************************************************************/


#define PROGRAM	getloc
#define R2LIB

#define MODULE_LIST getloc.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create getloc.pdf
process help=*
!  PDF FILE FOR GETLOC
!
PARM INP     TYPE=STRING   COUNT=1
PARM OUT     TYPE=STRING   COUNT=1
!
PARM SIZE    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SS      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM NL      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=1       DEFAULT=0
!
PARM SR      TYPE=INTEGER  COUNT=1                  VALID=(1:27)
PARM SC      TYPE=INTEGER  COUNT=1                  VALID=(1:27)
PARM NR      TYPE=INTEGER  COUNT=1                  VALID=(1:27)
PARM NC      TYPE=INTEGER  COUNT=1                  VALID=(1:27)
!
END-PROC
.TITLE
VICAR Program "getloc"
.HELP
PURPOSE

"getloc" is a special purpose program used during geometric calibration
of vidicon and CCD cameras.  It is used to extract a subarea from a
tiepoint file containing theodolite measurements of grid intersections.  

EXECUTION

	getloc INP=GRID OUT=OGRID SR=r SC=c NR=n NC=m 
   where...
	GRID are the input theodolite measurements,
	OGRID are the output extracted subarea,
	and (r,c,n,m) specify the starting row, starting column, number
	of rows and number of columns of the area to be extracted.

   The input and output grid locations are in the same format as output
   by the programs "gridloca" and "gridlocb" and input to the program "locus".

   The input grid is a fixed 27x27 array of grid intersections.

 WRITTEN BY:             Gary Yagi                   19 Sep 1984

 COGNIZANT PROGRAMMER:   Gary Yagi

 REVISIONS:
   SEPT. 1984:  ...SP...  converted to Vax.
   APRIL 1986:  ...SP...  Converted to Vicar2
   OCT.  1994:  ...AS...  (CRI) Made portable for UNIX
.LEVEL1
.VARIABLE INP
Input theodolite grid measurements (27x27 array).
.VARIABLE OUT
Output grid subarea.
.VARIABLE SIZE
(Not used.)
.VARIABLE SL
(Not used.)
.VARIABLE SS
(Not used.)
.VARIABLE NL
(Not used.)
.VARIABLE NS
(Not used.)
.VARIABLE SR
Starting row of extracted area.
.VARIABLE SC
Starting column of extracted area.
.VARIABLE NR
Number of rows in extracted area.
.VARIABLE NC
Number of columns in extracted area.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgetloc.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!  THIS IS A TEST OF PROGRAM GETLOC
!
!      byte image - first test defaults.
!
gen GETLOCA NL=1 NS=1458 'REAL4
getloc INP=GETLOCA OUT=GETLOCAO SR=1 SC=1 NR=27 NC=27
list GETLOCAO 
!
!    try SR not equal to 1.
!
getloc INP=GETLOCA OUT=GETLOCAO2 SR=3 SC=5 NR=12 NC=4
list GETLOCAO2
END-PROC
$ Return
$!#############################################################################
