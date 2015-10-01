$!****************************************************************************
$!
$! Build proc for MIPL module skew
$! VPACK Version 1.8, Tuesday, October 14, 1997, 17:34:53
$!
$! Execute by entering:		$ @skew
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
$ write sys$output "*** module skew ***"
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
$ write sys$output "Invalid argument given to skew.com file -- ", primary
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
$   if F$SEARCH("skew.imake") .nes. ""
$   then
$      vimake skew
$      purge skew.bld
$   else
$      if F$SEARCH("skew.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake skew
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @skew.bld "STD"
$   else
$      @skew.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create skew.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack skew.com -
	-s skew.f -
	-i skew.imake -
	-p skew.pdf -
	-t tstskew.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create skew.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	REAL*4 V(2,900),P(6)

	CALL XVUNIT(IU,'INP',1,IST,' ')
	CALL XVOPEN(IU,IST,' ')
	CALL XVREAD(IU,V,IST,' ')
	CALL XVCLOSE(IU,IST,' ')
	CALL XVPARM('NROW',NR,ICNT,IDEF,0)
	CALL XVPARM('NCOL',NC,ICNT,IDEF,0)
C	CL = V(1,NR*NC/2+1)
C	CS = V(2,NR*NC/2+1)

	CALL XVPARM('OFFSET',P,ICNT,IDEF,0)
	IF (ICNT .EQ. 2) THEN
		A = 1
		B = 0
		C = 0
		D = 1
		E = P(1)
		F = P(2)
	END IF
	CALL XVPARM('ROTOFF',P,ICNT,IDEF,0)
	IF (ICNT .EQ. 3) THEN
		PR = 3.1415926*P(3)/180.
		ST = SIN(PR)
		CT = COS(PR)
		A = CT
		B = -ST
		C = ST
		D = CT
		E = P(1)
		F = P(2)
	END IF
	CALL XVPARM('SCALEOFF',P,ICNT,IDEF,0)
	IF (ICNT .EQ. 3) THEN
		A = P(3)
		B = 0
		C = 0
		D = P(3)
		E = P(1)
		F = P(2)
	END IF
	CALL XVPARM('SRO',P,ICNT,IDEF,0)
	IF (ICNT .EQ. 4) THEN
		PR = 3.1415926*P(3)/180.
		ST = SIN(PR)
		CT = COS(PR)
		A = P(4)*CT
		B = -P(4)*ST
		C = P(4)*ST
		D = P(4)*CT
		E = P(1)
		F = P(2)
	END IF
	CALL XVPARM('UNC',P,ICNT,IDEF,0)
	IF (ICNT .EQ. 6) THEN
		A = P(1)
		B = P(2)
		C = P(3)
		D = P(4)
		E = P(5)
		F = P(6)
	END IF

	PRINT 1, A,B,E
	PRINT 1, C,D,F
1	FORMAT(3(5X,F12.4))

	DO 10 I=1,900
	VL = V(1,I)
	VS = V(2,I)
	V(1,I) = A*VL + B*VS + E
10	V(2,I) = C*VL + D*VS + F

	CALL XVUNIT(IO,'OUT',1,IST,' ')
	CALL XVOPEN(IO,IST,'OP','WRITE',' ')
	CALL XVWRIT(IO,V,IST,' ')
	CALL XVCLOSE(IO,IST,' ')

C	CALL PGRID(V,NR,NC,DUM,0)
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create skew.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM skew

   To Create the build file give the command:

		$ vimake skew			(VMS)
   or
		% vimake skew			(Unix)


************************************************************************/


#define PROGRAM	skew
#define R2LIB

#define MODULE_LIST skew.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create skew.pdf
PROCESS HELP=*
PARM INP STRING
PARM OUT STRING
PARM OFFSET REAL COUNT=(0,2) DEFAULT=--
PARM ROTOFF REAL COUNT=(0,3) DEFAULT=--
PARM SCALEOFF REAL COUNT=(0,3) DEFAULT=--
PARM SRO REAL COUNT=(0,4) DEFAULT=--
PARM UNC REAL COUNT=(0,6) DEFAULT=--
PARM NROW INTEGER COUNT=(0,1) DEFAULT=--
PARM NCOL INTEGER COUNT=(0,1) DEFAULT=--
END-PROC
.TITLE
SKEW
.HELP

Skew is a VICAR applications program that is called in the test
procedure for locus2, tstlocus2.pdf.  It is used to transform a
set of coordinates based on user defined parameters.

EXECUTION

	skew input_file output_file parms

For more information, see locus2.pdf.

REVISIONS:
    6-97  ...rrd...   Made portable for UNIX.
.LEVEL1
.VARIABLE OFFSET
LINE,SAMP
.VARIABLE ROTOFF
LINE,SAMP,ANGLE(DEG)
.VARIABLE SCALEOFF
LINE,SAMP,SCALE
.VARIABLE SRO
LINE,SAMP,ANGLE(DEG),SCALE
.VARIABLE UNC
FOR UNCONSTRAINED FIT
.VARIABLE NROW
NUMBER HORIZ GRID RULINGS
.VARIABLE NCOL
NUMBER VERT GRID RULINGS
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstskew.pdf
PROCEDURE
REFGBL $ECHO
REFGBL $AUTOUSAGE
BODY
LET $ECHO="YES"
LET $AUTOUSAGE="NONE"

gridgen G NROW=20 NCOL=20 GSL=40 GSS=40
skew G GO OFFSET=(25,10) NROW=20 NCOL=20
list GO

END-PROC
$ Return
$!#############################################################################
