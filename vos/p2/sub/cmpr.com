$!****************************************************************************
$!
$! Build proc for MIPL module cmpr
$! VPACK Version 1.5, Monday, December 14, 1992, 11:13:11
$!
$! Execute by entering:		$ @cmpr
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module cmpr ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("cmpr.imake") .nes. ""
$   then
$      vimake cmpr
$      purge cmpr.bld
$   else
$      if F$SEARCH("cmpr.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake cmpr
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @cmpr.bld "STD"
$   else
$      @cmpr.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cmpr.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cmpr.com -
	-s cmpr.f -
	-i cmpr.imake -
	-t tcmpr.f tcmpr.imake tcmpr.pdf tstcmpr.pdf -
	-o cmpr.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cmpr.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      FUNCTION CMPR(X,Y,N)
C
C  83-3-9  ...  LWK
C
C CMPR compares 2 byte arrays, X and Y, of length N.
C It returns .TRUE. if they are identical, .FALSE. otherwise.
C
      IMPLICIT INTEGER (A-Z)
      LOGICAL CMPR
      BYTE X(*),Y(*)
C
      CMPR = .TRUE.
      DO I=1,N
	IF (X(I).NE.Y(I)) THEN
	  CMPR = .FALSE.
	  RETURN
	ENDIF
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create cmpr.imake
/* Imake file for VICAR subroutine cmpr */

#define SUBROUTINE cmpr

#define MODULE_LIST cmpr.f

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tcmpr.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C-----THIS IS A ROUTINE TO TEST  FUNCTION  CMPR
	LOGICAL CMPR,T
        character*3 not
	character A*28
	INTEGER*4 S1/'ABCD'/,S2/'ABCE'/
	CALL XVMESSAGE('COMPARING ABCD WITH ABCE',' ')
C
	DO 10 I=1,4
        not = ' '
	T=CMPR(S1,S2,I)
	IF(.NOT.T)         not = 'NOT'
        write (A,9000) I,not
10	CALL XVMESSAGE(A,' ')
C
	return
9000    format( 'FIRST ', I1,' CHARACTERS ', A3,' SAME')
	END
$!-----------------------------------------------------------------------------
$ create tcmpr.imake
/* Imake file for Test of VICAR subroutine cmpr */

#define PROGRAM tcmpr

#define MODULE_LIST tcmpr.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB


$!-----------------------------------------------------------------------------
$ create tcmpr.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstcmpr.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!THIS IS A TEST OF MODULE CMPR   (FUNCTION)
tcmpr
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create cmpr.hlp
1 CMPR
  For the port to UNIX, calls to CMPR will mostly, if not always, be replaced
  with (expression1 .EQ. expression2) where the expressions may be character
  variables, character constants, character substrings, or other character
  expressions.  It is being ported in case any programs really need to
  compare BYTE arrays.

  The function subprogram CMPR compares two byte arrays,
  X and Y, of length N.  It returns .TRUE. if they are
  identical, and .FALSE. if they are not equal.

 From C, use ANSI C routine memcmp, or use strcmp or strncmp for strings.


  Usage:  L = CMPR(X,Y,N)

  X and Y are the starting addresses of the two BYTE
  arrays to be compared.  N is the number of 
  bytes to be compared.  N must be an INTEGER*4 variable.

2 History

  Original Programmer: Joel A. Mosher   3/31/83
  Source Language: Fortran
  Ported for UNIX by:  S. Pohorsky   12/12/92
  Current Cog Progr:   S. Pohorsky

$ Return
$!#############################################################################
