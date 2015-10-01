$!****************************************************************************
$!
$! Build proc for MIPL module makeck
$! VPACK Version 1.9, Wednesday, May 31, 2000, 12:55:58
$!
$! Execute by entering:		$ @makeck
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
$ write sys$output "*** module makeck ***"
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
$ write sys$output "Invalid argument given to makeck.com file -- ", primary
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
$   if F$SEARCH("makeck.imake") .nes. ""
$   then
$      vimake makeck
$      purge makeck.bld
$   else
$      if F$SEARCH("makeck.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake makeck
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @makeck.bld "STD"
$   else
$      @makeck.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create makeck.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack makeck.com -
	-s makeck.f -
	-p makeck.pdf -
	-i makeck.imake -
	-t tstmakeck.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create makeck.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C Program to create a new C-kernel.
      SUBROUTINE MAIN44
C
      CHARACTER*(132) FILE
      INTEGER  HANDLE,ICNT,IDEF

      CALL XVPARM('OUT',file,icnt,idef,1)
C           Open with the file identifier equal to the file name
C           (it doesn't have to be this).
C
      CALL CKOPN(FILE,FILE,0,HANDLE)
C
C  We call DAFCLS rather than CKCLS because CKCLS throws an error
C  if the file contains no segments.
C
      CALL DAFCLS(HANDLE)
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create makeck.pdf
process help=*
PARM OUT 	 STRING
END-PROC

.TITLE
  MAKECK - Program to create a new C-kernel

.HELP
PURPOSE:  MAKECK creates a new SPICE C-kernel as follows:

	MAKECK  OUT=filename

where "filename" is the filename of the output C-kernel.

PROGRAM HISTORY:

Original programmer: Gary Yagi
Cognizant programmer: Gary Yagi
Revisions:  New

.level1

.vari OUT
String
Output C-kernel

.level2

.vari OUT
String
Name of output SPICE C-kernel.
.end
$ Return
$!#############################################################################
$Imake_File:
$ create makeck.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM makeck

   To Create the build file give the command:

		$ vimake makeck			(VMS)
   or
		% vimake makeck			(Unix)


************************************************************************/
#define PROGRAM	makeck

#define MODULE_LIST makeck.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB
#define LIB_SPICE
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tstmakeck.pdf
procedure
refgbl $echo
refgbl $syschar
body
let _onfail="continue"
let $echo="yes"
!Test of program MAKECK
makeck  out=dummy_ck
end-proc
$ Return
$!#############################################################################
