$!****************************************************************************
$!
$! Build proc for MIPL module file_delete
$! VPACK Version 1.8, Tuesday, January 13, 1998, 16:55:26
$!
$! Execute by entering:		$ @file_delete
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
$ write sys$output "*** module file_delete ***"
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
$ write sys$output "Invalid argument given to file_delete.com file -- ", primary
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
$   if F$SEARCH("file_delete.imake") .nes. ""
$   then
$      vimake file_delete
$      purge file_delete.bld
$   else
$      if F$SEARCH("file_delete.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake file_delete
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @file_delete.bld "STD"
$   else
$      @file_delete.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create file_delete.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack file_delete.com -
	-s file_delete.f -
	-i file_delete.imake -
	-p file_delete.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create file_delete.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C
C  98-01-13   ...rea... Original Version
C
      SUBROUTINE MAIN44
C
      CHARACTER*80 FILENAME(30)
      CHARACTER*140 MSG
C
      CALL XVPARM('INP',FILENAME,NINP,IDEF,30)
      DO I=1,NINP
          OPEN (UNIT=66,FILE=FILENAME(I),IOSTAT=ISTAT,STATUS='OLD')
          IF (ISTAT .EQ. 0) THEN
              CLOSE (UNIT=66, STATUS='DELETE')
          ELSE
              WRITE (MSG,100) FILENAME(I)
  100         FORMAT('*** Unable to open/delete file: ',A80)
              CALL XVMESSAGE(MSG,' ')
          END IF
      END DO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create file_delete.imake
#define  PROGRAM   file_delete

#define MODULE_LIST file_delete.f

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
$ create file_delete.pdf
process help=*
PARM INP TYPE=STRING COUNT=1:30
END-PROC
.TITLE
VICAR Program FILE_DELETE
.HELP
PURPOSE:
FILE_DELETE deletes all files supplied as inputs.

EXECUTION:

Example
        FILE_DELETE (file1,file2,file3)

Output, size and parameter fields are not used.

OPERATION:
FILE_DELETE opens each input file, then closes it with the close status
of DELETE.

WRITTEN BY:  Ron Alley, 13 January 1998
COGNIZANT PROGRAMMER:  Ron Alley

.LEVEL1
.VARIABLE INP
Input files
.LEVEL2
.VARIABLE INP
INP specifies the input data sets to be deleted.  Up to thirty are allowed.
.END
$ Return
$!#############################################################################
