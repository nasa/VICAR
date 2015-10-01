$!****************************************************************************
$!
$! Build proc for MIPL module axispts2
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:43
$!
$! Execute by entering:		$ @axispts2
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
$ write sys$output "*** module axispts2 ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("axispts2.imake") .nes. ""
$   then
$      vimake axispts2
$      purge axispts2.bld
$   else
$      if F$SEARCH("axispts2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake axispts2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @axispts2.bld "STD"
$   else
$      @axispts2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create axispts2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack axispts2.com -
	-s axispts2.f -
	-i axispts2.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create axispts2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	SUBROUTINE AXISPTS2(XMIN,XMAX,NTIC)
C
	INTEGER NVAL(100)/ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,
     +			  11,12,13, 7, 5, 8,17, 9, 1,10,
     +			   7,11, 1, 8, 5,13, 9, 7, 1,10,
     +			   1, 8,11, 2, 7, 6, 1, 2,13,10,
     +			   1, 7, 1,11, 9, 2, 1, 8, 7,10,
     +			   3,13, 1, 9,11, 7, 3, 2, 1, 6,
     +			   1, 2, 9, 8,13,11, 1, 4,13, 7,
     +			   1, 9, 1, 2, 5, 4,11,13, 1, 8,
     +			   9, 2, 1, 7, 5, 2, 3,11, 1, 9,
     +			   7, 4, 3, 2, 5, 8, 1, 7,11,10/
C
C								scale axis
	DELTA = XMAX - XMIN
	X = ALOG10(DELTA)
	IF (X.GE.0.0) THEN				! span between tic-marks
	    DIV = 10.0**(INT(X)-1)
	ELSE
	    DIV = 10.0**(INT(X)-2)
	END IF
	N = DELTA/DIV + 0.5
	NTIC = NVAL(N)
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create axispts2.imake
#define SUBROUTINE axispts2

#define MODULE_LIST axispts2.f

#define P3_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
