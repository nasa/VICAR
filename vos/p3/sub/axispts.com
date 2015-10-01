$!****************************************************************************
$!
$! Build proc for MIPL module axispts
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:43
$!
$! Execute by entering:		$ @axispts
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
$ write sys$output "*** module axispts ***"
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
$   if F$SEARCH("axispts.imake") .nes. ""
$   then
$      vimake axispts
$      purge axispts.bld
$   else
$      if F$SEARCH("axispts.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake axispts
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @axispts.bld "STD"
$   else
$      @axispts.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create axispts.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack axispts.com -
	-s axispts.f -
	-i axispts.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create axispts.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	SUBROUTINE AXISPTS(XMIN,XMAX,BOT,TOP,NTIC)
C
C								scale axis
	X = ALOG10(XMAX-XMIN)
	IF (X.GE.0.0) THEN				! span between tic-marks
	    DIV = 10.0**INT(X)
	ELSE
	    DIV = 10.0**INT(X-1.0)
	END IF
	IF (XMIN.GE.0.0) THEN				! lower limit of graph
	    BOT = DIV*INT(XMIN/DIV)
	ELSE
	    BOT = DIV*INT((XMIN/DIV)-1.0)
	END IF
	NTIC = 1+(XMAX-BOT)/DIV				! # of tic-marks
	TOP = BOT+NTIC*DIV				! upper limit of graph
C
	IF (NTIC.LE.3) THEN				! adjust # of tic-marks
	    NTIC = 5*NTIC
	    DIV = DIV/5.0
	END IF
	IF (NTIC.LE.7) THEN
	    NTIC = 2*NTIC
	    DIV = DIV/2.0
	END IF
	DO WHILE (XMIN .GE. BOT+0.999999*DIV)
	    BOT = BOT+DIV
	    NTIC = NTIC-1
	END DO
	DO WHILE (XMAX .LE. TOP-0.999999*DIV)
	    TOP = TOP-DIV
	    NTIC = NTIC-1
	END DO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create axispts.imake
#define SUBROUTINE axispts

#define MODULE_LIST axispts.f

#define P3_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
