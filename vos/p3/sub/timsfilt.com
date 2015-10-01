$!****************************************************************************
$!
$! Build proc for MIPL module timsfilt
$! VPACK Version 1.5, Monday, March 29, 1993, 16:31:07
$!
$! Execute by entering:		$ @timsfilt
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
$ write sys$output "*** module timsfilt ***"
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
$   if F$SEARCH("timsfilt.imake") .nes. ""
$   then
$      vimake timsfilt
$      purge timsfilt.bld
$   else
$      if F$SEARCH("timsfilt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake timsfilt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @timsfilt.bld "STD"
$   else
$      @timsfilt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create timsfilt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack timsfilt.com -
	-s timsfilt.f -
	-i timsfilt.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create timsfilt.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C******************************************************************************
	SUBROUTINE TIMSFILT(IDATE,WAVE,FFILTER,FWVLN)
C
C	Interface to return the filter and wavelength arrays explicitly
C       instead of in a common block (for some c programs such as)
C       tcal
C
	REAL WAVE(6),FILTER(158,6),WVLN(158),FFILTER(158,6),FWVLN(158)
	COMMON /RAWFIL/ FILTER,WVLN,ICALDATE
C
        CALL CENTWAV(IDATE,WAVE)
C
	CALL MVE(7,158,FILTER(1,1),FFILTER(1,1),1,1)
	CALL MVE(7,158,FILTER(1,2),FFILTER(1,2),1,1)
	CALL MVE(7,158,FILTER(1,3),FFILTER(1,3),1,1)
	CALL MVE(7,158,FILTER(1,4),FFILTER(1,4),1,1)
	CALL MVE(7,158,FILTER(1,5),FFILTER(1,5),1,1)
	CALL MVE(7,158,FILTER(1,6),FFILTER(1,6),1,1)
	CALL MVE(7,158,WVLN,FWVLN,1,1)
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create timsfilt.imake
#define SUBROUTINE timsfilt

#define MODULE_LIST timsfilt.f

#define P3_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
