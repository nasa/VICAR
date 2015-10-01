$!****************************************************************************
$!
$! Build proc for MIPL module librtlf
$! VPACK Version 1.8, Tuesday, March 19, 1996, 18:15:18
$!
$! Execute by entering:		$ @librtlf
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module librtlf ***"
$!
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
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to librtlf.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("librtlf.imake") .nes. ""
$   then
$      vimake librtlf
$      purge librtlf.bld
$   else
$      if F$SEARCH("librtlf.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake librtlf
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @librtlf.bld "STD"
$   else
$      @librtlf.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create librtlf.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack librtlf.com -
	-i librtlf.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create librtlf.imake
/* Vimake file for creating a shared library out of the Fortran parts	*/
/* of the VICAR run-time library.  This contains those functions that	*/
/* require the Fortran run-time library (because they're either written	*/
/* in Fortran or call Fortran routines).  Splitting these out into a	*/
/* separate shared library allows C applications to link to the main	*/
/* shared library without needing the Fortran library.  The main	*/
/* shared library (rtl) is required to successfully link with this	*/
/* library.								*/
/*									*/
/* This library is not needed under VMS; the Fortran split does not	*/
/* cause a problem.  All of the code is included in the main shareable	*/
/* image.  The VMS-specific parts of this imake file are intended as	*/
/* examples for future shareable images.				*/
/*									*/
/* This is disabled for some platforms.  See util/imake.config for	*/
/* explanation (search for RTL_HAS_SO).  If you change the support list	*/
/* here, change it there, and in librtl.com, as well.			*/

#if SUN4_ARCH || SUN_SOLARIS_ARCH || X86_SOLARIS_ARCH || SGI_ARCH || ALPHA_ARCH || AXP_UNIX_ARCH || ANY_LINUX_ARCH

#define PROGRAM librtlf

#define SHARED_LIBRARY
#define SHARED_MAJOR_VERSION 1
#define SHARED_MINOR_VERSION 0

#define USES_EXTRACT
#define NO_CLEAN_SRC_LIST	/* suppress clean-src since they're all .o's */

/* VMS has this strange and brain-dead habit of renaming files in a	*/
/* library.  For Fortran object files, the name of the module in the	*/
/* library is *not* the name of the object file itself, but rather the	*/
/* name of the first function in the file.  Totally bizarro.  Thus the	*/
/* TAE_FORTRAN and XT defines below.  The FORSTRING define simply	*/
/* compensates for the difference in naming conventions: Unix.np.c vs.	*/
/* VMS.cnp.								*/

#if VMS_OS
#define TAE_FORTRAN EXTRACT(xzsinit.o,shvic)
#define XT EXTRACT_TAE(xtinit.o,tae)
#define FORSTRING EXTRACT_TAE(forstring.o,tae)
#else
#define TAE_FORTRAN EXTRACT(tae_fortran.o,shvic)
#define XT EXTRACT_TAE(xt.o,tae)
#define FORSTRING EXTRACT_TAE(forstring.np.o,tae)
#endif

#define MODULE_LIST EXTRACT(xvzinit.o,rtl) TAE_FORTRAN \
EXTRACT_TAE(xqgenbr.o,tae) EXTRACT_TAE(xqtaskbr.o,tae) \
EXTRACT_TAE(xzinit.o,tae) EXTRACT_TAE(stdfunc.o,tae) \

#define MODULE_LIST2 EXTRACT_TAE(wrtstd.o,tae) EXTRACT_TAE(xqgen.o,tae) \
EXTRACT_TAE(xqtask.o,tae) EXTRACT_TAE(xrgenbr.o,tae) \
EXTRACT_TAE(xrtaskbr.o,tae) XT \

#define MODULE_LIST3 EXTRACT_TAE(xubr.o,tae) EXTRACT_TAE(xzbr.o,tae) \
EXTRACT_TAE(xzcall.o,tae) EXTRACT_TAE(xzexit.o,tae) \
FORSTRING EXTRACT_TAE(xmbr.o,tae) \

#define MODULE_LIST4 EXTRACT_TAE(xrgen.o,tae) EXTRACT_TAE(xrtask.o,tae) \
EXTRACT_TAE(xu.o,tae) EXTRACT_TAE(oexits.o,tae) \
EXTRACT_TAE(xm.o,tae)

#define SYMBOL_LIST FSYMBOL(xvzinit) FSYMBOL(xzsinit) \
FSYMBOL(xqstr) FSYMBOL(xqsetm) FSYMBOL(xqdble) FSYMBOL(xqreal) \
FSYMBOL(xqintg) FSYMBOL(xqini) FSYMBOL(xqfile) \
FSYMBOL(xqout) FSYMBOL(xqdynp) FSYMBOL(xzinit) \
FSYMBOL(xzexit)

#else			/* Do nothing for platforms with no .so */
#define PROCEDURE librtlf
#endif

$ Return
$!#############################################################################
