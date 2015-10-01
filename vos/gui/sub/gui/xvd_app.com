$!****************************************************************************
$!
$! Build proc for MIPL module xvd_app
$! VPACK Version 1.9, Tuesday, August 25, 1998, 17:17:43
$!
$! Execute by entering:		$ @xvd_app
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
$ write sys$output "*** module xvd_app ***"
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
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to xvd_app.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
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
$   if F$SEARCH("xvd_app.imake") .nes. ""
$   then
$      vimake xvd_app
$      purge xvd_app.bld
$   else
$      if F$SEARCH("xvd_app.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xvd_app
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xvd_app.bld "STD"
$   else
$      @xvd_app.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xvd_app.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xvd_app.com -mixed -
	-s XvdApplication.cc -
	-i xvd_app.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create XvdApplication.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// XvdApplication.cc: 
////////////////////////////////////////////////////////////////////////
#include "XvdApplication.h"

XvdApplication *theXvdApplication = NULL;

// The subclass should contain all of these options.
// If you are updating record, always make sure 
// your changes are reflected in all the subclasses

XrmOptionDescRec  XvdApplication::_options[] = {
#ifdef ENABLE_SAGE
 {"-host",       "*host",                    XrmoptionSepArg,  NULL  },
 {"-port",       "*port",                    XrmoptionSepArg,  NULL  },
 {"-module",     "*module",                  XrmoptionSepArg,  NULL  },
#endif
 {(char *)"-min",       (char *)"*min",              XrmoptionSepArg,  NULL  },
 {(char *)"-max",       (char *)"*max",              XrmoptionSepArg,  NULL  },
 {(char *)"-fullscreen",(char *)"*fullScreenDisplay",XrmoptionNoArg,(char*)"True"},
 {(char *)"-fit",       (char *)"*fit",           XrmoptionNoArg,(char*)"True"},
 {(char *)"-height",    (char *)"*XVd.height",       XrmoptionSepArg,  NULL  },
 {(char *)"-width",     (char *)"*XVd.width",        XrmoptionSepArg,  NULL  },
 {(char *)"-x",         (char *)"*XVd.x",            XrmoptionSepArg,  NULL  },
 {(char *)"-y",         (char *)"*XVd.y",            XrmoptionSepArg,  NULL  },
 {(char *)"-help",      (char *)"*cmdLineHelpReqd", XrmoptionNoArg,(char*)"True"}
};

XvdApplication::XvdApplication ( const char *appClassName ) : 
#ifdef ENABLE_SAGE
                                         SpbApplication ( appClassName )
#else
                                         Application ( appClassName )
#endif
{
    // Set the global Application pointer

    theXvdApplication = this;
}

XvdApplication::~XvdApplication()
{
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xvd_app.imake
#define SUBROUTINE xvd_app
#define MODULE_LIST XvdApplication.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#ifdef ENABLE_SAGE
#define LIB_SAGE_CLIENT
#define LIB_DALI
#define LIB_SAGE_BASE
#define CCC_TEMPLATES
#endif

#define LIB_MOTIFAPP
#define LIB_MOTIF
$ Return
$!#############################################################################
