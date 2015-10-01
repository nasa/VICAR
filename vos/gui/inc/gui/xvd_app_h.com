$!****************************************************************************
$!
$! Build proc for MIPL module xvd_app_h
$! VPACK Version 1.9, Tuesday, August 25, 1998, 17:17:47
$!
$! Execute by entering:		$ @xvd_app_h
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
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
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
$ write sys$output "*** module xvd_app_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to xvd_app_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("xvd_app_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @xvd_app_h.bld "STD"
$   else
$      @xvd_app_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xvd_app_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xvd_app_h.com -mixed -
	-s XvdApplication.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create XvdApplication.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// XvdApplication.h: 
////////////////////////////////////////////////////////////
#ifndef XVDAPPLICATION_H
#define XVDAPPLICATION_H

#ifdef ENABLE_SAGE
#include "SpbApplication.h"
class XvdApplication : public SpbApplication {
#else
#include "Application.h"
class XvdApplication : public Application {
#endif

  private:

#ifdef ENABLE_SAGE
    static XrmOptionDescRec _options[12]; 
#else
    static XrmOptionDescRec _options[9];
#endif

  protected:

    // Allow subclasses to use command-line arguments for resources
    // If the array is dynamically allocated, the destructor can free it

    virtual XrmOptionDescList getOptions() { return _options; }
    virtual Cardinal getNumOptions() { return XtNumber(_options); }

  public:
    
    XvdApplication ( const char *appClassName );
    virtual ~XvdApplication();     

    virtual const char *const className() { return "XvdApplication"; }
};
#endif

// Create pointer to single global instance of XvdApplication class

extern XvdApplication *theXvdApplication;
$ VOKAGLEVE
$ Return
$!#############################################################################
