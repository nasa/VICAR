$!****************************************************************************
$!
$! Build proc for MIPL module si_scriptcmd_h
$! VPACK Version 1.9, Friday, March 26, 1999, 11:22:51
$!
$! Execute by entering:		$ @si_scriptcmd_h
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
$ write sys$output "*** module si_scriptcmd_h ***"
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
$ write sys$output "Invalid argument given to si_scriptcmd_h.com file -- ", primary
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
$   if F$SEARCH("si_scriptcmd_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @si_scriptcmd_h.bld "STD"
$   else
$      @si_scriptcmd_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create si_scriptcmd_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack si_scriptcmd_h.com -mixed -
	-s SiRunScriptCmd.h SiRunStretchScriptCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SiRunScriptCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiRunScriptCmd.h:  Runs a script (usually determined by resources)
// that is passed information about the state of the image display.
/////////////////////////////////////////////////////////////
#ifndef SIRUNSCRIPTCMD_H
#define SIRUNSCRIPTCMD_H
#include "NoUndoCmd.h"
#include <Xm/Xm.h>
#include <stdio.h>

class ImageData;

class SiRunScriptCmd : public NoUndoCmd {

 protected:

   Widget _xiw;
   ImageData *_imageData;

   char *_scriptToRun;

   virtual void doit();   

   virtual void printVersionString(FILE *tfp);
   virtual void printContents(FILE *tfp);
   virtual void cleanup() { }			// nothing to do here

 public:

   SiRunScriptCmd(const char *, int, Widget xiw, ImageData *,
						const char *script);

   virtual const char *const className () { return "SiRunScriptCmd"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiRunStretchScriptCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiRunStretchScriptCmd.h:  Runs a script (usually determined
// by resources) that is passed information about the state of
// the image display.  This subclass adds information about the
// stretch and pseudocolor tables to the basic SiRunScriptCmd.
/////////////////////////////////////////////////////////////
#ifndef SIRUNSTRETCHSCRIPTCMD_H
#define SIRUNSTRETCHSCRIPTCMD_H
#include "SiRunScriptCmd.h"
#include <Xm/Xm.h>

class Lut;

class SiRunStretchScriptCmd : public SiRunScriptCmd {

 protected:

   Lut *_lutRed, *_lutGrn, *_lutBlu;
   Lut *_pseudoRed, *_pseudoGrn, *_pseudoBlu;

   char _stretch_file[256], _pseudo_file[256];

// doit() is handled by the base class, it just calls the functions below

   virtual void printVersionString(FILE *tfp);
   virtual void printContents(FILE *tfp);
   virtual void cleanup();

 public:

   SiRunStretchScriptCmd(const char *, int, Widget xiw, ImageData *,
		const char *script,
		Lut *sR, Lut *sG, Lut *sB, Lut *pR, Lut *pG, Lut *pB);

   virtual const char *const className () { return "SiRunStretchScriptCmd"; }
};
#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
