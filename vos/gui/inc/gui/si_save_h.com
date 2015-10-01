$!****************************************************************************
$!
$! Build proc for MIPL module si_save_h
$! VPACK Version 1.9, Thursday, March 01, 2001, 10:24:56
$!
$! Execute by entering:		$ @si_save_h
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
$ write sys$output "*** module si_save_h ***"
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
$ write sys$output "Invalid argument given to si_save_h.com file -- ", primary
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
$   if F$SEARCH("si_save_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @si_save_h.bld "STD"
$   else
$      @si_save_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create si_save_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack si_save_h.com -mixed -
	-s SiSaveCmdInterface.h SiSaveDialog.h SiSaveAsCmd.h -
	   SiSaveComponentCmd.h SiSaveBrowserCmd.h SiSaveCmdValue.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SiSaveCmdInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiSaveCmdInterface.h: Command interface subclass that fills 
// a SiSaveCmdValue structure and executes the command.
////////////////////////////////////////////////////////////////

#ifndef SISAVECMDINTERFACE_H
#define SISAVECMDINTERFACE_H

#include "CmdInterface.h"
#include "SiSaveCmdValue.h"

class KeyinView;
class OptionCmdMenu;
class Cmd;
class CmdList;

class SiSaveCmdInterface : public CmdInterface {

  protected: 

    Boolean _suspend_updates;

    Cmd *_browser_red_cmd, *_post_red_cmd;
    CmdInterface *_browser_red;
    Cmd *_browser_grn_cmd, *_post_grn_cmd;
    CmdInterface *_browser_grn;
    Cmd *_browser_blu_cmd, *_post_blu_cmd;
    CmdInterface *_browser_blu;

    KeyinView *_filename_red;
    KeyinView *_filename_grn;
    KeyinView *_filename_blu;

    CmdList *_extentList;
    OptionCmdMenu *_extentMenu;
    Cmd *_extentDisplay, *_extentFile, *_extentROI;

    CmdList *_lutList;
    OptionCmdMenu *_lutMenu;
    Cmd *_lutRaw, *_lutStretch, *_lutPseudo, *_lutPseudoOnly;

    CmdList *_byteList;
    OptionCmdMenu *_byteMenu;
    Cmd *_byteFalse, *_byteTrue;

    CmdList *_fmtList;
    OptionCmdMenu *_fmtMenu;
    Cmd *_fmtVicar, *_fmtTiff;

    static void runCommandCallback(Widget, XtPointer, XtPointer);

  public:

    SiSaveCmdInterface(Widget, Cmd *);
    virtual ~SiSaveCmdInterface();

    virtual void runCommand();

    virtual void setFilename(char *name, int which);

    virtual void setValue(CmdValue);

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiSaveDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////
// SiSaveDialog.h: This class creates a work area for save dialog.
//////////////////////////////////////////////////////////////////////
#ifndef SISAVEDIALOG_H
#define SISAVEDIALOG_H
#include "CustomDialog.h"
#include "HelpBrowser.h"

class Cmd;
class SiSaveCmdInterface;

class SiSaveDialog : public CustomDialog {

  private:

    Cmd *_cmd;
    SiSaveCmdInterface *_saveCI;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    SiSaveDialog(const char *name, Cmd *cmd);

    virtual Widget createWorkArea(Widget parent);

    virtual void post();

    virtual const char *const className() { return "SiSaveDialog"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiSaveAsCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiSaveAsCmd.h:  Saves the image according to the SiSaveCmdValue
// CmdValue passed in.
//
//!!!! NOTE:  THIS IS A HACK CURRENTLY!  The command derives from
//!!!! SiRunStretchScriptCmd, adding the SiSaveCmdValue parameters to
//!!!! the file, then running an *external* script to implement the
//!!!! save.  It should be done internally, via save hooks in ImageData.
/////////////////////////////////////////////////////////////
#ifndef SISAVEASCMD_H
#define SISAVEASCMD_H
#include "SiRunStretchScriptCmd.h"

class SiSaveAsCmd : public SiRunStretchScriptCmd {

 protected:

// doit() is handled by the base class, it just calls the functions below

   virtual void printVersionString(FILE *tfp);
   virtual void printContents(FILE *tfp);
   // cleanup() not needed...

 public:

   SiSaveAsCmd(const char *name, int active, Widget xiw,
		ImageData *image, const char *script,
		Lut *sR, Lut *sG, Lut *sB, Lut *pR, Lut *pG, Lut *pB);

   virtual void freeValue(CmdValue);

   virtual const char *const className () { return "SiSaveAsCmd"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiSaveComponentCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiSaveComponentCmd.h:  Command class for any component within
// the SiSaveCmdInterface; all it does is tell the SCI to do its
// thing.  Used for simple creation of Option menus.
////////////////////////////////////////////////////////////////
#ifndef SISAVECOMPONENTCMD_H
#define SISAVECOMPONENTCMD_H
#include "RadioCmd.h"

class SiSaveCmdInterface;

class SiSaveComponentCmd : public RadioCmd {

  protected:

    SiSaveCmdInterface *_sci;

    virtual void doit();

  public:
    
    SiSaveComponentCmd(const char *, int, CmdList *, SiSaveCmdInterface *);

    virtual const char *const className () { return "SiSaveComponentCmd"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiSaveBrowserCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiSaveBrowserCmd:  Accepts a filename from a SingleFileSelection
// dialog (as CmdValue) and stuffs it in the appropriate slot in
// the SiSaveCmdInterface.
/////////////////////////////////////////////////////////////
#ifndef SISAVEBROWSERCMD_H
#define SISAVEBROWSERCMD_H
#include "NoUndoCmd.h"

class SiSaveCmdInterface;

class SiSaveBrowserCmd : public NoUndoCmd {

 protected:

   SiSaveCmdInterface *_sci;
   int _which;

   virtual void doit();

 public:

   SiSaveBrowserCmd(const char *name, int active, SiSaveCmdInterface *sci,
							int which);

   virtual void freeValue(CmdValue);

   virtual const char *const className () { return "SiSaveBrowserCmd"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiSaveCmdValue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiSaveCmdValue.h:  Contains the parameters needed for the Save
// (really SaveAs) command.  This object is passed to the Save
// command as a value.
////////////////////////////////////////////////////////////////
#ifndef SISAVECMDVALUE_H
#define SISAVECMDVALUE_H

#include "XvicBasicImage.h"

enum SiSaveImageExtent { SaveDisplayOnly, SaveEntireFile, SaveROI };

struct SiSaveCmdValue {

    char filename_red[256];
    char filename_grn[256];
    char filename_blu[256];
    SiSaveImageExtent imageExtent;
    unsigned char lutType;	// See Xiw lutType for values
    Boolean asByte;		// T: convert to byte, stretches allowed
				// F: preserve data type, no str unless byte
    char fileFormat[33];	// e.g. VICAR

    SiSaveCmdValue()		// Set defaults
	{  strcpy(filename_red, ""); strcpy(filename_grn, ".grn");
	   strcpy(filename_blu, ".blu"); imageExtent = SaveDisplayOnly;
	   lutType = XvicSTRETCH; asByte = True;
	   strcpy(fileFormat, "VICAR");		//!!!!
	}

};

#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
