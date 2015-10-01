$!****************************************************************************
$!
$! Build proc for MIPL module si_print_h
$! VPACK Version 1.9, Thursday, March 01, 2001, 10:24:56
$!
$! Execute by entering:		$ @si_print_h
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
$ write sys$output "*** module si_print_h ***"
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
$ write sys$output "Invalid argument given to si_print_h.com file -- ", primary
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
$   if F$SEARCH("si_print_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @si_print_h.bld "STD"
$   else
$      @si_print_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create si_print_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack si_print_h.com -mixed -
	-s SiPrintCmdInterface.h SiPrintDialog.h SiPrintCmd.h -
	   SiPrintComponentCmd.h SiPrintBrowserCmd.h SiPrintCmdValue.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SiPrintCmdInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiPrintCmdInterface.h: Command interface subclass that fills 
// a SiPrintCmdValue structure and executes the command.
////////////////////////////////////////////////////////////////

#ifndef SIPRINTCMDINTERFACE_H
#define SIPRINTCMDINTERFACE_H

#include "CmdInterface.h"
#include "SiPrintCmdValue.h"

class KeyinView;
class OptionCmdMenu;
class Cmd;
class CmdList;

class SiPrintCmdInterface : public CmdInterface {

  protected: 

    Boolean _suspend_updates;

    Cmd *_browser_file_cmd, *_post_file_cmd;
    CmdInterface *_browser_file;

    KeyinView *_filename;

    CmdList *_extentList;
    OptionCmdMenu *_extentMenu;
    Cmd *_extentDisplay, *_extentFile, *_extentROI;

    CmdList *_lutList;
    OptionCmdMenu *_lutMenu;
    Cmd *_lutRaw, *_lutStretch, *_lutPseudo, *_lutPseudoOnly;

    CmdList *_printToList;
    OptionCmdMenu *_printToMenu;
    Cmd *_printToPrinter, *_printToFile;

    KeyinView *_printerCommand;
    KeyinView *_printWidth;
    KeyinView *_printHeight;

    CmdList *_printOrientationList;
    OptionCmdMenu *_printOrientationMenu;
    Cmd *_printPortrait, *_printLandscape;

    CmdList *_printTitleList;
    OptionCmdMenu *_printTitleMenu;
    Cmd *_printTitleFilename, *_printTitleCustom;

    KeyinView *_title;

    static void runCommandCallback(Widget, XtPointer, XtPointer);

    void setPrintTo();		// sets sensitivity for file/cmd keyins

  public:

    SiPrintCmdInterface(Widget, Cmd *);
    virtual ~SiPrintCmdInterface();

    virtual void runCommand();

    virtual void setFilename(char *name);

    virtual void setValue(CmdValue);

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiPrintDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////
// SiPrintDialog.h: This class creates a work area for print dialog.
//////////////////////////////////////////////////////////////////////
#ifndef SIPRINTDIALOG_H
#define SIPRINTDIALOG_H
#include "CustomDialog.h"
#include "HelpBrowser.h"

class Cmd;
class SiPrintCmdInterface;

class SiPrintDialog : public CustomDialog {

  private:

    Cmd *_cmd;
    SiPrintCmdInterface *_printCI;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    SiPrintDialog(const char *name, Cmd *cmd);

    virtual Widget createWorkArea(Widget parent);

    virtual void post();

    virtual const char *const className() { return "SiPrintDialog"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiPrintCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiPrintCmd.h:  Prints the image according to the SiPrintCmdValue
// CmdValue passed in.
//
//!!!! NOTE:  THIS IS A HACK CURRENTLY!  The command derives from
//!!!! SiSaveAsCmd, adding the SiPrintCmdValue parameters to
//!!!! the file, then running an *external* script to implement the
//!!!! print.  It should be done internally, via save hooks in ImageData.
/////////////////////////////////////////////////////////////
#ifndef SIPRINTCMD_H
#define SIPRINTCMD_H
#include "SiSaveAsCmd.h"

class SiPrintCmd : public SiSaveAsCmd {

 protected:

// doit() is handled by the base class, it just calls the functions below

   virtual void printVersionString(FILE *tfp);
   virtual void printContents(FILE *tfp);
   // cleanup() not needed...

 public:

   SiPrintCmd(const char *name, int active, Widget xiw,
		ImageData *image, char *script,
		Lut *sR, Lut *sG, Lut *sB, Lut *pR, Lut *pG, Lut *pB);

   virtual void freeValue(CmdValue);

   virtual const char *const className () { return "SiPrintCmd"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiPrintComponentCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiPrintComponentCmd.h:  Command class for any component within
// the SiPrintCmdInterface; all it does is tell the PCI to do its
// thing.  Used for simple creation of Option menus.
////////////////////////////////////////////////////////////////
#ifndef SIPRINTCOMPONENTCMD_H
#define SIPRINTCOMPONENTCMD_H
#include "RadioCmd.h"

class SiPrintCmdInterface;

class SiPrintComponentCmd : public RadioCmd {

  protected:

    SiPrintCmdInterface *_pci;

    virtual void doit();

  public:
    
    SiPrintComponentCmd(const char *, int, CmdList *, SiPrintCmdInterface *);

    virtual const char *const className () { return "SiPrintComponentCmd"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiPrintBrowserCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiPrintBrowserCmd:  Accepts a filename from a SingleFileSelection
// dialog (as CmdValue) and stuffs it in the appropriate slot in
// the SiPrintCmdInterface.
/////////////////////////////////////////////////////////////
#ifndef SIPRINTBROWSERCMD_H
#define SIPRINTBROWSERCMD_H
#include "NoUndoCmd.h"

class SiPrintCmdInterface;

class SiPrintBrowserCmd : public NoUndoCmd {

 protected:

   SiPrintCmdInterface *_pci;

   virtual void doit();

 public:

   SiPrintBrowserCmd(const char *name, int active, SiPrintCmdInterface *pci);

   virtual void freeValue(CmdValue);

   virtual const char *const className () { return "SiPrintBrowserCmd"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiPrintCmdValue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiPrintCmdValue.h:  Contains the parameters needed for the Print
// command.  This object is passed to the Print command as a value.
////////////////////////////////////////////////////////////////
#ifndef SIPRINTCMDVALUE_H
#define SIPRINTCMDVALUE_H

#include "SiSaveCmdValue.h"

enum SiPrintDestination { PrintToPrinter, PrintToFile };
enum SiPrintOrientation { PrintPortrait, PrintLandscape };

struct SiPrintCmdValue : public SiSaveCmdValue {

    SiPrintDestination print_to;
    char print_command[256];
    char print_width[20];
    char print_height[20];
    SiPrintOrientation orientation;
    Boolean title_filename;
    char print_title[256];

    SiPrintCmdValue() : SiSaveCmdValue()		// Set defaults
	{  print_to = PrintToPrinter;  strcpy(print_command, "lp -c");
	   strcpy(print_width, "");  strcpy(print_height, "");
	   orientation = PrintPortrait;  strcpy(print_title, "");
	   title_filename = True;
	}

};

#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
