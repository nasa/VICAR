$!****************************************************************************
$!
$! Build proc for MIPL module pseudocmd_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:55:34
$!
$! Execute by entering:		$ @pseudocmd_h
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
$ write sys$output "*** module pseudocmd_h ***"
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
$ write sys$output "Invalid argument given to pseudocmd_h.com file -- ", primary
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
$   if F$SEARCH("pseudocmd_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @pseudocmd_h.bld "STD"
$   else
$      @pseudocmd_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pseudocmd_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pseudocmd_h.com -mixed -
	-s PseudoCmd.h PseudoDialog.h PostPseudoCmd.h PseudoModeCmd.h -
	   LoadPseudoFileCmd.h SavePseudoFileCmd.h SetDeferredCmd.h -
	   ClearMarksCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create PseudoCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// PseudoCmd.h: Command class to execute the PSEUDOCOLOR
//                command.
//////////////////////////////////////////////////////////
#ifndef PSEUDOCMD_H
#define PSEUDOCMD_H
#include "NoUndoCmd.h"

class ImageWindow;
class Lut;

class PseudoCmd : public NoUndoCmd {

  protected:

    int _created;

    Lut *_lutR, *_lutG, *_lutB;

  public:

    PseudoCmd(const char *, int, Lut *, Lut *, Lut * );

    virtual void doit();

    virtual void freeValue(CmdValue);
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PseudoDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////
// PseudoDialog.h: Dialog box for pseudocolor tool
//////////////////////////////////////////////
#ifndef PSEUDODIALOG_H
#define PSEUDODIALOG_H
#include "MenuDialog.h"
#include "HelpBrowser.h"

class Cmd;
class CmdList;
class PseudoValue;
class PseudoMarks;

class PseudoDialog : public MenuDialog {

  private:

    static XtResource _resources[];

    Widget _iw, _form;
    Cmd *_cmd;
    Cmd *_modeCmd;
    Cmd *_setDeferredCmd;
    PseudoValue *_pseudoValue;
    PseudoMarks *_pseudoMarks;
    int _numPseudoFiles;
    char *_filename, *_dirUNIX, *_dirVMS;
    char *_fullFilename;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    PseudoDialog(const char *name, Cmd *, Cmd *, Widget);

    virtual Widget createWorkArea(Widget);
    virtual void createMenuPanes();

    virtual void post();
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PostPseudoCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// PostPseudoCmd.h
////////////////////////////////////////////////////////////////
#include "Cmd.h"
#include "MenuDialog.h"

class PostPseudoCmd : public NoUndoCmd {

  protected:

    MenuDialog *_dialog;

  public:

    PostPseudoCmd (const char *name, int active, MenuDialog *dialog) 
		: NoUndoCmd(name, active) { _dialog = dialog; }

    virtual void doit() { _dialog->post(); }
};
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PseudoModeCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// PseudoModeCmd.h
////////////////////////////////////////////////////////////////
#ifndef PSEUDOMODECMD_H
#define PSEUDOMODECMD_H
#include "Cmd.h"
#include <Xm/Xm.h>

class PseudoModeCmd : public Cmd {

 protected:

   Widget _iw;

   virtual void doit();
   virtual void undoit();

 public:

   PseudoModeCmd(const char *name, int active, Widget iw);

   virtual const char *const className() { return ("PseudoModeCmd"); }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LoadPseudoFileCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// LoadPseudoFileCmd: A Command class that loads an IBIS file.  The Command 
// value is a dynamically allocated single string, suitable for passing in 
// to a PseudoValue subclass.
/////////////////////////////////////////////////////////
#ifndef LOADSINGLEFILECMD_H
#define LOADSINGLEFILECMD_H
#include "NoUndoCmd.h"

class PseudoValue;

class LoadPseudoFileCmd : public NoUndoCmd {

 protected:

   char *_filename;
   PseudoValue *_pseudoValue;
    
 public:

   LoadPseudoFileCmd(const char *, int, PseudoValue *, char * = NULL);

   virtual void doit();  
    
   virtual void freeValue(CmdValue value) { if (value) delete[] (char *)value; }

   virtual const char *const className () { return "LoadPseudoFileCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SavePseudoFileCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// SavePseudoFileCmd: A Command class that saves pseudocolor tables in  an 
// IBIS file format.  The Command value is a dynamically allocated single 
// string, suitable for passing in to a PseudoValue subclass.
/////////////////////////////////////////////////////////
#ifndef SAVESINGLEFILECMD_H
#define SAVESINGLEFILECMD_H
#include "NoUndoCmd.h"

class PseudoValue;

class SavePseudoFileCmd : public NoUndoCmd {

 protected:

   PseudoValue *_pseudoValue;
    
 public:

   SavePseudoFileCmd(const char *, int, PseudoValue *);

   virtual void doit();  
    
   virtual void freeValue(CmdValue value) { if (value) delete[] (char *)value; }

   virtual const char *const className () { return "SavePseudoFileCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetDeferredCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// SetDeferredCmd.cc: Switches between immediate and
// deferred execution modes.  This command in intended to
// work with CheckBox widget.  If the CheckBox's value is
// True then deferred execution is set, else immediate
// execution mode is set
////////////////////////////////////////////////////////////////
#ifndef SETDEFERREDCMD_H
#define SETDEFERREDCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>

class SetDeferredCmd : public Cmd {

  protected:

    CmdInterface *_interface;
    CmdList 	 *_applyCmdList;

    Widget *_applyButton;

    virtual void doit();
    virtual void undoit();

  public:

    SetDeferredCmd(const char *name, int active, CmdInterface *, CmdList *,
								Widget*);

    virtual const char *const className() { return ("SetDeferredCmd"); }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ClearMarksCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// ClearMarksCmd.h.
/////////////////////////////////////////////////////////
#ifndef CLEARMARKSCMD_H
#define CLEARMARKSCMD_H
#include "NoUndoCmd.h"

class PseudoMarks;

class ClearMarksCmd : public NoUndoCmd {

 protected:

   PseudoMarks *_pseudoMarks;
    
 public:

   ClearMarksCmd(const char *, int, PseudoMarks *);

   virtual void doit();  
    
   virtual const char *const className () { return "ClearMarksCmd"; }
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
