$!****************************************************************************
$!
$! Build proc for MIPL module xvdprefs_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:55:47
$!
$! Execute by entering:		$ @xvdprefs_h
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
$ write sys$output "*** module xvdprefs_h ***"
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
$ write sys$output "Invalid argument given to xvdprefs_h.com file -- ", primary
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
$   if F$SEARCH("xvdprefs_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @xvdprefs_h.bld "STD"
$   else
$      @xvdprefs_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xvdprefs_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xvdprefs_h.com -mixed -
	-s BorderCmd.h ColorMapCmd.h ColorMapModes.h DitherCmd.h -
	   LookupTableCmd.h PrefDialog.h MenuBarCmd.h FullScreenDisplayCmd.h -
	   PrefDialogCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create BorderCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// BorderCmd.h: Turn the Motif border on and off for the shell
// ancestor of any BasicComponent subclass.
/////////////////////////////////////////////////////////////
#ifndef BORDERCMD_H
#define BORDERCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>

class BasicComponent;

class BorderCmd : public Cmd {
 private:
   Widget _shell;
   int _decorations;
   CmdValue _lastValue;		// for Undo

 protected:

   virtual void doit();   
   virtual void undoit(); 

 public:

   BorderCmd(const char *, int, BasicComponent *);
   void SetValue(CmdValue value) { _value = value; }
   virtual const char *const className () { return "BorderCmd"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ColorMapCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// ColorMapCmd.h: Include file for radio command class
//                  ColorMapCmd.
/////////////////////////////////////////////////////////////
#ifndef COLORMAPCMD_H
#define COLORMAPCMD_H
#include "RadioCmd.h"
#include "MainWindow.h"
#include <stdint.h>

class ColorMapModes;

class ColorMapCmd : public RadioCmd {
  private:
    Widget _imageWidget;
    unsigned char _colorMapValue;
    ColorMapModes *_colorMapModes;
  protected:
    
    virtual void doit();   

  public:
    
    ColorMapCmd ( const char *, int, unsigned char, Widget, ColorMapModes *,
							CmdList * );
    void setValue (Boolean setVal) { _value = (CmdValue) ((uintptr_t) setVal); newValue(); }
    unsigned char getColorMapMode() { return _colorMapValue; }
    virtual const char *const className () { return "ColorMapCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ColorMapModes.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// ColorMapModes.h: Include file for class
//                  ColorMapModes.
/////////////////////////////////////////////////////////////
#ifndef COLORMAPMODES_H
#define COLORMAPMODES_H
#include "MainWindow.h"

class CmdList;
class Cmd;

class ColorMapModes {
  private:
    Widget  _imageWidget;
    CmdList *_ditherRadList;
    CmdList *_colorMapRadList;
    CmdList *_stretchPolicyRadList;
    Cmd     **_ditherCmds;
    Cmd     **_colorMapCmds;
    Cmd     **_stretchPolicyCmds;
    unsigned char _colorMapPolicy, _ditherMode, _stretchPolicy;
    int _redLevels, _greenLevels, _blueLevels, _grayLevels;
    int _allocRedLevels, _allocGreenLevels, _allocBlueLevels, _allocGrayLevels;

    static void modeChangedCallback(Widget, XtPointer, XtPointer);

  protected:
    
    void modeChanged(XtPointer);

  public:
    
    ColorMapModes ( Widget, int, int, int, int );
    void SetColorMapModes();
    void SetColorMapButtons();
    void SetStretchPolicy( unsigned char value ) { _stretchPolicy = value; }
    void addRadLists( CmdList *, CmdList *, CmdList *);
    virtual const char *const className () { return "ColorMapModes"; }
    virtual ~ColorMapModes() {}
};
#endif






$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DitherCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// DitherCmd.h: Include file for radio command class
//                  DitherCmd.
/////////////////////////////////////////////////////////////
#ifndef DITHERCMD_H
#define DITHERCMD_H
#include "RadioCmd.h"
#include "MainWindow.h"
#include <stdint.h>

class ColorMapModes;

class DitherCmd : public RadioCmd {
  private:
    Widget _imageWidget;
    unsigned char _ditherMode;
    ColorMapModes * _colorMapModes;
  protected:
    
    virtual void doit();   

  public:
    
    DitherCmd ( const char *, int, unsigned char, Widget, ColorMapModes * ,
								CmdList *);
    void setValue (Boolean setVal) { _value = (CmdValue)((uintptr_t) setVal); newValue(); }
    unsigned char getDitherMode() { return _ditherMode;  }
    virtual const char *const className () { return "DitherCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LookupTableCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// LookupTableCmd.h: Include file for radio command class
//                  LookupTableCmd.
/////////////////////////////////////////////////////////////
#ifndef LOOKUPTABLECMD_H
#define LOOKUPTABLECMD_H
#include "RadioCmd.h"
#include "MainWindow.h"
#include <stdint.h>

class ColorMapModes;

class LookupTableCmd : public RadioCmd {
  private:
    Widget _imageWidget;
    unsigned char _LUTMode;
    ColorMapModes *_colorMapModes;
  protected:
    
    virtual void doit();   

  public:
    
    LookupTableCmd ( const char *, int, unsigned char, Widget, ColorMapModes *,
								CmdList *);
    void setValue (Boolean setVal) { _value = (CmdValue)((uintptr_t) setVal); newValue(); }
    virtual const char *const className () { return "LookupTableCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PrefDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
//PrefDialog.h: Include file to create the Preferences Dialog 
//              Box
//////////////////////////////////////////////////////////////
#ifndef PREFDIALOG_H
#define PREFDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class Cmd;
class CmdList;
class BasicImageView;

class PrefDialog: public CustomDialog {

  private:

    Cmd *_showSideBarCmd;
    Cmd *_showBorderCmd;
    Cmd *_showMenuBarCmd;
    Cmd *_showFullScreenCmd;

    Widget _imageViewWidget;
    BasicImageView *_imageView;
    int _CMMRedLevels,  _CMMGreenLevels, _CMMBlueLevels, _CMMGrayLevels;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    PrefDialog(const char *name, Cmd *, Cmd *, Cmd *, Cmd *, Widget, 
	       BasicImageView *, int, int, int, int);

    virtual Widget createWorkArea(Widget);
};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create MenuBarCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// MenuBarCmd.h: Include file for turning a MenuWindow's menu bar
//                on and off
/////////////////////////////////////////////////////////////
#ifndef MENUBARCMD_H
#define MENUBARCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>		// only for Boolean!

class MenuWindow;

class MenuBarCmd : public Cmd {
 private:
   int _oldValue;    // Last valid command for Undo
   MenuWindow *_menuWindow;
 protected:

   virtual void doit();   
   virtual void undoit(); 

 public:

   MenuBarCmd(const char *, int, MenuWindow *, Boolean=True);
   void SetValue(CmdValue value) { _value = value; }
   virtual const char *const className () { return "MenuBarCmd"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create FullScreenDisplayCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// FullScreenDisplayCmd.h: Turn off all decorations and make the
// image cover the entire screen.
/////////////////////////////////////////////////////////////
#ifndef FULLSCREENDISPLAYCMD_H
#define FULLSCREENDISPLAYCMD_H
#include "Cmd.h"
#include <X11/Intrinsic.h>		// for Dimension and Boolean only

class ImageDisplayView;

class FullScreenDisplayCmd : public Cmd {
 private:
   Cmd *_borderCmd;
   Cmd *_sideBarCmd;
   Cmd *_menuBarCmd;
   ImageDisplayView *_imageView;
   Widget _shell;

 protected:

   // The saved values are for use with Undo

   CmdValue _borderCmdSaveValue;
   CmdValue _sideBarCmdSaveValue;
   CmdValue _menuBarCmdSaveValue;
   Dimension _saveViewWidth, _saveViewHeight;
   Position _saveX, _saveY;
   Boolean _saveStoredValues;

   // The stored values are for executing with False.  We want to set it
   // back to what it was before the command was executed with True, regardless
   // of how many times it was executed with True.  So, undo isn't sufficient
   // and we must store the values separately.

   CmdValue _borderCmdStoreValue;
   CmdValue _sideBarCmdStoreValue;
   CmdValue _menuBarCmdStoreValue;
   Dimension _storeViewWidth, _storeViewHeight;
   Position _storeX, _storeY;
   Boolean _storedValues;	// True if we've stored something significant

   virtual void doit();   
   virtual void undoit(); 

 public:

   FullScreenDisplayCmd(const char *, int, Cmd *, Cmd *, Cmd *,
							ImageDisplayView *);
   virtual const char *const className () { return "FullScreenDisplayCmd"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PrefDialogCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
// PrefDialogCmd.h: Include file to define class for displaying
//                  Preferences Dialog Box for XVICDISP widget
///////////////////////////////////////////////////////////////
#ifndef PREFDIALOGCMD_H
#define PREFDIALOGCMD_H
#include "Cmd.h"
#include "CustomDialog.h"

class BasicImageView;

class PrefDialogCmd : public NoUndoCmd {
  private:
    CustomDialog *_dialog;
  protected:
    
    virtual void doit() { _dialog->post(); };   

  public:
    
    PrefDialogCmd ( const char *name , int active, CustomDialog *dialog )
		: NoUndoCmd( name, active) { _dialog = dialog; };
    virtual const char *const className () { return "PrefDialogCmd"; }
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
