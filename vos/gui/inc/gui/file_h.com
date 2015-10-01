$!****************************************************************************
$!
$! Build proc for MIPL module file_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:55:28
$!
$! Execute by entering:		$ @file_h
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
$ write sys$output "*** module file_h ***"
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
$ write sys$output "Invalid argument given to file_h.com file -- ", primary
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
$   if F$SEARCH("file_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @file_h.bld "STD"
$   else
$      @file_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create file_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack file_h.com -mixed -
	-s FileSelBox.h FileSelWindow.h FileTextInterface.h LoadFileCmd.h -
	   LoadMenuCmd.h ReloadFileCmd.h ImageToReloadGlue.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create FileSelBox.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// Compenent that implements a file selection box.
////////////////////////////////////////////////////////////////////////
#ifndef FILESELBOX_H
#define FILESELBOX_H

#include "UIComponent.h"

class FileTextInterface;
class Cmd;

class FileSelBox : public UIComponent {
 private:
   static void newTextCallback(Widget, XtPointer, XtPointer);
   static void okButtonCallback(Widget, XtPointer, XtPointer);
   static void applyButtonCallback(Widget, XtPointer, XtPointer);
   static void cancelButtonCallback(Widget, XtPointer, XtPointer);
   static void helpButtonCallback(Widget, XtPointer, XtPointer);

 protected:
   FileTextInterface *_fileText;
   Widget _applyWidget;
   UIComponent *_mainWindow;

   virtual void newText(Widget w);
   virtual void okButton(Widget, XtPointer);
   virtual void applyButton();
   virtual void cancelButton();
   virtual void helpButton(Widget, XtPointer);

 public:

   FileSelBox(Widget, const char *, Cmd *, UIComponent *mainWindow=NULL);

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create FileSelWindow.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// Compenent that creates a popup window containing a file selection box.
////////////////////////////////////////////////////////////////////////
#ifndef FILESELWINDOW_H
#define FILESELWINDOW_H

#include "MainWindow.h"
#ifdef ENABLE_SAGE
class SptParamMultiFileSel;
#else
class FileSelBox;
#endif
class Cmd;

class FileSelWindow : public MainWindow {

 protected:

   Widget _form;
#ifdef ENABLE_SAGE
   SptParamMultiFileSel *_fileSelBox;
#else
   FileSelBox *_fileSelBox;
#endif
   Cmd *_loadFileCmd;

   virtual Widget createWorkArea(Widget);

 public:

   FileSelWindow(const char *name, Cmd *);

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create FileTextInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// Compenent that implements the command interface (text keyin areas)
// for the FileSelBox.
////////////////////////////////////////////////////////////////////////
#ifndef FILETEXTINTERFACE_H
#define FILETEXTINTERFACE_H

#include "CmdInterface.h"

class FileTextInterface : public CmdInterface {
 private:
   static void toggleCallback(Widget, XtPointer, XtPointer);
   static void typeToggleCallback(Widget, XtPointer, XtPointer);
   static void fileFocusCallback(Widget, XtPointer, XtPointer);
   static void bandFocusCallback(Widget, XtPointer, XtPointer);

 protected:
   int _numFiles;
   int _activeIndex;
   Boolean _useBands;
   Widget *_toggleWidgetList;
   Widget *_fileWidgetList;
   Widget *_bandWidgetList;

   Widget _toggleForm, _filesForm, _bandsForm;

   Widget _typeForm, _typeLabel, _typeTogglePDS, _typeToggleVICAR;

   virtual void createWidgetRow(int);

   virtual void toggle(Widget, XtPointer);
   virtual void typeToggle(Widget, XtPointer);
   virtual void fileFocus(Widget, XtPointer);
   virtual void bandFocus(Widget, XtPointer);

   virtual void enableFileIndex(int);

 public:

   FileTextInterface(Widget, Cmd *, int numFiles=3, Boolean useBands=True,
						    Boolean useType=True);

   virtual void enterName(char *);
   virtual void nextFile();

   virtual void startCmd();
};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LoadFileCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// LoadFileCmd: A Command class that loads a file.  The Command value
// is a dynamically allocated single string, suitable for passing in to
// an ImageData subclass.
/////////////////////////////////////////////////////////
#ifndef LOADFILECMD_H
#define LOADFILECMD_H

#include "NoUndoCmd.h"

class ImageData;

class LoadFileCmd : public NoUndoCmd {

 protected:

   ImageData *_imageData;
    
 public:

   LoadFileCmd(const char *, int, ImageData *);

   virtual void doit();  
    
   virtual void freeValue(CmdValue value) { if (value) delete[] (char *)value; }

   virtual const char *const className () { return "LoadFileCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LoadMenuCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// LoadMenuCmd.h: Include file to handle posting the File dialog box.
////////////////////////////////////////////////////////////////////////
#ifndef LOADMENUCMD_H
#define LOADMENUCMD_H
#include "NoUndoCmd.h"

class MainWindow;
class Cmd;

class LoadMenuCmd : public NoUndoCmd {

 private:

   MainWindow *_fileSelWindow;
   Cmd *_loadFileCmd;

 protected:

   virtual void doit();

 public:

   LoadMenuCmd(const char *, int, Cmd *);
   virtual const char *const className () { return "LoadMenuCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ReloadFileCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// ReloadFileCmd: A Command class, the purpose of which
// is simply to call execute(filename) on a LoadFileCmd
// object.
/////////////////////////////////////////////////////////
#ifndef RELOADFILECMD_H
#define RELOADFILECMD_H

#include "NoUndoCmd.h"

class ImageToReloadGlue;
class ImageData;

class ReloadFileCmd : public NoUndoCmd {

 protected:

   ImageData *_image;
   ImageToReloadGlue *_glue;
   Cmd *_loadFileCmd;

 public:

   ReloadFileCmd(const char*, int, ImageData*, Cmd *loadFileCmd);
   ~ReloadFileCmd();

   virtual void doit();
    
   virtual const char *const className () { return "ReloadFileCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageToReloadGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ImageToReloadGlue: class that serves as a "glue" class between an
// ImageData object and a ReloadFileCmd object.
//
// This class, though a UIComponent, creates no widget, and therefore 
// should never be managed.
////////////////////////////////////////////////////////////////////////

#ifndef IMAGETORELOADGLUE_H
#define IMAGETORELOADGLUE_H
#include "BasicImageView.h"

class ImageData;
class Cmd;

class ImageToReloadGlue : public BasicImageView {

 private: 

 protected:

   Cmd *_reloadFileCmd;

 public:

   ImageToReloadGlue (ImageData *model, Cmd *reloadFileCmd);

   virtual ~ImageToReloadGlue ();

   virtual void update();
   virtual void updatePart(int flags);

   virtual const char *const className() { return  "ImageToReloadGlue"; }

};
#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
