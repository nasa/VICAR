$!****************************************************************************
$!
$! Build proc for MIPL module zoom_h
$! VPACK Version 1.8, Friday, July 26, 1996, 14:06:59
$!
$! Execute by entering:		$ @zoom_h
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
$ write sys$output "*** module zoom_h ***"
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
$ write sys$output "Invalid argument given to zoom_h.com file -- ", primary
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
$   if F$SEARCH("zoom_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @zoom_h.bld "STD"
$   else
$      @zoom_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create zoom_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack zoom_h.com -mixed -
	-s PostZoomDialogCmd.h ZoomCmd.h ZoomConstantCmd.h ZoomFitCmd.h -
	   ZoomCmdInterface.h ZoomCmdSet.h ZoomDialog.h ZoomRadioCmdBox.h -
	   ZoomSpecialCmd.h ZoomSpecialCmdList.h ZoomBaseCmd.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create PostZoomDialogCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// PostZoomDialogCmd.h - simple command to post the zoom dialog
////////////////////////////////////////////////////////////////
#ifndef POSTZOOMDIALOGCMD_H
#define POSTZOOMDIALOGCMD_H

#include "NoUndoCmd.h"
#include "ZoomDialog.h"
#include <assert.h>
 
class PostZoomDialogCmd : public NoUndoCmd {

 protected:

   ZoomDialog *_dialog;
 
 public:
 
   PostZoomDialogCmd(const char *name, int active, ZoomDialog *z)
			: NoUndoCmd(name, active)  
		{ _dialog = z; }

   virtual void doit() { assert(_dialog != NULL); _dialog->post(); }

};
 
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomCmd.h
//	
//	ZoomCmd performs a zoom on the image. 
//	This is subclass of Cmd which
//	implements the doit() and undoit() pure virtual 
//	functions.
//
////////////////////////////////////////////////////////////////
#ifndef ZOOMCMD_H
#define ZOOMCMD_H
#include "Cmd.h"

class ZoomFactor;
class BasicImageView;

class ZoomCmd : public Cmd {

 protected:

   BasicImageView *_imageView;
   int _undoXin;
   int _undoXout;
   int _undoYin;
   int _undoYout;

   virtual void doit();
   virtual void undoit();

 public:

   // Constructor with no zoom values - used to defer until 'ok' button pressed
   ZoomCmd(const char *name, int active, BasicImageView *imageView);

   virtual void freeValue(CmdValue);

   virtual ZoomFactor getUndoZoom();

   virtual const char *const className() { return ("ZoomCmd"); }

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomConstantCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomConstantCmd.h
//
//	Is a (indirect) subclass of RadioCmd.  Used for reading in the
//	zoom values in the constructor and passing the
//	values in a (CmdValue) ZoomFactor structure
//	to its parent class.
//
////////////////////////////////////////////////////////////////
#ifndef ZOOMCONSTANTCMD_H
#define ZOOMCONSTANTCMD_H
#include "ZoomBaseCmd.h"
#include "ZoomFactor.h"
 
class BasicImageView;
class ZoomSpecialCmd;

class ZoomConstantCmd :  public ZoomBaseCmd  {
 
 protected:
 
   BasicImageView *_imageView;
   ZoomFactor _zoomValue;

   virtual void	doit();
 
 public:

   // Constructor with zoom values in arg list

   ZoomConstantCmd(const char * name, int active,  int zoomIn, int zoomOut, 
        	CmdList * radioCmdList, CmdValue startState,
		ZoomSpecialCmd *zSC, BasicImageView * imageView ); 

   virtual ~ZoomConstantCmd();
   virtual const char *const className() { return ("ZoomConstantCmd"); }
 
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomFitCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomFitCmd.h
//	
//	This class is a zoom control for performing a 
//	zoom-to-fit on the image.
//	Unlike ZoomConstantCmd,
//	it doesn't need a value to perform its function.
//	It implements the doit() and undoit() pure virtual
//	functions in its parent class: Cmd.
//
////////////////////////////////////////////////////////////////
#ifndef ZOOMFITCMD_H
#define ZOOMFITCMD_H
#include "ZoomBaseCmd.h"

class BasicImageView;
class ZoomSpecialCmd;

class ZoomFitCmd : public ZoomBaseCmd {

 protected:

   BasicImageView *_imageView;
	
   virtual void doit();

 public:

   ZoomFitCmd(const char *, int, CmdList *, CmdValue, ZoomSpecialCmd *,
			BasicImageView *);

   virtual ~ZoomFitCmd( ) {};
   virtual const char *const className() { return ("ZoomFitCmd"); }

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomCmdInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// ZoomCmdInterface.h
///////////////////////////////////////////////////////////////
#ifndef ZOOMCMDINTERFACE
#define ZOOMCMDINTERFACE
#include "CmdInterface.h"
#include <Xm/Xm.h>

class KeyinView;
class BasicImageView;

class ZoomCmdInterface : public CmdInterface {
 private:
   static void newZoomCallback(Widget, XtPointer, XtPointer);

 protected:

   KeyinView *_zoomXIn;
   KeyinView *_zoomXOut;
   KeyinView *_zoomYIn;
   KeyinView *_zoomYOut;

   virtual KeyinView *addOneSubView(Widget parent, const char *name);
   virtual void createAllSubViews(Widget parent, BasicImageView *imageView);
   virtual void executeCmd(XtPointer=NULL);
   virtual void setValue(CmdValue);
   virtual void newZoom(Widget);

 public:

   ZoomCmdInterface(Widget, Cmd *, BasicImageView * = NULL);
   ZoomCmdInterface(Widget, const char *, BasicImageView * = NULL);
   ~ZoomCmdInterface();
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomCmdSet.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomCmdSet.h - creates and maintains all the Zoom commands
////////////////////////////////////////////////////////////////
#ifndef ZOOMCMDSET_H
#define ZOOMCMDSET_H

class BasicImageView;
class MenuCmdList;
class CmdList;
class ZoomSpecialCmdList;
class ZoomSpecialCmd;
class ZoomFitCmd;
class ZoomCmd;

class ZoomCmdSet {

 protected:

   MenuCmdList *_menuCmdList;
   CmdList *_radioList;
   ZoomSpecialCmdList *_zoomSpecialCmdList;
   ZoomSpecialCmd *_zoomSpecialCmd;
   ZoomCmd *_zoomCmd;
   ZoomFitCmd *_zoomFitCmd;
   BasicImageView *_imageView;

 public:

   ZoomCmdSet(BasicImageView *imageView);

   MenuCmdList *getMenuList() { return _menuCmdList; }
   CmdList *getRadioList() { return _radioList; }
   ZoomCmd *getZoomCmd() { return _zoomCmd; }
   ZoomSpecialCmdList *getZoomSpecialCmdList() { return _zoomSpecialCmdList; }
   ZoomSpecialCmd *getZoomSpecialCmd() { return _zoomSpecialCmd; }
   ZoomFitCmd *getZoomFitCmd() { return _zoomFitCmd; }
   BasicImageView *getImageView() { return _imageView; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomDialog.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomDialog.h - Dialog box for the zoom factor
////////////////////////////////////////////////////////////////
#ifndef ZOOMDIALOG_H
#define ZOOMDIALOG_H
#include "CustomDialog.h"
#include "HelpBrowser.h"
#include <Xm/Xm.h>

class ZoomCmdSet;
class ZoomRadioCmdBox;
class ZoomCmdInterface;

class ZoomDialog : public CustomDialog {

 protected:

   ZoomCmdSet *_zoomCmdSet;
   ZoomRadioCmdBox *_zoomRadioCmdBox;
   ZoomCmdInterface *_zoomCmdInterface;

   virtual Widget createWorkArea(Widget);

   virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

 public:

   ZoomDialog(const char *name, ZoomCmdSet *);
   virtual ~ZoomDialog();
   virtual const char *const className() { return ("ZoomDialog"); }

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomRadioCmdBox.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// ZoomRadioCmdBox.h - Just like RadioCmdBox but adds capability to
// find one interface given the command it's attached to.
//////////////////////////////////////////////////////////////
#ifndef ZOOMRADIOCMDBOX_H
#define ZOOMRADIOCMDBOX_H
#include "RadioCmdBox.h"
#include <Xm/Xm.h>
 
class Cmd;
class CmdList;

class ZoomRadioCmdBox : public RadioCmdBox {

 protected:

   CmdList *_cmdList;
 
 public:

   ZoomRadioCmdBox(Widget w, const char *name, CmdList *cmdList,
							CmdList *deferList)
    			: RadioCmdBox(w, name, cmdList, deferList)
		{ _cmdList = cmdList; }

   virtual Widget getInterfaceWidget(Cmd *cmd);
 
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomSpecialCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomSpecialCmd.h
//
//	This class is a zoom control for managing a 
//	dialog with zoom commands.
////////////////////////////////////////////////////////////////
#ifndef ZOOMSPECIALCMD_H
#define ZOOMSPECIALCMD_H
#include "ZoomBaseCmd.h"

class CmdList;
class ZoomDialog;
class ZoomSpecialCmdList;

class ZoomSpecialCmd : public ZoomBaseCmd {

 protected:

   ZoomDialog *_zoomDialog;
   ZoomSpecialCmdList *_zoomSpecialCmdList;

   virtual void doit();
 public:

   ZoomSpecialCmd(const char *name, int active, CmdList *radioCmdList, 
			CmdValue startState, ZoomSpecialCmdList *zSCL);
   virtual ~ZoomSpecialCmd();

   virtual void undoZoom();

   virtual void setDialog(ZoomDialog *z) { _zoomDialog = z; }

   virtual const char *const className() { return ("ZoomSpecialCmd"); }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomSpecialCmdList.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomSpecialCmdList.h - Command list that maintains the desired
// "special" zoom factor.  The keyins for the zoom factor are deferred
// to this list, which is executed by ZoomSpecialCmd.
/////////////////////////////////////////////////////////////////
#ifndef ZOOMSPECIALCMDLIST_H
#define ZOOMSPECIALCMDLIST_H
#include "CmdList.h"
#include "ZoomFactor.h"
#include <Xm/Xm.h>

class ZoomCmd;

class ZoomSpecialCmdList : public CmdList {
 protected:

   Widget _button;			// radiobutton in dialog.

   ZoomFactor _undoZoom;		// Last zoom factor, used for undo
   ZoomCmd *_undoZoomCmd;		// Zoom command, used for undo

   virtual void doit();

 public:
   ZoomSpecialCmdList() : CmdList()
			{ _button = NULL; _undoZoomCmd = NULL; }
   ZoomSpecialCmdList(const char *name) : CmdList(name)
			{ _button = NULL; _undoZoomCmd = NULL; }

   virtual ~ZoomSpecialCmdList() { };

   void setInterfaceWidget(Widget w) { _button = w; }

   // Override CmdList functions to add setting the toggle button
   virtual void add(Cmd *cmd, CmdValue value = NULL);

   virtual void undoZoom(Boolean);

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomBaseCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomBaseCmd.h - superclass for all Zoom radio buttons.  Needed in
// order for Undo of the special command to work in the dialog.  Kinda
// kludgy but it works.
////////////////////////////////////////////////////////////////
#ifndef ZOOMBASECMD_H
#define ZOOMBASECMD_H
#include "RadioCmd.h"
 
class ZoomSpecialCmd;

class ZoomBaseCmd :  public RadioCmd  {
 
 protected:

   ZoomSpecialCmd *_zoomSpecialCmd;
 
   virtual void	undoit();
 
 public:

   ZoomBaseCmd(const char *, int, CmdValue, CmdList *, ZoomSpecialCmd *);

   virtual const char *const className() { return ("ZoomBaseCmd"); }
 
};
#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
