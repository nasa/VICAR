$!****************************************************************************
$!
$! Build proc for MIPL module zoom
$! VPACK Version 1.8, Wednesday, August 14, 1996, 17:45:22
$!
$! Execute by entering:		$ @zoom
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
$ write sys$output "*** module zoom ***"
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
$ write sys$output "Invalid argument given to zoom.com file -- ", primary
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
$   if F$SEARCH("zoom.imake") .nes. ""
$   then
$      vimake zoom
$      purge zoom.bld
$   else
$      if F$SEARCH("zoom.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake zoom
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @zoom.bld "STD"
$   else
$      @zoom.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create zoom.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack zoom.com -mixed -
	-s ZoomCmd.cc ZoomConstantCmd.cc ZoomFitCmd.cc ZoomCmdInterface.cc -
	   ZoomCmdSet.cc ZoomDialog.cc ZoomRadioCmdBox.cc ZoomSpecialCmd.cc -
	   ZoomSpecialCmdList.cc ZoomBaseCmd.cc -
	-i zoom.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ZoomCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomCmd.cc
//	
//	ZoomCmd performs a zoom on the image. 
//	This is subclass of Cmd which
//	implements the doit() and undoit() pure virtual 
//	functions.
//
////////////////////////////////////////////////////////////////
#include "ZoomCmd.h"
#include "ZoomFactor.h"
#include "BasicImageView.h"

ZoomCmd::ZoomCmd(const char *name, int active, BasicImageView *imageView)
		: Cmd(name, active)
{
   _imageView = imageView;
}

////////////////////////////////////////////////////////////////
// doit()
// Perform the requested zoom on the image
////////////////////////////////////////////////////////////////
void ZoomCmd::doit()
{

   // Save previous zoom

   ZoomFactor &zoom = _imageView->getImageZoom();
   _undoXin  = zoom.getXIn();
   _undoXout = zoom.getXOut();
   _undoYin  = zoom.getYIn();
   _undoYout = zoom.getYOut();

   // Now do new zoom

   if (!_value)			// in case we're executed with NULL
      return;

   ZoomFactor *z = (ZoomFactor *)_value;
   _imageView->setUserZoom(*z);	
} 

////////////////////////////////////////////////////////////////
// undoit()
// Undo the previous zoom
////////////////////////////////////////////////////////////////
void ZoomCmd::undoit()
{
   ZoomFactor zoom(_undoXin, _undoYin, _undoXout, _undoYout);
   _imageView->setUserZoom(zoom);
} 

////////////////////////////////////////////////////////////////
// Free the ZoomFactor value
////////////////////////////////////////////////////////////////
void ZoomCmd::freeValue(CmdValue value)
{
   ZoomFactor *z = (ZoomFactor *)value;
   if (z)
      delete z;
}

////////////////////////////////////////////////////////////////
// Returns the Undo zoom factor
////////////////////////////////////////////////////////////////
ZoomFactor ZoomCmd::getUndoZoom()
{
   ZoomFactor zoom(_undoXin, _undoYin, _undoXout, _undoYout);
   return zoom;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomConstantCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomConstantCmd.cc
//
//	Is a (indirect) subclass of RadioCmd.  Used for reading in the
//	zoom values in the constructor and passing the
//	values in a   (CmdValue) ZoomValue structure 
//	to it's parent class.
//
////////////////////////////////////////////////////////////////
#include "ZoomConstantCmd.h"
#include "BasicImageView.h"

////////////////////////////////////////////////////////////////
// Constructor with zoom info
////////////////////////////////////////////////////////////////
ZoomConstantCmd::ZoomConstantCmd(const char *name, int active,
			int zoomIn, int zoomOut, 
			CmdList *radioCmdList, CmdValue startState,
			ZoomSpecialCmd *zSC, BasicImageView *imageView ) 
		: ZoomBaseCmd(name, active, startState, radioCmdList, zSC),
		  _zoomValue(zoomIn, zoomIn, zoomOut, zoomOut)
{
   _imageView = imageView;
}

////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////
ZoomConstantCmd::~ZoomConstantCmd()
{
   // empty
}

////////////////////////////////////////////////////////////////
// doit()
// Perform the requested zoom
////////////////////////////////////////////////////////////////
void ZoomConstantCmd::doit()
{

   // Do zoom only if value is True

   if (_value) {
      _imageView->setUserZoom(_zoomValue);
   }
} 

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomFitCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomFitCmd.cc
//	
//	This class is a zoom control for performing a 
//	zoom-to-fit on the image.
//	Unlike ZoomConstantCmd,
//	it doesn't need a value to perform its function.
//	It implements the doit() and undoit() pure virtual
//	functions in its parent class: Cmd.
//
////////////////////////////////////////////////////////////////
#include "ZoomFitCmd.h"
#include "BasicImageView.h"

ZoomFitCmd::ZoomFitCmd(const char *name, int active, CmdList *radioCmdList,
			CmdValue startState, ZoomSpecialCmd *zSC,
			BasicImageView *imageView)
		: ZoomBaseCmd(name, active, startState, radioCmdList, zSC)
{
   _imageView = imageView;
}

////////////////////////////////////////////////////////////////
// doit()
// Set zoom-to-fit mode.  We don't need to worry about Undo because
// RadioCmd handles it for us.
////////////////////////////////////////////////////////////////
void ZoomFitCmd::doit()
{
   // Do zoom if radio cmd is True

   if (_value) {
      _imageView->setUserZoom2Fit();
   }	
} 

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomCmdInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
//ZoomCmdInterface.cc: Integer text field interface to a Cmd object
//////////////////////////////////////////////////////////////
#include "ZoomCmdInterface.h"
#include "Cmd.h"
#include "ZoomFactor.h"
#include "KeyinView.h"
#include "BasicImageView.h"
#include "XvicImage.h"
#include <Xm/Form.h>
#include "stdlib.h"		// for atoi()
#include "stdio.h"		// for sprintf()

ZoomCmdInterface::ZoomCmdInterface(Widget parent, Cmd *cmd,
			BasicImageView *imageView) // = NULL
		: CmdInterface(cmd)
{
   createAllSubViews(parent, imageView);
}

ZoomCmdInterface::ZoomCmdInterface(Widget parent, const char *name,
			BasicImageView *imageView) // = NULL
		: CmdInterface(name)
{
    createAllSubViews(parent, imageView);
}

ZoomCmdInterface::~ZoomCmdInterface()
{
   delete _zoomXIn;
   delete _zoomXOut;
   delete _zoomYIn;
   delete _zoomYOut;
}

//////////////////////////////////////////////////////////////
// Add the KeyinView for one text field
//////////////////////////////////////////////////////////////

KeyinView *ZoomCmdInterface::addOneSubView(Widget parent, const char *name)
{
   KeyinView *view = new KeyinView(parent, name);
   view->manage();
   view->installCallback(&CmdInterface::executeCmdCallback, (XtPointer)this);
   return view;
}

//////////////////////////////////////////////////////////////
// Add keyins for all four components of the zoom.  Also, set up
// the zoom-changed callback on the image widget, and set up the
// initial value of the text widgets.
//////////////////////////////////////////////////////////////

void ZoomCmdInterface::createAllSubViews(Widget parent,
				BasicImageView *imageView)
{
   _w = XtCreateManagedWidget(_name, xmFormWidgetClass, parent,
			 NULL, 0 );
   installDestroyHandler();

   _zoomXIn  = addOneSubView( _w, "Zoom X In" );
   XtVaSetValues(_zoomXIn->baseWidget(),
	XmNleftAttachment, XmATTACH_FORM, XmNrightAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_FORM,
	NULL);
   _zoomXOut = addOneSubView( _w, "Zoom X Out" );
   XtVaSetValues(_zoomXOut->baseWidget(),
	XmNleftAttachment, XmATTACH_FORM, XmNrightAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_WIDGET, XmNtopWidget, _zoomXIn->baseWidget(),
	NULL);
   _zoomYIn  = addOneSubView( _w, "Zoom Y In" );
   XtVaSetValues(_zoomYIn->baseWidget(),
	XmNleftAttachment, XmATTACH_FORM, XmNrightAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_WIDGET, XmNtopWidget,_zoomXOut->baseWidget(),
	NULL);
   _zoomYOut = addOneSubView( _w, "Zoom Y Out" );
   XtVaSetValues(_zoomYOut->baseWidget(),
	XmNleftAttachment, XmATTACH_FORM, XmNrightAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_WIDGET, XmNtopWidget, _zoomYIn->baseWidget(),
	NULL);

   // Set the initial value from the widget if present, or the cmd if not

   if (imageView && imageView->getWidget()) {
      XtAddCallback(imageView->getWidget(), XvicNvisibleAreaCallback,
		&ZoomCmdInterface::newZoomCallback, (XtPointer)this);
      newZoom(imageView->getWidget());
   }
   else if (_cmd)
      setValue(_cmd->getValue());
}

//////////////////////////////////////////////////////////////
// If any of the zoom values changes, create a ZoomFactor object
// and execute the command.
//////////////////////////////////////////////////////////////

void ZoomCmdInterface::executeCmd(XtPointer)
{
   char *in_text, *out_text;
   ZoomFactor *z = new ZoomFactor;

   in_text = _zoomXIn->getFieldValue();
   out_text = _zoomXOut->getFieldValue();
   z->setX(atoi(in_text), atoi(out_text));
   XtFree(in_text);
   XtFree(out_text);

   in_text = _zoomYIn->getFieldValue();
   out_text = _zoomYOut->getFieldValue();
   z->setY(atoi(in_text), atoi(out_text));
   XtFree(in_text);
   XtFree(out_text);

   runCmd((CmdValue)z);
}

//////////////////////////////////////////////////////////////
// If the zoom changes, update what's displayed in the text fields
//////////////////////////////////////////////////////////////

void ZoomCmdInterface::setValue(CmdValue value)
{
   char buf[50];
   ZoomFactor *z = (ZoomFactor *) value;

   CmdInterface::setValue(value);	// Removes cmd from deferred list

   if (z == NULL)			// No value has been set
      return;

   sprintf(buf, "%d", z->getXIn());
   _zoomXIn->setFieldValue(buf);
 
   sprintf(buf, "%d", z->getXOut());
   _zoomXOut->setFieldValue(buf);
    
   sprintf(buf, "%d", z->getYIn());
   _zoomYIn->setFieldValue(buf);

   sprintf(buf, "%d", z->getYOut());
   _zoomYOut->setFieldValue(buf);
}

//////////////////////////////////////////////////////////////
// Callback called when the zoom factor changes
//////////////////////////////////////////////////////////////

void ZoomCmdInterface::newZoomCallback(Widget w, XtPointer clientData,
						 XtPointer callData)
{
   ZoomCmdInterface *obj = (ZoomCmdInterface *)clientData;
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)callData;

   if (cb->reason != XvicCR_VISIBLE_AREA)
      return;
   if (! (cb->flags & XvicZOOM_CHANGED))
      return;

   obj->newZoom(w);
}

//////////////////////////////////////////////////////////////
// New zoom factor in widget; update display
//////////////////////////////////////////////////////////////

void ZoomCmdInterface::newZoom(Widget w)
{
   int xZoomIn, xZoomOut, yZoomIn, yZoomOut;

   XtVaGetValues(w, XvicNxZoomIn, &xZoomIn, XvicNxZoomOut, &xZoomOut,
		    XvicNyZoomIn, &yZoomIn, XvicNyZoomOut, &yZoomOut, NULL);
   ZoomFactor z(xZoomIn, yZoomIn, xZoomOut, yZoomOut);
   setValue((CmdValue)&z);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomCmdSet.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomCmdSet.cc - creates and maintains all the Zoom commands
////////////////////////////////////////////////////////////////
#include "ZoomCmdSet.h"
#include "BasicImageView.h"
#include "MenuCmdList.h"
#include "CmdList.h"
#include "ZoomCmd.h"
#include "ZoomFitCmd.h"
#include "ZoomConstantCmd.h"
#include "ZoomSpecialCmd.h"
#include "ZoomSpecialCmdList.h"

ZoomCmdSet::ZoomCmdSet(BasicImageView *imageView)
{
   Cmd *cmd;

   _imageView = imageView;

   _menuCmdList = new MenuCmdList("Zoom");
   _radioList = new CmdList;

   _zoomCmd = new ZoomCmd("Zoom", True, imageView);

   // Commands related to the special zoom

   _zoomSpecialCmdList = new ZoomSpecialCmdList("ZoomSpecialCmdList");

   _zoomSpecialCmd = new ZoomSpecialCmd("Special", True, _radioList,
				(CmdValue)False, _zoomSpecialCmdList);
   _menuCmdList->addRadioButton(_zoomSpecialCmd);

   // Normal (constant) zoom commands

   _zoomFitCmd = new ZoomFitCmd("Zoom To Fit", True, _radioList,(CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(_zoomFitCmd);
   cmd = new ZoomConstantCmd("8 x", True, 8, 1, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("4 x", True, 4, 1, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("3 x", True, 3, 1, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("2 x", True, 2, 1, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("None",True, 1, 1, _radioList, (CmdValue)True,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/2", True, 1, 2, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/3", True, 1, 3, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/4", True, 1, 4, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/5", True, 1, 5, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/6", True, 1, 6, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/8", True, 1, 8, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/10",True, 1,10, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/16",True, 1,16, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);

}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ZoomDialog.cc - Dialog box for the zoom factor
////////////////////////////////////////////////////////////////
#include "ZoomDialog.h"
#include "ZoomCmdInterface.h"
#include "ZoomRadioCmdBox.h"
#include "ZoomCmdSet.h"
#include "ZoomCmd.h"
#include "ZoomSpecialCmd.h"
#include "ZoomSpecialCmdList.h"
#include <Xm/Form.h>

ZoomDialog::ZoomDialog(const char *name, ZoomCmdSet *zoomCmdSet)
		: CustomDialog(name)
{
   _zoomCmdSet = zoomCmdSet; 
}

ZoomDialog::~ZoomDialog()
{
   delete _zoomRadioCmdBox;
   delete _zoomCmdInterface;
}

////////////////////////////////////////////////////////////////
// Create the actual dialog box here
////////////////////////////////////////////////////////////////

Widget ZoomDialog::createWorkArea(Widget parent)
{
   // Create work area widget

   Widget form = XtVaCreateWidget("ZoomDialog", xmFormWidgetClass, parent,
		NULL);

   // Create a radiobox of zoom commands
   _zoomRadioCmdBox = new ZoomRadioCmdBox(form, "ZoomRadioCmdBox",
				_zoomCmdSet->getRadioList(), _applyCmdList);
   XtVaSetValues(_zoomRadioCmdBox->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		NULL);
   _zoomRadioCmdBox->manage();

   // Create ZoomCmdInterface and defer its execution to the ZoomSpecialCmdList

   _zoomCmdInterface = new ZoomCmdInterface(form, _zoomCmdSet->getZoomCmd(),
				_zoomCmdSet->getImageView());
   XtVaSetValues(_zoomCmdInterface->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, _zoomRadioCmdBox->baseWidget(),
		NULL);
   _zoomCmdInterface->manage();
   _zoomCmdInterface->setDeferredExec(_zoomCmdSet->getZoomSpecialCmdList());

   // Pass the ZoomSpecialCmd interface widget to ZoomSpecialCmdList

   Widget w =
	_zoomRadioCmdBox->getInterfaceWidget(_zoomCmdSet->getZoomSpecialCmd());
   _zoomCmdSet->getZoomSpecialCmdList()->setInterfaceWidget(w);

   return form;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomRadioCmdBox.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// ZoomRadioCmdBox.cc - Just like RadioCmdBox but adds capability to
// find one interface given the command it's attached to.
//////////////////////////////////////////////////////////////
#include "ZoomRadioCmdBox.h"
#include "CmdList.h"
#include "RadioButtonInterface.h"

Widget ZoomRadioCmdBox::getInterfaceWidget(Cmd *cmd)
{    
   for (int i = 0; i < _numElements; i++) {
      if (cmd == (*_cmdList)[i]) 
         return (_list[i]->baseWidget());
   }
   return NULL;		// shouldn't happen
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomSpecialCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ZoomSpecialCmd.cc
//
//	This class is a zoom control for managing a 
//	dialog with zoom commands.
////////////////////////////////////////////////////////////////
#include "ZoomSpecialCmd.h"
#include "ZoomSpecialCmdList.h"
#include "ZoomDialog.h"
#include "CmdList.h"
#include <assert.h>

ZoomSpecialCmd::ZoomSpecialCmd(const char *name, int active, CmdList *radioCmdList,
			CmdValue startState, ZoomSpecialCmdList *zSCL) 
		: ZoomBaseCmd(name, active, startState, radioCmdList, NULL) 
{
   _zoomDialog = NULL;
   _zoomSpecialCmdList = zSCL;
}

ZoomSpecialCmd::~ZoomSpecialCmd()
{
   delete _zoomSpecialCmdList;
}

////////////////////////////////////////////////////////////////
// Post the dialog and execute the ZoomSpecialCmdList.
////////////////////////////////////////////////////////////////
void ZoomSpecialCmd::doit()
{
   assert(_zoomDialog != NULL);
   assert(_zoomSpecialCmdList != NULL);

   if (_value) {
      _zoomDialog->post();
      _zoomSpecialCmdList->execute();
   }
}

////////////////////////////////////////////////////////////////
// When another zoom radio button is undone, and we're the previous one,
// then we can't just get executed again because zoomSpecialCmdList
// has forgotten what was on its list.  If we're also current, then
// the undo is done differently (sigh).
////////////////////////////////////////////////////////////////
void ZoomSpecialCmd::undoZoom()
{
   if (_value)
      _zoomSpecialCmdList->undoZoom(True);
   else
      _zoomSpecialCmdList->undoZoom(False);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomSpecialCmdList.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomSpecialCmdList.cc
/////////////////////////////////////////////////////////////////
#include "ZoomSpecialCmdList.h"
#include "ZoomCmd.h"
#include <Xm/ToggleB.h>

// We really want to trap addUnique(), but since it calls add() anyway,
// we can catch both birds with the same stone.

void ZoomSpecialCmdList::add(Cmd *cmd, CmdValue value)	// value = NULL
{
   CmdList::add(cmd, value);
   if (_button != NULL) {
      XmToggleButtonSetState(_button, False, False);
      XmToggleButtonSetState(_button, True, True);
   }
}

// Trap doit() so that we can save the zoom value for undo.  Yuck.

void ZoomSpecialCmdList::doit()
{
   int saveNumElements = _numElements;
   if (saveNumElements > 0) {
      _undoZoomCmd = NULL;

      for (int i=0; i<_numElements; i++) {
         if (strcmp(_contents[i]->className(), "ZoomCmd") == 0)
            _undoZoomCmd = (ZoomCmd *)_contents[i];
      }
   }

   CmdList::doit();

   if (saveNumElements > 0) {
      if (_undoZoomCmd)
         _undoZoom = *((ZoomFactor *)_undoZoomCmd->getValue());
   }

}

// If the current and previous commands are both Special zooms, we need to
// use the ZoomCmd's undo capability.  If the current is not Special, then
// we need to undo the zoom by executing the command with the saved value
// (we can't do that in the first case because the saved value is the *new*
// zoom factor.  yucky kludge).

void ZoomSpecialCmdList::undoZoom(Boolean useUndo)
{
   if (_undoZoomCmd) {
      if (useUndo) {
         _undoZoomCmd->undo();
         _undoZoom = _undoZoomCmd->getUndoZoom();
      }
      else
         _undoZoomCmd->execute(new ZoomFactor(_undoZoom));
   }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ZoomBaseCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// ZoomBaseCmd.cc - superclass for all Zoom radio buttons.  Needed in
// order for Undo of the special command to work in the dialog.  Kinda
// kludgy but it works.
////////////////////////////////////////////////////////////////
#include "ZoomBaseCmd.h"
#include "ZoomSpecialCmd.h"
 
ZoomBaseCmd::ZoomBaseCmd(const char *name, int active, CmdValue startState,
		CmdList *radioCmdList, ZoomSpecialCmd *zoomSpecialCmd)
	: RadioCmd(name, active, startState, radioCmdList)
{
   if (zoomSpecialCmd == NULL)
      _zoomSpecialCmd = (ZoomSpecialCmd *)this;		// We're the ZSC
   else
      _zoomSpecialCmd = zoomSpecialCmd;
}

void ZoomBaseCmd::undoit()
{
   if (_lastTrueCmd == _zoomSpecialCmd) {
      _zoomSpecialCmd->undoZoom();
   }

   RadioCmd::undoit();
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create zoom.imake
#define SUBROUTINE zoom
#define MODULE_LIST ZoomCmd.cc ZoomConstantCmd.cc ZoomFitCmd.cc \
 ZoomCmdInterface.cc ZoomCmdSet.cc ZoomDialog.cc ZoomRadioCmdBox.cc \
 ZoomSpecialCmd.cc ZoomSpecialCmdList.cc ZoomBaseCmd.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_RTL
#define LIB_TAE
#define LIB_MOTIF
#define LIB_MOTIFAPP

$ Return
$!#############################################################################
