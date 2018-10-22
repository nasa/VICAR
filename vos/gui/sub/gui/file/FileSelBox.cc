////////////////////////////////////////////////////////////////////////
// Compenent that implements a file selection box.
////////////////////////////////////////////////////////////////////////

#include "FileSelBox.h"
#include "FileTextInterface.h"
#include "Cmd.h"
#include "HelpBrowser.h"
#include <Xm/FileSB.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>

////////////////////////////////////////////////////////////////////////
// Since the Motif FileSelectionBox provides OK/Cancel buttons for us,
// we do the dialog stuff a little differently here.  When OK is hit, we
// explicitly execute the interface's Cmd object (rather than using a
// deferred list).  Also, the popdown action of OK/Cancel is done by
// calling mainWindow->unmanage().  It is assumed that mainWindow is a
// subclass of MainWindow that contains this object, but it can in reality
// be any UIComponent (NULL is okay too).
////////////////////////////////////////////////////////////////////////

FileSelBox::FileSelBox(Widget parent, const char *name, Cmd *loadFileCmd,
			UIComponent *mainWindow)
			// = NULL
		: UIComponent(name)
{
   _mainWindow = mainWindow;

   _w = XtVaCreateWidget(name, xmFileSelectionBoxWidgetClass, parent,
	NULL);
   installDestroyHandler();

   XtUnmanageChild(XmFileSelectionBoxGetChild(_w,XmDIALOG_TEXT));
   XtUnmanageChild(XmFileSelectionBoxGetChild(_w,XmDIALOG_SELECTION_LABEL));

   _fileText = new FileTextInterface(_w, loadFileCmd);
   _fileText->manage();

   // Add an Apply button.  We only do this for Motif 1.2 or later since
   // 1.1 FileSelectionBox can only handle one child.  If we're running
   // 1.1, there's no Apply button, but that's not a major loss since the
   // user can always hit OK.

#if XmVERSION != 1 || XmREVISION > 1
   _applyWidget = XtVaCreateManagedWidget("Apply", xmPushButtonWidgetClass, _w,
		NULL);
   XtAddCallback(_applyWidget, XmNactivateCallback,
		&FileSelBox::applyButtonCallback, (XtPointer)this);
#else
   _applyWidget = NULL;
#endif

   XtAddCallback(XmFileSelectionBoxGetChild(_w,XmDIALOG_TEXT),
		XmNvalueChangedCallback,
		&FileSelBox::newTextCallback, (XtPointer) this);

   XtAddCallback(XmFileSelectionBoxGetChild(_w,XmDIALOG_OK_BUTTON),
		XmNactivateCallback,
		&FileSelBox::okButtonCallback, (XtPointer) this);

   XtAddCallback(_w, XmNcancelCallback,
		&FileSelBox::cancelButtonCallback, (XtPointer)this);

   XtAddCallback(_w, XmNhelpCallback,
		&FileSelBox::helpButtonCallback, (XtPointer)this);
}

////////////////////////////////////////////////////////////////////////
// Callback called when the "real" Selection text changes.  We use
// this to notify the FileTextInterface of the new value.
////////////////////////////////////////////////////////////////////////

void FileSelBox::newTextCallback(Widget w, XtPointer clientData, XtPointer)
{
   FileSelBox *obj = (FileSelBox *)clientData;

   obj->newText(w);
}

////////////////////////////////////////////////////////////////////////
// Callback called when the "real" Selection text changes.  We use
// this to notify the FileTextInterface of the new value.
////////////////////////////////////////////////////////////////////////

void FileSelBox::newText(Widget w)
{
   char *text = XmTextGetString(w);

   _fileText->enterName(text);

   XtFree(text);
}

////////////////////////////////////////////////////////////////////////
// Callback called when the OK button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::okButtonCallback(Widget w, XtPointer clientData,
					    XtPointer callData)
{
   FileSelBox *obj = (FileSelBox *)clientData;

   obj->okButton(w, callData);
}

////////////////////////////////////////////////////////////////////////
// Callback called when the OK button is hit.  We must determine if this
// is the REAL OK button (user clicked on it or hit return with focus
// set there), or if it is a FAKE OK button caused by double-clicking on
// a List entry (or hitting return on the selection text, but since that's
// unmanaged it's not an issue here).
////////////////////////////////////////////////////////////////////////

void FileSelBox::okButton(Widget w, XtPointer callData)
{
   XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)callData;
   Boolean mine = True;

   // If it's a keyboard event, the focus widget has to be the OK button.
   // We can't just check to see if the window matches because SelectionBox
   // uses gadget children and thus OK has no window.

   if (cb->event->type == KeyPress || cb->event->type == KeyRelease) {
      if (XmGetFocusWidget(w) != w)
         mine = False;
   }

   // Otherwise, check to make sure the event's window is the same as the
   // OK button's.  If not, it came from somewhere else, probably the
   // file selection list.

   else {
      if (XtWindowOfObject(w) != cb->event->xany.window)
         mine = False;
   }

   if (!mine) {

      // FAKE OK.  The action here is to advance the "current" selection so
      // that the next text item will appear there.

      _fileText->nextFile();
   }
   else {

      // REAL OK.  Take the appropriate action.

      _fileText->startCmd();
      if (_mainWindow)
         _mainWindow->unmanage();
   }
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Apply button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::applyButtonCallback(Widget, XtPointer clientData, XtPointer)
{
   FileSelBox *obj = (FileSelBox *)clientData;

   obj->applyButton();
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Apply button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::applyButton()
{
   _fileText->startCmd();
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Cancel button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::cancelButtonCallback(Widget, XtPointer clientData, XtPointer)
{
   FileSelBox *obj = (FileSelBox *)clientData;

   obj->cancelButton();
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Cancel button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::cancelButton()
{
   if (_mainWindow)
      _mainWindow->unmanage();
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Help button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::helpButtonCallback(Widget w, XtPointer clientData,
					      XtPointer callData)
{
   FileSelBox *obj = (FileSelBox *)clientData;

   obj->helpButton(w, callData);
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Help button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::helpButton(Widget w, XtPointer)
{
   theHelpBrowser->run(XmFileSelectionBoxGetChild(w, XmDIALOG_HELP_BUTTON));
}

