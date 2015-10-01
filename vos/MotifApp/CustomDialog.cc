//////////////////////////////////////////////////////////
// CustomDialog.h: A base class for custom dialogs.  Intended for use
// with delayed command execution on the Ok/Apply buttons.  Caching
// is not really supported, but an unposted dialog is saved for later use
// (i.e. each object supports one and only one dialog).  The dialog is
// not created until it is needed.
//////////////////////////////////////////////////////////
#include "CustomDialog.h"
#include "Application.h"
#include "CmdList.h"
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

String CustomDialog::_defaults[] = {
    (char *)"*XmSash.traversalOn: False",
    NULL,
};

CustomDialog::CustomDialog( const char *name,
		ButtonState showOk, ButtonState showApply,
		ButtonState showReset,
		ButtonState showCancel, ButtonState showHelp )
	: MainWindow ( name )
		// ButtonState's all default to Visible except OK is Default
		// and Reset is Invisible
{
    _actionArea = NULL;
    _workArea = NULL;
    _numActionWidgets = 0;
    _posted = False;
    _showOk = showOk;
    if (showOk != Invisible)
        _numActionWidgets++;
    _showApply = showApply;
    if (showApply != Invisible)
        _numActionWidgets++;
    _showReset = showReset;
    if (showReset != Invisible)
        _numActionWidgets++;
    _showCancel = showCancel;
    if (showCancel != Invisible)
        _numActionWidgets++;
    _showHelp = showHelp;
    if (showHelp != Invisible)
        _numActionWidgets++;

    _applyCmdList = new CmdList((char *)"OkApply");
}

//////////////////////////////////////////////////////////

CustomDialog::~CustomDialog()
{
    if (_applyCmdList)
        delete _applyCmdList;
}

//////////////////////////////////////////////////////////
// Create the dialog shell and the standard parts.  This should not normally
// be overridden by subclasses.
//////////////////////////////////////////////////////////
Widget CustomDialog::createDialog( Widget parent )
{
    Widget shell;

    // Create the shell widget.  Unmap, don't destroy, when the Close box
    // is hit so that we don't have to recreate the thing later.

    shell = XtVaCreatePopupShell(_name, topLevelShellWidgetClass, parent,
                XmNdeleteResponse, XmUNMAP,
                NULL);

    _body = createBody(shell);

    return shell;
}

//////////////////////////////////////////////////////////
// Create the actual body area of the widget (inside the shell).
// This should not normally be overridden by subclasses.
//////////////////////////////////////////////////////////

Widget CustomDialog::createBody(Widget parent)
{
    // Create a PanedWindow (suggested by O'Reilly 6A) to manage things

    Widget pane;

    // The default resources are to turn off keyboard traversal to the Sash
    // in the PanedWindow.

    setDefaultResources(parent, _defaults);

    pane = XtVaCreateWidget("pane", xmPanedWindowWidgetClass, parent,
		XmNsashWidth, 1, XmNsashHeight, 1,	// Should be 0
		NULL);

    _workArea = createWorkArea(pane);
    XtVaSetValues(_workArea, XmNallowResize, True, NULL);

    _actionArea = createActionArea(pane);

    // Prevent the action area from resizing by turning off allowResize
    // and setting paneMin and paneMax the same (set them to the action
    // area's preferred height)

    XtVaSetValues(_actionArea, XmNallowResize, True,
		XmNpaneMaximum, _buttonHeight,
		XmNpaneMinimum, _buttonHeight, NULL);

    XtManageChild(_workArea);
    XtManageChild(_actionArea);
    XtManageChild(pane);

    return pane;
}

//////////////////////////////////////////////////////////
// Create the action area (where all the buttons are).  These buttons
// aren't CmdInterface's because not everything we want to do with
// them is a Command.  Plus, we don't want to undo Cancel or Help so
// there's no real point.
//
// One major bit of ugliness is the height stuff.  We get the preferred
// height of the first button created and save it in _buttonHeight.
// Then we add 2 * the margin height of this form.  All this so we can tell
// the PanedWindow above not to resize the action area (by setting the
// min and max the same).  Yuck.
//////////////////////////////////////////////////////////

Widget CustomDialog::createActionArea( Widget parent )
{
    Widget form;

    form = XtVaCreateWidget("action_area", xmRowColumnWidgetClass, parent,
	XmNorientation, XmHORIZONTAL, XmNpacking, XmPACK_COLUMN,
	XmNentryAlignment, XmALIGNMENT_CENTER,
	NULL);

    _buttonNum = 0;
    _buttonHeight = 0;
    _ok = makeOneButton(form, "OK", _showOk,
						&CustomDialog::okCallback);
    _apply = makeOneButton(form, "Apply", _showApply,
						&CustomDialog::applyCallback);
    _reset = makeOneButton(form, "Reset", _showReset,
						&CustomDialog::resetCallback);
    _cancel = makeOneButton(form, "Cancel", _showCancel,
						&CustomDialog::cancelCallback);
    _help = makeOneButton(form, "Help", _showHelp,
						&CustomDialog::helpCallback);

    Dimension h;
    XtVaGetValues(form, XmNmarginHeight, &h, NULL);
    _buttonHeight += 2 * h;

    return form;
}

//////////////////////////////////////////////////////////
// Create one button for action area
//////////////////////////////////////////////////////////

Widget CustomDialog::makeOneButton(Widget parent, const char *name,
			ButtonState state, XtCallbackProc proc)
{
    if (state == Invisible)
        return NULL;			// Do nothing

    Widget w = XtVaCreateManagedWidget(name, xmPushButtonWidgetClass, parent,
		XmNshowAsDefault, state==Default,
		XmNdefaultButtonShadowThickness, 1,
		XmNalignment, XmALIGNMENT_CENTER,
		NULL);
    if (_buttonNum == 0)	// Get height from first one
        XtVaGetValues(w, XmNheight, &_buttonHeight, NULL);
    _buttonNum++;
    if (proc)
        XtAddCallback(w, XmNactivateCallback, proc, (XtPointer) this);

    return w;
}

//////////////////////////////////////////////////////////
// Post the dialog.  It is created if needed.
//////////////////////////////////////////////////////////

void CustomDialog::post()
{
    if (!_w) {
        _w = createDialog(theApplication->baseWidget());
        XtManageChild(_w);
        installDestroyHandler();
    }

    // Call manage() function to do the popup and raise if iconified

    manage();

    _posted = True;
}

//////////////////////////////////////////////////////////
// Unpost the dialog.  Nothing else is done (i.e. no Cancel semantics)
//////////////////////////////////////////////////////////

void CustomDialog::unpost()
{
    if (_w)
        XtPopdown(_w);
    _posted = False;
}

//////////////////////////////////////////////////////////
// Returns True if window is posted, False otherwise
// This may need to be more sophisticated; for example, monitoring
// the popdown_callback list in case the window is iconified
// or the Close box is hit.
//////////////////////////////////////////////////////////

Boolean CustomDialog::isPosted()
{
    return _posted;
}

//////////////////////////////////////////////////////////
// Callbacks for the buttons
//////////////////////////////////////////////////////////

void CustomDialog::okCallback ( Widget, 
				XtPointer clientData,
				XtPointer )
{
    CustomDialog *obj = (CustomDialog *)clientData;

    // Apply then Unpost

    obj->apply();
    obj->unpost();
}

void CustomDialog::applyCallback ( Widget, 
				XtPointer clientData,
				XtPointer )
{
    CustomDialog *obj = (CustomDialog *)clientData;

    // Apply

    obj->apply();
}

void CustomDialog::resetCallback ( Widget, 
				XtPointer clientData,
				XtPointer )
{
    CustomDialog *obj = (CustomDialog *)clientData;

    // Reset

    obj->reset();
}

void CustomDialog::cancelCallback ( Widget, 
				XtPointer clientData,
				XtPointer )
{
    CustomDialog *obj = (CustomDialog *)clientData;

    // Reset then unpost

    obj->reset();
    obj->unpost();
}

void CustomDialog::helpCallback ( Widget w, 
				XtPointer clientData,
				XtPointer callData)
{
    CustomDialog *obj = (CustomDialog *)clientData;

    // Let the subclass implement help as it wishes

    obj->help(w, callData);
}

//////////////////////////////////////////////////////////
// Apply by executing then clearing the command list
//////////////////////////////////////////////////////////

void CustomDialog::apply()
{
    if (_applyCmdList) {
        _applyCmdList->execute();
        _applyCmdList->clear();
    }
}

//////////////////////////////////////////////////////////
// Reset by clearing the list
//////////////////////////////////////////////////////////

void CustomDialog::reset()
{
    if (_applyCmdList) {
        _applyCmdList->reset();
        _applyCmdList->clear();
    }
}

