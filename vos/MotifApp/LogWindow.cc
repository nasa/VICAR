//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
#include "LogWindow.h"
#include "Application.h"
#include <Xm/Text.h>

String LogWindow::_defaults[] = {
    (char *)"*errorLog.cursorPositionVisible: 	False",
    (char *)"*errorLog.editable: 		False",
    (char *)"*errorLog.editMode: 		XmMULTI_LINE_EDIT",
    (char *)"*errorLog.columns: 		80",
    (char *)"*errorLog.rows: 			15",
    (char *)"*x: 				0",
    (char *)"*y: 				750",
    NULL,
};

LogWindow::LogWindow(const char *name)
	: MainWindow(name)
{
    _posted = False;
}

//////////////////////////////////////////////////////////

LogWindow::~LogWindow()
{
    // Empty
}

//////////////////////////////////////////////////////////
// Create the dialog shell and the standard parts.  This should not normally
// be overridden by subclasses.
//////////////////////////////////////////////////////////
Widget LogWindow::createWorkArea(Widget parent)
{
    // Set default shell and widget resources

    setDefaultResources(parent, _defaults);

    Widget shell;

    // Create the shell widget.  Unmap, don't destroy, when the user closes
    // the window so that we don't have to recreate the thing later.

    shell = XtVaCreatePopupShell(_name, 
		topLevelShellWidgetClass, parent,
                XmNdeleteResponse, XmUNMAP,
                NULL);

    _textW = XmCreateScrolledText(shell, (char *)"errorLog", NULL, 0 );

    XtManageChild(_textW);

    return shell;
}

//////////////////////////////////////////////////////////
// Post the dialog.  It is created if needed.
//////////////////////////////////////////////////////////

void LogWindow::post(const char *text)
{
    if (!_w) {
        _w = createWorkArea(theApplication->baseWidget());
        XtManageChild(_w);
        installDestroyHandler();
    }

    // Check if some text already exists

    char *oldText = XmTextGetString(_textW);

    if(oldText && *oldText)	// Insert at the end of the old text
	XmTextInsert(_textW, strlen(oldText), (char *)text);
    else 			// Insert at the beginning
	XmTextInsert(_textW, 0, (char *)text);

    XtFree(oldText);

    // Call manage() function to do the popup and raise if iconified

    manage();

    _posted = True;
}

//////////////////////////////////////////////////////////
// Unpost the dialog.  Nothing else is done (i.e. no Cancel semantics)
//////////////////////////////////////////////////////////

void LogWindow::unpost()
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

Boolean LogWindow::isPosted()
{
    return _posted;
}

