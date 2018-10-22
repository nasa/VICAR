///////////////////////////////////////////////////////////////
// ImageWindowBlink.cc
///////////////////////////////////////////////////////////////

#ifdef ENABLE_SAGE
#include "SageApplication.h"
#else
#include "Application.h"
#endif
#include "ImageWindowBlink.h"
#include "BlinkControl.h"

#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <assert.h>

Widget ImageWindowBlink::_shell = 0;
Widget ImageWindowBlink::_blinker = 0;

///////////////////////////////////////////////////////////////
// Override initialize so we don't create a new shell all the time.
// This is a horribly ugly hack.  Because MainWindow::initialize
// explicitly creates a shell (which would be impossible to override),
// we take over the work here.  So much of this routine is a copy
// of MainWindow::initialize.  Unfortunately, we also have to copy
// MenuWindow::initialize and do *its* work here.  Yuck.
///////////////////////////////////////////////////////////////

void ImageWindowBlink::initialize()
{
    if (_shell == NULL) {
	_shell = XtVaCreatePopupShell("blinker_shell",
				topLevelShellWidgetClass,
				theApplication->baseWidget(),
				XmNallowShellResize, True,
				NULL);
	_blinker = XtVaCreateManagedWidget("blinker",
				xmFormWidgetClass,
				_shell,
				NULL);
    }

    _w = XtVaCreateManagedWidget(_name,
				xmFrameWidgetClass,
				_blinker,
				XmNleftAttachment, XmATTACH_FORM,
				XmNrightAttachment, XmATTACH_FORM,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				NULL);

// Now do what MainWindow does (except for the BlinkControl::add call)

    installDestroyHandler();

    // Use a Motif XmMainWindow widget to handle window layout

    _main = XtCreateManagedWidget("mainWindow",
		xmMainWindowWidgetClass,
		_w,
		NULL, 0);

    // Call derived class to create the work area

    _workArea = createWorkArea(_main);
    assert(_workArea != NULL);

// Add this in to the BlinkControl.  We do it after createWorkArea so
// BlinkControl can get at models and such.

    theBlinkControl->addBlinkWindow(this);

// Back to MainWindow code...

    // Designate the _workArea widget as the XmMainWindow
    // widget's XmNworkWindow widget

    XtVaSetValues(_main, XmNworkWindow, _workArea, NULL);

    // Manage the work area if the derived class hasn't already

    if (!XtIsManaged(_workArea))
	XtManageChild(_workArea);

// Now do what MenuWindow does

    // Specify the base widget of a MenuBar object
    // the XmMainWindow widget's menu bar.

    _menuBar = new MenuBar(_main, "menubar");

    XtVaSetValues(_main, XmNmenuBar, _menuBar->baseWidget(), NULL);

    // Call derived class hook to add panes to the menu

    createMenuPanes();

    _menuBar->manage();
}

// Copied from MainWindow.  Needed only to use _shell instead of _w for
// XtPopup/XtPopdown.

void ImageWindowBlink::manage()
{
    assert ( _w != NULL);
    XtPopup ( _shell, XtGrabNone );

    // Map the window, in case the window is iconified

    if ( XtIsRealized ( _w ) )
        XMapRaised ( XtDisplay ( _w ), XtWindow ( _w ) );
}

void ImageWindowBlink::unmanage()
{
    assert ( _w != NULL);
    XtPopdown ( _shell );
}

void ImageWindowBlink::iconify()
{
    assert ( _w != NULL);

    // Set the widget to have an initial iconic state
    // in case the base widget has not yet been realized

    XtVaSetValues ( _shell, XmNiconic, TRUE, NULL );

    // If the widget has already been realized,
    // iconify the window

    if ( XtIsRealized ( _w ) )
        XIconifyWindow ( XtDisplay ( _w ), XtWindow ( _w ), 0 );
}

void ImageWindowBlink::deiconify()
{
    if ((_w == NULL) || !XtIsRealized(_w))
        return;

    // Map the window, in case the window is iconified
    // Window state can be 0=withdrawn, 1=normal, 3=iconified
    // if it's anything but 0, popup and raise

    if ((getWindowIconState () != WithdrawnState)) {
        XtPopup ( _shell, XtGrabNone );
        XMapRaised ( XtDisplay ( _w ), XtWindow ( _w ) );
    }
}


