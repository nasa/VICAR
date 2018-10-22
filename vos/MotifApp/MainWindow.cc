///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//         This example code is from the book:
//
//           Object-Oriented Programming with C++ and OSF/Motif
//         by
//           Douglas Young
//           Prentice Hall, 1992
//           ISBN 0-13-630252-1	
//
//         Copyright 1991 by Prentice Hall
//         All Rights Reserved
//
//  Permission to use, copy, modify, and distribute this software for 
//  any purpose except publication and without fee is hereby granted, provided 
//  that the above copyright notice appear in all copies of the software.
///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////
// MainWindow.C: Support a toplevel window
////////////////////////////////////////////////////////////////////
#include "Application.h"
#include "MainWindow.h"
#include "Cmd.h"
#include <Xm/MainW.h>
#include <assert.h>
#include <stdlib.h>
#include <X11/cursorfont.h>
#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>

MainWindow::MainWindow ( const char *name ) : UIComponent ( name )
{
    _workArea = NULL;
    _exitOnDestroy = False;
    _cmdOnClose = NULL;
    _busyCursor = None;
    _inputWindow = None;
    assert ( theApplication != NULL ); // Application object must exist
    // before any MainWindow object
    theApplication->registerWindow ( this );
}

void MainWindow::initialize( )
{
    // All toplevel windows in the MotifApp framework are 
    // implemented as a popup shell off the Application's
    // base widget.
    
    _w = XtVaCreatePopupShell ( _name, 
			     topLevelShellWidgetClass,
			     theApplication->baseWidget(),
			     XmNallowShellResize, True,
			     NULL );
    installDestroyHandler();
    
    // Use a Motif XmMainWindow widget to handle window layout
    
    _main = XtCreateManagedWidget ( "mainWindow", 
				   xmMainWindowWidgetClass,
				   _w, 
				   NULL, 0 );
    
    // Called derived class to create the work area
    
    _workArea = createWorkArea ( _main );  
    assert ( _workArea != NULL );
    
    // Designate the _workArea widget as the XmMainWindow
    // widget's XmNworkWindow widget
    
    XtVaSetValues ( _main, 
		   XmNworkWindow, _workArea, 
		   NULL );
    
    // Manage the work area if the derived class hasn't already.
    
    if ( !XtIsManaged ( _workArea ) )
	XtManageChild ( _workArea ); 
}

MainWindow::~MainWindow( )
{
    // Unregister this window with the Application object
    
    theApplication->unregisterWindow ( this );
}

void MainWindow::manage()
{
    assert ( _w != NULL);
    XtPopup ( _w, XtGrabNone );
    
    // Map the window, in case the window is iconified
    
    if ( XtIsRealized ( _w ) )
	XMapRaised ( XtDisplay ( _w ), XtWindow ( _w ) );
}

void MainWindow::unmanage()
{
    assert ( _w != NULL);
    XtPopdown ( _w );
}

void MainWindow::iconify()
{
    assert ( _w != NULL);
    
    // Set the widget to have an initial iconic state
    // in case the base widget has not yet been realized
    
    XtVaSetValues ( _w, XmNiconic, TRUE, NULL );
    
    // If the widget has already been realized,
    // iconify the window
    
    if ( XtIsRealized ( _w ) )
	XIconifyWindow ( XtDisplay ( _w ), XtWindow ( _w ), 0 );
}

// Return the normal/iconic/withdrawn state of a window.  Why doesn't
// X provide this function??

int MainWindow::getWindowIconState ()
{
    Atom atom;
    Atom actual_type_return;
    int actual_format_return;
    unsigned long nitems_returned;
    unsigned long bytes_after_return;
    int *window_state;
    int return_val;
    int state;

    atom = XInternAtom (XtDisplay (_w), "WM_STATE", False);

    return_val  = XGetWindowProperty (XtDisplay (_w), XtWindow (_w), 
                        atom, 0, 1, False, atom, 
                        &actual_type_return, &actual_format_return,
                        &nitems_returned, &bytes_after_return, 
                        (unsigned char **) &window_state); 

    if (return_val == Success) {
        state = *window_state;
        XFree ((void *) window_state);
    }
    else
        state = NormalState;  // just figure it's normal

    return state;
}

void MainWindow::deiconify()
{
    if ((_w == NULL) || !XtIsRealized(_w))
	return;

    // Map the window, in case the window is iconified
    // Window state can be 0=withdrawn, 1=normal, 3=iconified
    // if it's anything but 0, popup and raise 

    if ((getWindowIconState () != WithdrawnState)) {
        XtPopup ( _w, XtGrabNone );
	XMapRaised ( XtDisplay ( _w ), XtWindow ( _w ) );
    }
}


void MainWindow::widgetDestroyed()
{
    UIComponent::widgetDestroyed();

    // Motif does this automatically when the close box is hit and
    // XmNdeleteResponse is XmDESTROY and it's an application shell.
    // Since this is a topLevel shell instead, we must do it here.

    if (_exitOnDestroy) {
        XtDestroyApplicationContext(theApplication->appContext());
        exit(0);
    }
}


void MainWindow::setBusyCursor()
{ 
    // Create an input only window & change its cursor to
    // a busyCursor. Put on top of app window to prevent user input
   
    XSetWindowAttributes attrs;
    unsigned int width, height;
   
    if (!_busyCursor && (_w != NULL) && XtIsRealized(_w)) {
        Display *dpy = XtDisplay (_w);

        width = DisplayWidth(dpy, XScreenNumberOfScreen(XtScreen(_w)));
        height = DisplayHeight(dpy, XScreenNumberOfScreen(XtScreen(_w)));

        _busyCursor = XCreateFontCursor (dpy, XC_watch);
        attrs.cursor = _busyCursor;

        _inputWindow = XCreateWindow (dpy, XtWindow(_w), 0, 0, width, height,
          0, (int)CopyFromParent, InputOnly, CopyFromParent, CWCursor, &attrs);

        XMapRaised (dpy, _inputWindow);
        XFlush (dpy);
    }
}

void MainWindow::removeBusyCursor()
{
    // Remove the input only window with busy cursor 

    if (_busyCursor && _w && XtIsRealized(_w)) {
        if (_inputWindow)
            XDestroyWindow (XtDisplay (_w), _inputWindow);
        XFreeCursor(XtDisplay (_w), _busyCursor);
        _busyCursor = None;   
        _inputWindow = None;
    }
}

// Call this function if you would like to execute some
// command in case user chooses Close option on window 
// manager menu.

void MainWindow::trapDeleteWindowEvent(Cmd *cmdOnClose)
{
    _cmdOnClose = cmdOnClose;

    XtVaSetValues(_w, XmNdeleteResponse, XmDO_NOTHING, NULL);
    Atom WM_DELETE_WINDOW = XmInternAtom(XtDisplay(_w),
				(char *)"WM_DELETE_WINDOW", False);
    XmAddWMProtocolCallback(_w, WM_DELETE_WINDOW, 
				deleteWindowResponseCallback, this);
}

void MainWindow::deleteWindowResponseCallback(Widget, XtPointer clientData, 
				XtPointer)
{
    MainWindow *obj = (MainWindow *)clientData;
    obj->deleteWindowResponse();
}

void MainWindow::deleteWindowResponse()
{
    if (_cmdOnClose)
	_cmdOnClose->execute();
}
