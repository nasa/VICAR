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


////////////////////////////////////////////////////////////
// Application.C: 
////////////////////////////////////////////////////////////
#include "Application.h"
#include "MainWindow.h"
#include <assert.h>
#include <stdlib.h>
#if XmVERSION != 1 || XmREVISION > 1
#include <Xm/RepType.h>
#endif

Application *theApplication = NULL;

Application::Application ( const char *appClassName ) : 
                    UIComponent ( appClassName )
{
    // Set the global Application pointer
    
    theApplication = this;  
    
    // Initialize data members
    
    _display    = NULL;
    _appContext = NULL;
    _windows    = NULL;
    _numWindows = 0;
    _applicationClass = strdup ( appClassName );
}

#if (XlibSpecificationRelease>=5)
void Application::initialize ( int *argcp, char **argv )
#else
void Application::initialize ( unsigned int *argcp, char **argv )
#endif
{
    initialize_X(argcp, argv);
    
    // Install the TearOffMenu type converter, if we're Motif 1.2 or later...
#if XmVERSION != 1 || XmREVISION > 1
    XmRepTypeInstallTearOffModelConverter();
#endif

    // Extract and save a pointer to the X display structure
    
    _display = XtDisplay ( _w );
    
    // The Application class is less likely to need to handle
    // "surprise" widget destruction than other classes, but
    // we might as well install a callback to be safe and consistent
    
    installDestroyHandler();
    
    // Center the shell, and make sure it isn't visible
    
    XtVaSetValues ( _w,
		   XmNmappedWhenManaged, FALSE,
		   XmNx, DisplayWidth ( _display, 0 ) / 2,
		   XmNy, DisplayHeight ( _display, 0 ) / 2,
		   XmNwidth,  1,
		   XmNheight, 1,
		   NULL );
    
    // The instance name of this object was set in the UIComponent 
    // constructor, before the name of the program was available
    // Free the old name and reset it to argv[0]
    
    delete _name;
    _name = strdup ( argv[0] );
    _argc = (int *)argcp;	// Save these away in case the app needs them
    _argv = argv;
    
    // Force the shell window to exist so dialogs popped up from
    // this shell behave correctly
    
    XtRealizeWidget ( _w );
    
    // Allow subclasses to do their own initialization before the MainWindows
    // are initialized.

    initialize_hook();

    // Initialize and manage any windows registered
    // with this application.  Copy the list first so the windows we
    // create can create their own MainWindow's without getting them
    // automatically initialized/managed here.
    
    MainWindow **saveWindows = new MainWindow*[_numWindows];
    int n = _numWindows;
    int i;
    for ( i = 0; i < n; i++ )
        saveWindows[i] = _windows[i];

    for ( i = 0; i < n; i++ )
    {
	_windows[i]->initialize();
        _windows[i]->setExitOnDestroy();
	_windows[i]->manage();
    }
    delete []saveWindows;
}

// Let subclasses do their own X initialization (e.g. to use a specific display)
// Note that _w and _appContext must be set by this routine.

#if (XlibSpecificationRelease>=5)
void Application::initialize_X ( int *argcp, char **argv )
#else
void Application::initialize_X ( unsigned int *argcp, char **argv )
#endif
{
    _w = XtAppInitialize ( &_appContext, 
			  _applicationClass, getOptions(), getNumOptions(),
			  argcp, argv, 
			  NULL, NULL, 0 );
}

Application::~Application()
{
    delete _applicationClass;
    delete _windows;
}

void Application::handleEvents()
{
    // Just loop forever
    
    XtAppMainLoop ( _appContext );
}

void Application::registerWindow ( MainWindow *window )
{
    int i;
    MainWindow **newList;
    
    // Allocate a new list large enough to hold the new
    // object, and copy the contents of the current list 
    // to the new list
    
    newList = new MainWindow*[_numWindows + 1];
    
    for ( i = 0; i < _numWindows; i++ )
	newList[i] = _windows[i];
    
    // Install the new list and add the window to the list
    
    delete []_windows;
    _windows =  newList;
    _windows[_numWindows] = window;
    
    _numWindows++;
}

void Application::unregisterWindow ( MainWindow *window )
{
    int i, index;
    MainWindow **newList;
    
    // Allocate a new, smaller list
    
    newList = new MainWindow*[_numWindows - 1];
    
    // Copy all objects, except the one to be 
    // removed, to the new list
    
    index = 0;
    for ( i = 0; i < _numWindows; i++ )
	if ( _windows[i] != window )
	    newList[index++] = _windows[i];
    
    // Install the new list
    
    delete []_windows;
    _windows =  newList;
    
    _numWindows--;
}

void Application::manage()
{
    // Manage all application windows. This will pop up
    // iconified windows as well.
    
    for ( int i = 0; i < _numWindows; i++ )
	_windows[i]->manage();
}

void Application::unmanage()
{
    // Unmanage all application windows
    
    for ( int i = 0; i < _numWindows; i++ )
	_windows[i]->unmanage();
}

void Application::iconify()
{
    // Iconify all top-level windows.
    
    for ( int i = 0; i < _numWindows; i++ )
	_windows[i]->iconify();
}

void Application::deiconify()
{
    // Manages all application windows. This will deiconify 
    // iconified windows.
    
    for ( int i = 0; i < _numWindows; i++ )
        _windows[i]->deiconify();
}

void Application::setBusyCursor()
{
    // set busy cursor in app window
    
    for ( int i = 0; i <  _numWindows; i++ )
       _windows[i]->setBusyCursor();
}

void Application::removeBusyCursor()
{
    // set cursor back to normal
   
    for ( int i = 0; i <  _numWindows; i++ )
       _windows[i]->removeBusyCursor();
}

