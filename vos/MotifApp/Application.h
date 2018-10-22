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
// Application.h: 
////////////////////////////////////////////////////////////
#ifndef APPLICATION_H
#define APPLICATION_H
#include "UIComponent.h"
class MainWindow;
class Application : public UIComponent {
    
    // Allow main and MainWindow to access protected member functions

#if (XlibSpecificationRelease>=5)
    friend int main ( int, char ** );
#else
    friend int main ( unsigned int, char ** );
#endif
    
    friend class MainWindow;  // MainWindow needs to call 

    // private functions for registration

  private:    
    
    // Functions for registering and unregistering toplevel windows
    
    void registerWindow ( MainWindow * );
    void unregisterWindow ( MainWindow * );
    
  protected:
    
    // Support commonly needed data structures as a convenience
    
    Display     *_display;
    XtAppContext _appContext;
    
    // Functions to handle Xt interface
#if (XlibSpecificationRelease>=5)   
    virtual void initialize ( int *, char ** );  
#else
    virtual void initialize ( unsigned int *, char ** );  
#endif
    virtual void handleEvents();
    
    char   *_applicationClass;    // Class name of this application
    MainWindow  **_windows;       // top-level windows in the program
    int           _numWindows;

    int    *_argc;
    char   **_argv;
    
    // Allow subclasses to use command-line arguments for resources
    // If the array is dynamically allocated, the destructor can free it

    virtual XrmOptionDescList getOptions() { return NULL; }
    virtual Cardinal getNumOptions() { return 0; }

    // Allow subclasses to do initialization before the MainWindows are init'ed

    virtual void initialize_hook() { }

    // Let subclasses do their own X initialization (e.g. to a specific display)

#if (XlibSpecificationRelease>=5)
    virtual void initialize_X ( int *argcp, char **argv );
#else
    virtual void initialize_X ( unsigned int *argcp, char **argv );
#endif

  public:
    
    Application ( const char * );
    virtual ~Application();     
    
    // Functions to manipulate application's top-level windows
    
    void manage();
    void unmanage();
    void iconify();
    void deiconify();
    void setBusyCursor();
    void removeBusyCursor();   
 
    // Convenient access functions
    
    Display      *display()     { return _display; }
    XtAppContext  appContext()  { return _appContext; }
    const char   *applicationClass()  { return _applicationClass; }
    
    // Functions to handle command line parameters

    int getArgc()             { return *_argc; }
    char *getParam ( int n )  { return _argv[n]; }

    virtual const char *const className() { return "Application"; }
};

// Pointer to single global instance

extern Application *theApplication; 

#endif
