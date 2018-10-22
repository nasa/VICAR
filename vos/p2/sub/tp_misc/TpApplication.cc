///////////////////////////////////////////////////////////////////////////////
// TpApplication.cc 
///////////////////////////////////////////////////////////////////////////////
#include "TpApplication.h"
#include "MainWindow.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>

TpApplication *theTpApplication = NULL;

// The subclass should contain all of these options.
// If you are updating record, always make sure 
// your changes are reflected in all the subclasses

XrmOptionDescRec  TpApplication::_options[] = {
 {(char *)"-pfile",	(char *)"*pfile", XrmoptionSepArg, NULL},
 {(char *)"-config",    (char *)"*config", XrmoptionSepArg, NULL},
 {(char *)"-v", 	(char *)"*verbose", XrmoptionSepArg, NULL},
};

XtResource TpApplication::_resources [] = {
  {
        (char *)XtpNpfile,
        (char *)XtpCPfile,
        XmRString,
        sizeof(String),
        XtOffset(TpApplication *, _pfile),
        XmRImmediate,
        (XtPointer) 0,
  },
  {
        (char *)XtpNconfig,
        (char *)XtpCConfig,
        XmRString,
        sizeof(String),
        XtOffset(TpApplication *, _config),
        XmRImmediate,
        (XtPointer) 0,
  },
  {
	(char *)XtpNverbose,
	(char *)XtpCVerbose,
	XmRBoolean,
	sizeof(Boolean),
	XtOffset(TpApplication *, _verbose),
	XmRImmediate,
	(XtPointer) False,
  },
};

TpApplication::TpApplication ( const char *appClassName ) 
	: Application ( appClassName )
{
    // Set the global Application pointer

    theTpApplication = this;

    // Initialize data members

    _pfile = NULL;
    _config = NULL;
    _verbose = False;
    _exitStatus = 0;
}

void TpApplication::initialize_hook()
{
    XtGetApplicationResources ( _w,
                (XtPointer) this,
                _resources,
                XtNumber (_resources),
                NULL, 0 );
}

TpApplication::~TpApplication()
{
    // Empty
}
