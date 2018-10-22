////////////////////////////////////////////////////////////////////////
// XvdApplication.cc: 
////////////////////////////////////////////////////////////////////////
#include "XvdApplication.h"

XvdApplication *theXvdApplication = NULL;

// The subclass should contain all of these options.
// If you are updating record, always make sure 
// your changes are reflected in all the subclasses

XrmOptionDescRec  XvdApplication::_options[] = {
#ifdef ENABLE_SAGE
 {"-host",       "*host",                    XrmoptionSepArg,  NULL  },
 {"-port",       "*port",                    XrmoptionSepArg,  NULL  },
 {"-module",     "*module",                  XrmoptionSepArg,  NULL  },
#endif
 {(char *)"-min",       (char *)"*min",              XrmoptionSepArg,  NULL  },
 {(char *)"-max",       (char *)"*max",              XrmoptionSepArg,  NULL  },
 {(char *)"-fullscreen",(char *)"*fullScreenDisplay",XrmoptionNoArg,(char*)"True"},
 {(char *)"-fit",       (char *)"*fit",           XrmoptionNoArg,(char*)"True"},
 {(char *)"-height",    (char *)"*XVd.height",       XrmoptionSepArg,  NULL  },
 {(char *)"-width",     (char *)"*XVd.width",        XrmoptionSepArg,  NULL  },
 {(char *)"-x",         (char *)"*XVd.x",            XrmoptionSepArg,  NULL  },
 {(char *)"-y",         (char *)"*XVd.y",            XrmoptionSepArg,  NULL  },
 {(char *)"-help",      (char *)"*cmdLineHelpReqd", XrmoptionNoArg,(char*)"True"}
};

XvdApplication::XvdApplication ( const char *appClassName ) : 
#ifdef ENABLE_SAGE
                                         SpbApplication ( appClassName )
#else
                                         Application ( appClassName )
#endif
{
    // Set the global Application pointer

    theXvdApplication = this;
}

XvdApplication::~XvdApplication()
{
}
