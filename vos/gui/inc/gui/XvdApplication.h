////////////////////////////////////////////////////////////
// XvdApplication.h: 
////////////////////////////////////////////////////////////
#ifndef XVDAPPLICATION_H
#define XVDAPPLICATION_H

#ifdef ENABLE_SAGE
#include "SpbApplication.h"
class XvdApplication : public SpbApplication {
#else
#include "Application.h"
class XvdApplication : public Application {
#endif

  private:

#ifdef ENABLE_SAGE
    static XrmOptionDescRec _options[12]; 
#else
    static XrmOptionDescRec _options[9];
#endif

  protected:

    // Allow subclasses to use command-line arguments for resources
    // If the array is dynamically allocated, the destructor can free it

    virtual XrmOptionDescList getOptions() { return _options; }
    virtual Cardinal getNumOptions() { return XtNumber(_options); }

  public:
    
    XvdApplication ( const char *appClassName );
    virtual ~XvdApplication();     

    virtual const char *const className() { return "XvdApplication"; }
};
#endif

// Create pointer to single global instance of XvdApplication class

extern XvdApplication *theXvdApplication;
