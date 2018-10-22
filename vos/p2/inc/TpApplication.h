///////////////////////////////////////////////////////////////////////////////
// TpApplication.h: Special application class for tp application.
// It allows user to issue commands of type 
// % tp -images image1, image2, ..., imageN -pfile point_filename 
// -config config_file -v
//
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPAPPLICATION_H
#define TPAPPLICATION_H
#include "Application.h"

#define XtpNpfile "pfile"
#define XtpCPfile "Pfile"

#define XtpNconfig "config"
#define XtpCConfig "Config"

#define XtpNverbose "verbose"
#define XtpCVerbose "Verbose"

class TpApplication : public Application {

  private:

    static XrmOptionDescRec _options[3];
    static XtResource _resources[];

  protected:

    // Functions to handle Xt interface

    virtual void initialize_hook ();  
    
    char *_pfile;
    char *_config;
    Boolean _verbose;
    int _exitStatus;

    // Allow subclasses to use command-line arguments for resources
    // If the array is dynamically allocated, the destructor can free it

    virtual XrmOptionDescList getOptions() { return _options; }
    virtual Cardinal getNumOptions() { return XtNumber(_options); }

  public:
    
    TpApplication ( const char *appClassName );
    virtual ~TpApplication();     

    char *getPfile() { return _pfile; }
    char *getConfig() { return _config; }
    Boolean getVerbose() { return _verbose; }
    void setExitStatus(int status) { _exitStatus = status; }
    int getExitStatus() { return _exitStatus; }

    virtual const char *const className() { return "TpApplication"; }
};
#endif

// Create pointer to single global instance of TpApplication class

extern TpApplication *theTpApplication;
