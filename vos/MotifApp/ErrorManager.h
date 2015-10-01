//////////////////////////////////////////////////////////
// ErrorManager.h: Error manager base class.  It interprets 
// the error and produces the appropriate action based on the 
// action variable.
//////////////////////////////////////////////////////////
#ifndef ERRORMANAGER_H
#define ERRORMANAGER_H

#include "ErrorData.h"
#include <X11/Intrinsic.h>	// For definition of Boolean

class LogWindow;

class ErrorManager {

  protected:

    static void okCallback(XtPointer);

    char *_name;
    LogWindow *_logWindow;

    virtual void atExit();
    virtual LogWindow *createLogWindow();

  public:

    ErrorManager(const char *);
    virtual ~ErrorManager();

    virtual Boolean process(ErrorData *error);
    virtual Boolean process(ActionOnError, const char *, const char *,
						const char * = NULL);

};

extern ErrorManager *theErrorManager;

#endif
