//////////////////////////////////////////////////////////
// ErrorManager.cc: Error manager base class.  It interprets
// the error and produces the appropriate action based on the
// action variable.
//////////////////////////////////////////////////////////
#include "ErrorManager.h"
#include "ErrorData.h"
#include "LogWindow.h"
#include "ErrorDialogManager.h"
#include "BasicComponent.h"		// for strdup() on VMS
#include "Application.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

ErrorManager *theErrorManager = NULL;

ErrorManager::ErrorManager(const char *name)
{
    // Set the global ErrorManager pointer

    theErrorManager = this;

    _name = NULL;
    assert(name != NULL);
    _name = strdup(name);

    _logWindow = NULL;
}

ErrorManager::~ErrorManager()
{
    delete [] _name;
}

LogWindow *ErrorManager::createLogWindow()
{
    return (new LogWindow(_name));
}

// Return true on success, false otherwise
Boolean ErrorManager::process(ErrorData *error)
{
    char *msg = error->getErrorMsg();
    if (!msg || (strlen(msg) == 0)) return FALSE;

    if (theApplication == NULL || theApplication->display() == NULL)
	fprintf(stderr, "%s", msg);

    else {

	switch(error->getAction()) {
	    case Ignore:
		break;
	    case Warning:
		if (!_logWindow)
		    _logWindow = createLogWindow();
		_logWindow->post(msg);
		break;
	    case Error:
		theErrorDialogManager->post(msg);
		break;
	    case Fatal:
		theErrorDialogManager->post(msg, (void *)this, 
				&ErrorManager::okCallback);
		break;
	    default:
		fprintf(stderr, "%s", msg);
		return FALSE;
	}
    }

    delete [] msg;

    return TRUE;
}

Boolean ErrorManager::process(ActionOnError action, const char *preText, 
			      const char *text, const char *details)
{
    ErrorData *error = new ErrorData(action, preText, text, details);
    Boolean v = process(error);
    delete error;
    return v;
}

void ErrorManager::okCallback(XtPointer cd)
{
    ErrorManager *obj = (ErrorManager *) cd;
    // Execute exit routine

    obj->atExit();
}

void ErrorManager::atExit()
{
    exit (-1);
}

