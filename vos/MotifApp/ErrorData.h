//////////////////////////////////////////////////////////
// ErrorData.h: A unit indicating an error.  It represents 
// the error text, the criticality of the error, and the 
// source of the error.
//////////////////////////////////////////////////////////
#ifndef ERRORDATA_H
#define ERRORDATA_H

typedef enum _ActionOnError {
   Ignore = 0,	// Don't output.
   Debug,	// For debug prints.
   Warning,	// Dump to an error window.
   Error,	// Post an error dialog.
   Fatal	// Post an error dialog and quit application.
} ActionOnError;

class ErrorData {

  protected:

    ActionOnError _action;

    // Text can be in the form 
    // <preText>: <text> (<details>)
    // e.g.
    // myapp: Can't set specified icon (icon is not available)

    char *_preText;
    char *_text;
    char *_details;

  public:

    ErrorData(ActionOnError, const char *, const char *, const char *);
    ErrorData(const ErrorData &);
    ErrorData& operator=(const ErrorData &);
    virtual ~ErrorData();

    // This function allocates memory for the string.  
    // Caller should free the string with delete call.

    virtual char *getErrorMsg();

    ActionOnError getAction() { return _action; }
    char *getPreText() { return _preText; }
    char *getText() { return _text; }
    char *getDetails() { return _details; }

};

#endif
