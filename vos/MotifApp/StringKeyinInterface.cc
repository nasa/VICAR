////////////////////////////////////////////////////////////////        
// StringKeyinInterface.cc: A TextField interface to a command that takes
// a dynamically allocated string (freed via XtFree()) as its value.
////////////////////////////////////////////////////////////////

#include "StringKeyinInterface.h"
#include <Xm/TextF.h>

StringKeyinInterface::StringKeyinInterface(Widget parent, Cmd *cmd)
	: CmdInterface(cmd)
{
    _oldValue = NULL;

    _w = XtVaCreateWidget(_name, xmTextFieldWidgetClass, parent,
		NULL);
    installDestroyHandler();

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
    
    if ( _active )
	activate();     
    else
	deactivate();   

// No idea what the right ifdef is for automatically recognizing g++
#ifdef GNU_CC
    
    // G++ reportedly doesn't like the form expected by cfront. I'm
    // told this will work, but I haven't tested it myself.
    
    XtAddCallback(_w, XmNlosingFocusCallback, executeCmdCallback,
		(XtPointer)this);
    XtAddCallback(_w, XmNactivateCallback, executeCmdCallback,
		(XtPointer)this);
#else

    XtAddCallback(_w, XmNlosingFocusCallback, &CmdInterface::executeCmdCallback,
		(XtPointer)this);
    XtAddCallback(_w, XmNactivateCallback, &CmdInterface::executeCmdCallback,
		(XtPointer)this);
#endif

}

////////////////////////////////////////////////////////////////

void StringKeyinInterface::executeCmd(XtPointer)
{
    char *string;

    string = XmTextFieldGetString(_w);

    if (_oldValue && strcmp(string, _oldValue) == 0) {
        // Duplicate value, so don't execute the command again
        XtFree(string);
    }
    else {
        copyOldValue(string);
        runCmd((CmdValue) string);
    }
}

////////////////////////////////////////////////////////////////

void StringKeyinInterface::setValue(CmdValue value)
{
    CmdInterface::setValue(value);	// Removes cmd from deferred list

    XmTextFieldSetString(_w, (char *)value);
    copyOldValue((char *)value);
}

///////////////////////////////////////////////////////////////////
//  Allow execution of command after value is set.
///////////////////////////////////////////////////////////////////
void StringKeyinInterface::setValueAndRun(CmdValue value)
{
    CmdInterface::setValue(value);	// Removes cmd from deferred list

    XmTextFieldSetString(_w, (char *)value);
    executeCmd();
}

////////////////////////////////////////////////////////////////

void StringKeyinInterface::copyOldValue(char *string)
{
    if (_oldValue)
        delete _oldValue;
    if (string)
        _oldValue = strdup(string);
    else
        _oldValue = NULL;
}

