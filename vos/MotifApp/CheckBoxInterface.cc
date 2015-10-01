//////////////////////////////////////////////////////////////
// CheckBoxInterface.cc: A check box (n of many) interface to a Cmd object
// The value is either TRUE or FALSE
///////////////////////////////////////////////////////////////
#include "CheckBoxInterface.h"
#include <Xm/ToggleB.h>
#include <stdint.h>

CheckBoxInterface::CheckBoxInterface ( Widget parent, Cmd *cmd )
		: CmdInterface ( cmd )
{
    doConstruct(parent);
    setValue(cmd->getValue());
}

CheckBoxInterface::CheckBoxInterface ( Widget parent, const char *name )
		: CmdInterface ( name )
{
    doConstruct(parent);
}

void CheckBoxInterface::doConstruct( Widget parent )
{
    _w = XtVaCreateWidget ( _name, xmToggleButtonWidgetClass, parent,
		XmNindicatorType, XmN_OF_MANY,
		XmNset, False,
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
    
    XtAddCallback ( _w,  
		   XmNactivateCallback, 
		   executeCmdCallback,
		   (XtPointer) this );  
#else

    XtAddCallback ( _w,  
		   XmNvalueChangedCallback, 
		   &CmdInterface::executeCmdCallback,
		   (XtPointer) this );  
#endif
}

void CheckBoxInterface::executeCmd ( XtPointer )
{
    Boolean value;

    value = XmToggleButtonGetState(_w);

    runCmd((CmdValue) (int)value);
}

void CheckBoxInterface::setValue ( CmdValue value )
{
    int onoff;

    if (value)		// Make sure value is 0 or 1
        onoff = True;
    else
        onoff = False;
    if (_w)
        XmToggleButtonSetState (_w, onoff, False);
    CmdInterface::setValue(value);
}

