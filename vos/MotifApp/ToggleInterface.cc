//////////////////////////////////////////////////////////////
// ToggleInterface.C: A toggle button interface to a Cmd object
///////////////////////////////////////////////////////////////
#include "ToggleInterface.h"
#include <Xm/ToggleB.h>

ToggleInterface::ToggleInterface ( Widget parent, Cmd *cmd, int on )
		: CmdInterface ( cmd )
{
    _w = XtVaCreateWidget ( _name, xmToggleButtonWidgetClass, parent,
		XmNindicatorType, XmONE_OF_MANY,
		XmNset, on,
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

// executeCmd() is overridden here because we only want to call the command
// if the value is set.  If it is not set, don't call the associated Cmd.

void ToggleInterface::executeCmd ( XtPointer )
{
    if (XmToggleButtonGetState(_w))
        runCmd();
}

void ToggleInterface::setDefault(Boolean onoff)
{
   XtVaSetValues(_w, XmNset, onoff, NULL);
}

