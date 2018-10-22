//////////////////////////////////////////////////////////////
// OptionInterface.cc: A push button interface to a Cmd object intended
// for use within an Option menu.  Note that the widget ID of the
// option menu itself must be provided so that setValue can tell
// the option menu what the new setting is (there appears to be no
// way to get the option menu given one of the pushbuttons!).
///////////////////////////////////////////////////////////////
#include "OptionInterface.h"
#include "Cmd.h"
#include <Xm/PushB.h>

OptionInterface::OptionInterface ( Widget parent, Cmd *cmd, Widget option )
		: CmdInterface ( cmd )
{
    doConstruct(parent);
    _option = option;
    setValue(cmd->getValue());
}

OptionInterface::OptionInterface ( Widget parent, const char *name,
							Widget option )
		: CmdInterface ( name )
{
    doConstruct(parent);
    _option = option;
}

void OptionInterface::doConstruct ( Widget parent )
{
    _w = XtCreateWidget ( _name, 
			 xmPushButtonWidgetClass,
			 parent,
			 NULL, 0 );
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
		   XmNactivateCallback, 
		   &CmdInterface::executeCmdCallback,
		   (XtPointer) this );  
#endif
    
}

// Make sure we execute the command with a True value (or it'll be
// ignored by the RadioCmd).

void OptionInterface::executeCmd ( XtPointer )
{
    runCmd((CmdValue) True);
}

// If our value is True, tell the option menu to show us

void OptionInterface::setValue ( CmdValue value )
{
    if (!value)
        return;			// nothing to do

    if (_option && _w)
        XtVaSetValues(_option, XmNmenuHistory, _w, NULL);

    CmdInterface::setValue(value);
}

