/////////////////////////////////////////////////////////////////
// SetDeferredCmd.cc: Switches between immediate and 
// deferred execution modes.  This command in intended to
// work with CheckBox widget.  If the CheckBox's value is
// True then deferred execution is set, else immediate 
// execution mode is set
////////////////////////////////////////////////////////////////
#include "SetDeferredCmd.h"
#include "CmdInterface.h"


SetDeferredCmd::SetDeferredCmd(const char *name, int active, 
		CmdInterface *interface, CmdList *applyCmdList, 
		Widget *applyButton) 
	: Cmd(name, active)
{
    _interface = interface;
    _applyCmdList = applyCmdList;
    _applyButton = applyButton;
}

////////////////////////////////////////////////////////////////
// doit()
////////////////////////////////////////////////////////////////
void SetDeferredCmd::doit()
{
    if (_value) {
	_interface->setDeferredExec(_applyCmdList);
	if (*_applyButton)
            XtSetSensitive(*_applyButton, True);
    }
    else {
	_interface->setDeferredExec(NULL);
	if (*_applyButton)
            XtSetSensitive(*_applyButton, False);
    }
}

////////////////////////////////////////////////////////////////
// undoit()
////////////////////////////////////////////////////////////////
void SetDeferredCmd::undoit()
{
    if (_value) {
	_interface->setDeferredExec(NULL);
	if (*_applyButton)
            XtSetSensitive(*_applyButton, False);
    }
    else {
	_interface->setDeferredExec(_applyCmdList);
	if (*_applyButton)
            XtSetSensitive(*_applyButton, True);
    }

    _value = (CmdValue)(!_value);
    newValue();

}
