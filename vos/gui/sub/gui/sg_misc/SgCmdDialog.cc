////////////////////////////////////////////////////////////////
// SgCmdDialog.cc: This is an abstract class that implements a 
// dialog that holds a command interface.  The subclasses must
// provide the *command interface*. The *command* itself is created
// outside the class and is passed in via the constructor.
////////////////////////////////////////////////////////////////
#include "SgCmdDialog.h"
#include "Cmd.h"
#include "CmdInterface.h"

#include <Xm/Xm.h>

SgCmdDialog::SgCmdDialog(const char *name, Cmd *cmd, ButtonState showOk,
			 ButtonState showApply,	ButtonState showReset,
			 ButtonState showCancel, ButtonState showHelp)
  : CustomDialog(name, showOk, showApply, showReset, 
		 showCancel, showHelp)
{
    _cmd = cmd; 
    _cmdInterface = NULL;
}

SgCmdDialog::~SgCmdDialog()
{
    if (_cmdInterface)
	delete _cmdInterface;
}

Widget SgCmdDialog::createWorkArea(Widget parent)
{
    _cmdInterface = createCmdInterface(parent, _cmd);

    setCmdIfDeferredExec();

    XtVaSetValues(_cmdInterface->baseWidget(),
		XmNleftAttachment,   XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopAttachment,    XmATTACH_FORM,
		NULL);

    return _cmdInterface->baseWidget();
}

// Subclasses should overwrite this function if they don't need deferred 
// execution (e.g. for very simple one-item dialogs)

void SgCmdDialog::setCmdIfDeferredExec()
{
    _cmdInterface->setDeferredExec(_applyCmdList);    
}

// Reset dialog by setting a new Command.  This new Command should have
// new value in it already.

void SgCmdDialog::resetDialog(Cmd *cmd)
{
    if (_cmdInterface)
	_cmdInterface->setCommand(cmd);
}
