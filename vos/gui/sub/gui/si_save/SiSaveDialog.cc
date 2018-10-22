//////////////////////////////////////////////////////////////////////
// SiSaveDialog.cc: This class creates a work area for save dialog.
//////////////////////////////////////////////////////////////////////
#include "SiSaveDialog.h"
#include "SiSaveCmdInterface.h"
#include "Cmd.h"

SiSaveDialog::SiSaveDialog(const char *name, Cmd *cmd)
    : CustomDialog(name, Default, Invisible, Invisible, Visible, Visible)
{
    _cmd = cmd;
    _saveCI = NULL;
}

Widget SiSaveDialog::createWorkArea(Widget parent)
{
    _saveCI = new SiSaveCmdInterface(parent, _cmd);
    _saveCI->setDeferredExec(_applyCmdList);
    return _saveCI->baseWidget();
}

//////////////////////////////////////////////////////////////////////
// We want OK to do something even if the user doesn't enter any values
// (e.g. save again to the same place).  So, we execute the command
// when the dialog is posted, just to prime the queue.  (otherwise, the
// user has to *do* something to get an execution on the deferred list).
//////////////////////////////////////////////////////////////////////

void SiSaveDialog::post()
{
    CustomDialog::post();		// do all the real work
    if (_saveCI)
        _saveCI->runCommand();
}

