//////////////////////////////////////////////////////////////////////
// SiPrintDialog.cc: This class creates a work area for print dialog.
//////////////////////////////////////////////////////////////////////
#include "SiPrintDialog.h"
#include "SiPrintCmdInterface.h"
#include "Cmd.h"

SiPrintDialog::SiPrintDialog(const char *name, Cmd *cmd)
    : CustomDialog(name, Default, Invisible, Invisible, Visible, Visible)
{
    _cmd = cmd;
    _printCI = NULL;
}

Widget SiPrintDialog::createWorkArea(Widget parent)
{
    _printCI = new SiPrintCmdInterface(parent, _cmd);
    _printCI->setDeferredExec(_applyCmdList);
    return _printCI->baseWidget();
}

//////////////////////////////////////////////////////////////////////
// We want OK to do something even if the user doesn't enter any values
// (e.g. print again to the same place).  So, we execute the command
// when the dialog is posted, just to prime the queue.  (otherwise, the
// user has to *do* something to get an execution on the deferred list).
//////////////////////////////////////////////////////////////////////

void SiPrintDialog::post()
{
    CustomDialog::post();		// do all the real work
    if (_printCI)
        _printCI->runCommand();
}

