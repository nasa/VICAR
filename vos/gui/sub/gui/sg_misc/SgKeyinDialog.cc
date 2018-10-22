////////////////////////////////////////////////////////////////
// SgKeyinDialog.cc: A dialog class used to take single string
//      input from the user
////////////////////////////////////////////////////////////////

#include "SgKeyinDialog.h"

SgKeyinDialog::SgKeyinDialog(const char *name, Cmd *cmd)
  : SgCmdDialog(name, cmd, Invisible, Invisible, Invisible, 
		Default, Invisible)
{
    _ci = NULL;
    _command = cmd;
}

CmdInterface *SgKeyinDialog::createCmdInterface(Widget parent, Cmd *cmd)
{
  _ci = new SgKeyinCmdInterface(parent, cmd);
  return _ci;
}  
