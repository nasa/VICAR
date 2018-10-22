////////////////////////////////////////////////////////////////
// SgSearchTextDialog.cc:  A dialog for SgSearchTextCmd objects;
// overrides CustomDialog::apply for immediate execution of a 
// single command (as opposed to the deferred list)
////////////////////////////////////////////////////////////////

#include "SgSearchTextDialog.h"

String SgSearchTextDialog::_defaults[] = {
  (char *)"*OK*labelString: Find",
  NULL
};


SgSearchTextDialog::SgSearchTextDialog(const char *name, Cmd *cmd)
  : SgCmdDialog(name, cmd, Default, Invisible, Invisible, 
		Visible, Invisible)
{
  _ci = NULL;
  _command = cmd;
}


CmdInterface *SgSearchTextDialog::createCmdInterface(Widget parent, Cmd *cmd)
{
  
  setDefaultResources(parent, _defaults);
  _ci = new SgSearchTextCmdInterface( parent, cmd );

  return _ci;
}  

//////////////////////////////////////////////////////////
// Unlike CustomDialog, our apply() only executes a single
// Cmd, not a cmdList
//////////////////////////////////////////////////////////
 
void SgSearchTextDialog::apply()
{
  if (_command)
    _ci->triggerCmd();
}
