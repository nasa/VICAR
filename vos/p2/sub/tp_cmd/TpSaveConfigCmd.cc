/////////////////////////////////////////////////////////////
// TpSaveConfigCmd.cc: Saves configuration file.
/////////////////////////////////////////////////////////////
#include "TpSaveConfigCmd.h"
#include "TpWindow.h"
#include "TpSaveConfigAsCmd.h"
#include "PostSingleFileDialogCmd.h"

TpSaveConfigCmd::TpSaveConfigCmd(const char *name, int active, TpWindow *window)
    : NoUndoCmd(name, active)
{
    _window = window;
}

void TpSaveConfigCmd::doit()
{
    char *config = _window->getConfigFileName();
    if (config && strlen(config))
	_window->saveConfig();
    else {
	Cmd *saveCmd = new TpSaveConfigAsCmd("SaveConfigAs", True, _window);
	Cmd *cmd = new PostSingleFileDialogCmd("Save Config As", True,saveCmd);
	cmd->execute();
    }
}

