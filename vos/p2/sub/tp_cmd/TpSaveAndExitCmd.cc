/////////////////////////////////////////////////////////////
// TpSaveAndExitCmd.cc: Saves points into IBIS-2 file.  
/////////////////////////////////////////////////////////////
#include "TpSaveAndExitCmd.h"
#include "TpMatchManager.h"
#include "TpSavePointFileAsCmd.h"
#include "PostSingleFileDialogCmd.h"
#include "TpApplication.h"

TpSaveAndExitCmd::TpSaveAndExitCmd(const char *name, int active, 
				   TpMatchManager *matchManager)
    : NoUndoCmd(name, active)
{
    _matchManager = matchManager;
}

void TpSaveAndExitCmd::doit()
{
    if (_matchManager->getIbisFileName())
	_matchManager->writePointsIbis();
    else {
	Cmd *saveCmd = new TpSavePointFileAsCmd("SavePointFileAs",
						True, _matchManager);
	Cmd *cmd = new PostSingleFileDialogCmd("Save Point File As",
					       True, saveCmd);
	cmd->execute();
    }

    exit(theTpApplication->getExitStatus());
}

