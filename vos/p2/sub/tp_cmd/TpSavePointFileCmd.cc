/////////////////////////////////////////////////////////////
// TpSavePointFileCmd.cc: Saves points into IBIS-2 file.  
/////////////////////////////////////////////////////////////
#include "TpSavePointFileCmd.h"
#include "TpMatchManager.h"
#include "TpSavePointFileAsCmd.h"
#include "PostSingleFileDialogCmd.h"

TpSavePointFileCmd::TpSavePointFileCmd(const char *name, int active, 
				       TpMatchManager *matchManager)
    : NoUndoCmd(name, active)
{
    _matchManager = matchManager;
}

void TpSavePointFileCmd::doit()
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

}

