////////////////////////////////////////////////////////////
// TpClosePointFileCmd.cc: Close ibis-2 point file.
////////////////////////////////////////////////////////////
#include "TpClosePointFileCmd.h"
#include "TpMatchManager.h"
#include "TpCloseCmd.h"

TpClosePointFileCmd::TpClosePointFileCmd(const char *name, int active, 
				       TpMatchManager *mm) 
    : NoUndoCmd(name, active)
{
    _matchManager = mm;
}

void TpClosePointFileCmd::doit()
{
    if (_matchManager->isDirty()) {
	Cmd *saveCmd = new TpCloseCmd ("close", True,
				       _matchManager, (Cmd *)this);
	saveCmd->execute();
    }
    else {
	_matchManager->closePointsIbis();
    }
}       

