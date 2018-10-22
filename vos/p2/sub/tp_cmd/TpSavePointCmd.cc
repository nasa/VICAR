/////////////////////////////////////////////////////////////
// TpSavePointCmd.cc: Saves currently selected match.
/////////////////////////////////////////////////////////////
#include "TpSavePointCmd.h"
#include "TpMatchManager.h"

TpSavePointCmd::TpSavePointCmd(const char *name, int active, 
			       TpMatchManager *matchManager)
	: Cmd(name, active)
{
    _matchManager = matchManager;
}

void TpSavePointCmd::doit()
{
    _matchManager->addMatch();
    deactivate();
}

void TpSavePointCmd::undoit()
{
    _matchManager->deleteLastAddedMatch();
}
