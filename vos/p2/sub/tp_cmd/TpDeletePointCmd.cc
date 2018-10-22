/////////////////////////////////////////////////////////////
// TpDeletePointCmd.cc: Saves currently selected match.
/////////////////////////////////////////////////////////////
#include "TpDeletePointCmd.h"
#include "TpMatchManager.h"

TpDeletePointCmd::TpDeletePointCmd(const char *name, int active, 
				   TpMatchManager *matchManager)
	: Cmd(name, active)
{
    _matchManager = matchManager;
}

void TpDeletePointCmd::doit()
{
    _matchManager->deleteCurrentSelection();
}

void TpDeletePointCmd::undoit()
{
    //_matchManager->addDeletedMatch();
}
