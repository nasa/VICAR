/////////////////////////////////////////////////////////////
// TpListPointsCmd.cc: Outputs all points to stdout.
/////////////////////////////////////////////////////////////
#include "TpListPointsCmd.h"
#include "TpMatchManager.h"

TpListPointsCmd::TpListPointsCmd(const char *name, int active, 
				 TpMatchManager *matchManager)
	: NoUndoCmd(name, active)
{
    _matchManager = matchManager;
}

void TpListPointsCmd::doit()
{
    _matchManager->listPoints();
}
