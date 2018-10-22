////////////////////////////////////////////////////////////
// TpWritePointsCmd.h: Write collected points
// to the ASCII file.
////////////////////////////////////////////////////////////
#include "TpWritePointsCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>

TpWritePointsCmd::TpWritePointsCmd(const char *name, int active, 
		TpMatchManager *mm) 
	: NoUndoCmd(name, active)
{
    _matchManager = mm;
}

void TpWritePointsCmd::doit()
{
    _matchManager->writePointsIbis();
}       
