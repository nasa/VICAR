///////////////////////////////////////////////////////////////////////////////
// TpRedoMatchIdsCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpRedoMatchIdsCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>
#include <stdio.h>

TpRedoMatchIdsCmd::TpRedoMatchIdsCmd(const char *name, int active, 
				     TpMatchManager *mm)
    : WarnNoUndoCmd(name, active)
{
    _matchManager = mm;
}

void TpRedoMatchIdsCmd::doit()
{
    _matchManager->redoIds();
}
