///////////////////////////////////////////////////////////////////////////////
// TpSetMatchModeCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetMatchModeCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>
#include <stdio.h>

TpSetMatchModeCmd::TpSetMatchModeCmd(const char *name, int active, 
				     CmdValue starting_value,
				     CmdList *radCmdList, 
				     TpMatchMode matchMode, 
				     TpMatchManager *mm)
    : RadioCmd(name, active, starting_value, radCmdList)
{
    _matchMode = matchMode;
    _matchManager = mm;
}

void TpSetMatchModeCmd::doit()
{
    if (_value) {
	_matchManager->setMatchMode(_matchMode);
    }
}
