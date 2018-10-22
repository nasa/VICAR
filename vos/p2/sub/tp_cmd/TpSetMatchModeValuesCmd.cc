///////////////////////////////////////////////////////////////////////////////
// TpSetMatchModeValuesCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetMatchModeValuesCmd.h"
#include "TpMatchManager.h"
#include "TpMatchModeValues.h"
#include <stdlib.h>
#include <stdio.h>

TpSetMatchModeValuesCmd::TpSetMatchModeValuesCmd(const char *name, int active, 
		 CmdValue value, TpMatchManager *mm)
    : Cmd(name, active, value)
{
    _matchManager = mm;
}

void TpSetMatchModeValuesCmd::doit()
{
    TpMatchModeValues *value = (TpMatchModeValues *)_value;
    _matchManager->setMatchModeValues(value);
}
