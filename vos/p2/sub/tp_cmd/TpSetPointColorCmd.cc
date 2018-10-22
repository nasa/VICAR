///////////////////////////////////////////////////////////////////////////////
// TpSetPointColorCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetPointColorCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>
#include <stdio.h>

TpSetPointColorCmd::TpSetPointColorCmd(const char *name, int active, CmdValue value,
				       TpMatchManager *mm)
    : Cmd(name, active, value)
{
    _matchManager = mm;
}

void TpSetPointColorCmd::doit()
{
    _matchManager->setPointColor((char *)_value);
}
