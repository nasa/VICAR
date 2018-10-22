///////////////////////////////////////////////////////////////////////////////
// TpSetPointColorSelCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetPointColorSelCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>
#include <stdio.h>

TpSetPointColorSelCmd::TpSetPointColorSelCmd(const char *name, int active, 
					     CmdValue value, 
					     TpMatchManager *mm)
    : Cmd(name, active, value)
{
    _matchManager = mm;
}

void TpSetPointColorSelCmd::doit()
{
    _matchManager->setPointColorSel((char *)_value);
}
