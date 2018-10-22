///////////////////////////////////////////////////////////////////////////////
// TpSetMatchIdNextCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetMatchIdNextCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>

TpSetMatchIdNextCmd::TpSetMatchIdNextCmd(const char *name, int active, 
			CmdValue value, TpMatchManager *mm)
    : Cmd(name, active, value)
{
    _matchManager = mm;
}

void TpSetMatchIdNextCmd::doit()
{
    char *s = (char *)_value;
    int startId = atoi(s);
    _matchManager->setNextId(startId);
}
