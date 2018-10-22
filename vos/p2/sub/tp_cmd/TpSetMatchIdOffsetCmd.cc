///////////////////////////////////////////////////////////////////////////////
// TpSetMatchIdOffsetCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetMatchIdOffsetCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>

TpSetMatchIdOffsetCmd::TpSetMatchIdOffsetCmd(const char *name, int active, 
			CmdValue value, TpMatchManager *mm)
    : Cmd(name, active, value)
{
    _matchManager = mm;
}

void TpSetMatchIdOffsetCmd::doit()
{
    char *s = (char *)_value;
    int startId = atoi(s);
    _matchManager->setStartId(startId);
}
