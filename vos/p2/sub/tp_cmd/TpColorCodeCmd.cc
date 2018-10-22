///////////////////////////////////////////////////////////////////////////////
// TpColorCodeCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpColorCodeCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>
#include <stdio.h>

TpColorCodeCmd::TpColorCodeCmd(const char *name, int active, TpMatchManager *mm)
    : Cmd(name, active)
{
    _matchManager = mm;
}

void TpColorCodeCmd::doit()
{
    _matchManager->colorCodePointsPoint(0);
}
