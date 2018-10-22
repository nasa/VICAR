///////////////////////////////////////////////////////////////////////////////
// TpSetPointSizeCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetPointSizeCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>
#include <stdio.h>

TpSetPointSizeCmd::TpSetPointSizeCmd(const char *name, int active, 
				     CmdList *radCmdList,
				     int size, TpMatchManager *mm)
    : RadioCmd(name, active, radCmdList)
{
    _size = size;
    _matchManager = mm;
}

void TpSetPointSizeCmd::doit()
{
    if (_value) {
	_matchManager->setPointDimensions(_size, _size);
    }
}
