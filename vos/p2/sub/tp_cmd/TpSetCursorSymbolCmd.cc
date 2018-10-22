///////////////////////////////////////////////////////////////////////////////
// TpSetCursorSymbolCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetCursorSymbolCmd.h"
#include "TpDisplayer.h"
#include <stdlib.h>
#include <stdio.h>

TpSetCursorSymbolCmd::TpSetCursorSymbolCmd(const char *name, int active, 
					 CmdList *radCmdList, 
					 TpDisplayer *disp)
    : RadioCmd(name, active, radCmdList)
{
    _cursor = sdup(name);
    _displayer = disp;

    if (!strcmp(_displayer->getCursor(), _cursor)) {
	_value = (CmdValue)_cursor;
	newValue();
    }
}

void TpSetCursorSymbolCmd::doit()
{
    if (_value) {
	_displayer->setCursor(_cursor);
    }
}
