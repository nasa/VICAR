////////////////////////////////////////////////////////////
// TpShiftCmd.h: Shift subdisplays either to the left or to 
// the right depending on the value passed in the constructor.
////////////////////////////////////////////////////////////
#include "TpShiftCmd.h"
#include "TpDisplayer.h"
#include <stdlib.h>

TpShiftCmd::TpShiftCmd(const char *name, int active, 
		TpDisplayer *d, Boolean left) 
	: Cmd(name, active)
{
    _displayer = d;
    _leftShift = left;
}

void TpShiftCmd::doit()
{
    if (_leftShift) 
	_displayer->shiftLeft();
    else 
	_displayer->shiftRight();
}       

void TpShiftCmd::undoit()
{
    if (_leftShift) 
        _displayer->shiftRight();
    else
        _displayer->shiftLeft();
}
