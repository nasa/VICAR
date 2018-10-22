////////////////////////////////////////////////////////////
// TpSwapLockCmd.cc: Set image swap lock option on or off.
////////////////////////////////////////////////////////////
#include "TpSwapLockCmd.h"
#include "TpDisplayer.h"

TpSwapLockCmd::TpSwapLockCmd(const char *name, int active,
		TpDisplayer *displayer, int i)
	: Cmd(name, active)
{
    _displayer = displayer;
    _imageNo = i;
}

void TpSwapLockCmd::doit()
{
    if (_value)
	_displayer->setLock(_imageNo);
    else 
	_displayer->unSetLock(_imageNo);
}

void TpSwapLockCmd::undoit()
{
//    if ((int)_value)
//        _displayer->setLock(_imageNo);
//    else
//        _displayer->unSetLock(_imageNo);
}
