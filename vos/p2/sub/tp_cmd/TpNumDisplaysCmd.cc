////////////////////////////////////////////////////////////
// TpNumDisplaysCmd.cc: Set number of displays.
////////////////////////////////////////////////////////////
#include "TpNumDisplaysCmd.h"
#include "TpDisplayer.h"

TpNumDisplaysCmd::TpNumDisplaysCmd(const char *name, int active, 
		CmdValue starting_value, CmdList *radioList,
		TpDisplayer *displayer, int numDisplays)
	: RadioCmd(name, active, starting_value, radioList)
{
    _displayer = displayer;
    _numDisplays = numDisplays;
}

void TpNumDisplaysCmd::doit()
{
    if (_value)
	_displayer->setNumDisplays(_numDisplays);
}

