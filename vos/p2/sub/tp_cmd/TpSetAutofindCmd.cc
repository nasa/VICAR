////////////////////////////////////////////////////////////
// TpSetAutofindCmd.cc: Set number of displays.
////////////////////////////////////////////////////////////
#include "TpSetAutofindCmd.h"
#include "TpMatchManager.h"

TpSetAutofindCmd::TpSetAutofindCmd(const char *name, int active, 
		CmdValue starting_value, CmdList *radioList,
		TpMatchManager *mm, TpAutofindMode autofindMode)
    : RadioCmd(name, active, starting_value, radioList)
{
    _matchManager = mm;
    _autofindMode = autofindMode;
}

void TpSetAutofindCmd::doit()
{
    if (_value)
	_matchManager->setAutofindMode(_autofindMode);
}

