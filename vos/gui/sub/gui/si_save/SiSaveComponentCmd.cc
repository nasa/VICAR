////////////////////////////////////////////////////////////////
// SiSaveComponentCmd.h:  Command class for any component within
// the SiSaveCmdInterface; all it does is tell the SCI to do its
// thing.  Used for simple creation of Option menus.
////////////////////////////////////////////////////////////////

#include "SiSaveComponentCmd.h"
#include "SiSaveCmdInterface.h"

SiSaveComponentCmd::SiSaveComponentCmd(const char *name, int active,
		CmdList *radioList, SiSaveCmdInterface *sci)
	: RadioCmd(name, active, radioList)
{
    _sci = sci;
}

void SiSaveComponentCmd::doit()
{
    if (_value) {
	_sci->runCommand();
    }
}

