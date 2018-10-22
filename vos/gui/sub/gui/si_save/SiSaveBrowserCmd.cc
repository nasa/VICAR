/////////////////////////////////////////////////////////////
// SiSaveBrowserCmd:  Accepts a filename from a SingleFileSelection
// dialog (as CmdValue) and stuffs it in the appropriate slot in
// the SiSaveCmdInterface.
/////////////////////////////////////////////////////////////

#include "SiSaveBrowserCmd.h"
#include "SiSaveCmdInterface.h"

SiSaveBrowserCmd::SiSaveBrowserCmd(const char *name, int active,
			SiSaveCmdInterface *sci, int which)
	: NoUndoCmd(name, active)
{
   _sci = sci;
   _which = which;		// r, g, or b
}

void SiSaveBrowserCmd::doit()
{
   _sci->setFilename((char *)_value, _which);
}

void SiSaveBrowserCmd::freeValue(CmdValue value)
{
   delete (char *)value;
}

