/////////////////////////////////////////////////////////////
// SiPrintBrowserCmd:  Accepts a filename from a SingleFileSelection
// dialog (as CmdValue) and stuffs it in the SiPrintCmdInterface.
/////////////////////////////////////////////////////////////

#include "SiPrintBrowserCmd.h"
#include "SiPrintCmdInterface.h"

SiPrintBrowserCmd::SiPrintBrowserCmd(const char *name, int active,
			SiPrintCmdInterface *pci)
	: NoUndoCmd(name, active)
{
   _pci = pci;
}

void SiPrintBrowserCmd::doit()
{
   _pci->setFilename((char *)_value);
}

void SiPrintBrowserCmd::freeValue(CmdValue value)
{
   delete (char *)value;
}

