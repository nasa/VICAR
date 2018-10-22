////////////////////////////////////////////////////////////////
// SiPrintComponentCmd.h:  Command class for any component within
// the SiPrintCmdInterface; all it does is tell the PCI to do its
// thing.  Used for simple creation of Option menus.
////////////////////////////////////////////////////////////////

#include "SiPrintComponentCmd.h"
#include "SiPrintCmdInterface.h"

SiPrintComponentCmd::SiPrintComponentCmd(const char *name, int active,
		CmdList *radioList, SiPrintCmdInterface *pci)
	: RadioCmd(name, active, radioList)
{
    _pci = pci;
}

void SiPrintComponentCmd::doit()
{
    if (_value) {
	_pci->runCommand();
    }
}

