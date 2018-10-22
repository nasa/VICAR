////////////////////////////////////////////////////////////////
// SiPrintComponentCmd.h:  Command class for any component within
// the SiPrintCmdInterface; all it does is tell the PCI to do its
// thing.  Used for simple creation of Option menus.
////////////////////////////////////////////////////////////////
#ifndef SIPRINTCOMPONENTCMD_H
#define SIPRINTCOMPONENTCMD_H
#include "RadioCmd.h"

class SiPrintCmdInterface;

class SiPrintComponentCmd : public RadioCmd {

  protected:

    SiPrintCmdInterface *_pci;

    virtual void doit();

  public:
    
    SiPrintComponentCmd(const char *, int, CmdList *, SiPrintCmdInterface *);

    virtual const char *const className () { return "SiPrintComponentCmd"; }
};
#endif

