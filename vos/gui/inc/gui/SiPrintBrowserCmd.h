/////////////////////////////////////////////////////////////
// SiPrintBrowserCmd:  Accepts a filename from a SingleFileSelection
// dialog (as CmdValue) and stuffs it in the appropriate slot in
// the SiPrintCmdInterface.
/////////////////////////////////////////////////////////////
#ifndef SIPRINTBROWSERCMD_H
#define SIPRINTBROWSERCMD_H
#include "NoUndoCmd.h"

class SiPrintCmdInterface;

class SiPrintBrowserCmd : public NoUndoCmd {

 protected:

   SiPrintCmdInterface *_pci;

   virtual void doit();

 public:

   SiPrintBrowserCmd(const char *name, int active, SiPrintCmdInterface *pci);

   virtual void freeValue(CmdValue);

   virtual const char *const className () { return "SiPrintBrowserCmd"; }
};
#endif

