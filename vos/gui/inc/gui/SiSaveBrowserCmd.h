/////////////////////////////////////////////////////////////
// SiSaveBrowserCmd:  Accepts a filename from a SingleFileSelection
// dialog (as CmdValue) and stuffs it in the appropriate slot in
// the SiSaveCmdInterface.
/////////////////////////////////////////////////////////////
#ifndef SISAVEBROWSERCMD_H
#define SISAVEBROWSERCMD_H
#include "NoUndoCmd.h"

class SiSaveCmdInterface;

class SiSaveBrowserCmd : public NoUndoCmd {

 protected:

   SiSaveCmdInterface *_sci;
   int _which;

   virtual void doit();

 public:

   SiSaveBrowserCmd(const char *name, int active, SiSaveCmdInterface *sci,
							int which);

   virtual void freeValue(CmdValue);

   virtual const char *const className () { return "SiSaveBrowserCmd"; }
};
#endif

