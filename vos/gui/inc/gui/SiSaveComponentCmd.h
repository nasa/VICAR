////////////////////////////////////////////////////////////////
// SiSaveComponentCmd.h:  Command class for any component within
// the SiSaveCmdInterface; all it does is tell the SCI to do its
// thing.  Used for simple creation of Option menus.
////////////////////////////////////////////////////////////////
#ifndef SISAVECOMPONENTCMD_H
#define SISAVECOMPONENTCMD_H
#include "RadioCmd.h"

class SiSaveCmdInterface;

class SiSaveComponentCmd : public RadioCmd {

  protected:

    SiSaveCmdInterface *_sci;

    virtual void doit();

  public:
    
    SiSaveComponentCmd(const char *, int, CmdList *, SiSaveCmdInterface *);

    virtual const char *const className () { return "SiSaveComponentCmd"; }
};
#endif

