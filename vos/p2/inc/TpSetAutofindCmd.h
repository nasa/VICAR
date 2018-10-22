////////////////////////////////////////////////////////////
// TpSetAutofindCmd.h: Set autofind mode
////////////////////////////////////////////////////////////
#ifndef TPSETAUTOFINDCMD_H
#define TPSETAUTOFINDCMD_H
#include "RadioCmd.h"
#include "TpDefs.h"

class TpMatchManager;

class TpSetAutofindCmd : public RadioCmd {

  protected:

    TpMatchManager *_matchManager;
    TpAutofindMode _autofindMode;

    virtual void doit();
    
  public:

    TpSetAutofindCmd(const char *, int, CmdValue, CmdList *, 
			TpMatchManager *, TpAutofindMode);

    virtual const char *const className () { return "TpSetAutofindCmd"; }
};
#endif
