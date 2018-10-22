///////////////////////////////////////////////////////////////////////////////
// TpSetMatchModeCmd.h: This command class allow user to set the match mode.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETMATCHMODECMD_H
#define TPSETMATCHMODECMD_H
#include "RadioCmd.h"
#include "TpDefs.h"

class TpMatchManager;

class TpSetMatchModeCmd : public RadioCmd {

  protected:

    TpMatchManager *_matchManager;
    TpMatchMode _matchMode;

    void doit();
    void undoit() { }

  public:

    TpSetMatchModeCmd(const char *name, int active, CmdValue, CmdList *, 
		      TpMatchMode, TpMatchManager *);
    virtual ~TpSetMatchModeCmd() { }

    TpMatchMode getMatchMode() { return _matchMode; }

    virtual const char *const className () { return "TpSetMatchModeCmd"; }
};

#endif
