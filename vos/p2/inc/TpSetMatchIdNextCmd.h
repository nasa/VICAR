///////////////////////////////////////////////////////////////////////////////
// TpSetMatchIdNextCmd:  
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETMATCHIDNEXTCMD_H
#define TPSETMATCHIDNEXTCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpSetMatchIdNextCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();
    void undoit() { }

  public:

    TpSetMatchIdNextCmd(const char *name, int active, CmdValue, TpMatchManager *);
    virtual ~TpSetMatchIdNextCmd() { }

    virtual const char *const className () { return "TpSetMatchIdNextCmd"; }
};

#endif
