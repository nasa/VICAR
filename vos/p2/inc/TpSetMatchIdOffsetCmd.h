///////////////////////////////////////////////////////////////////////////////
// TpSetMatchIdOffsetCmd:  
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETMATCHIDOFFSETCMD_H
#define TPSETMATCHIDOFFSETCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpSetMatchIdOffsetCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();
    void undoit() { }

  public:

    TpSetMatchIdOffsetCmd(const char *name, int active, CmdValue, TpMatchManager *);
    virtual ~TpSetMatchIdOffsetCmd() { }

    virtual const char *const className () { return "TpSetMatchIdOffsetCmd"; }
};

#endif
