///////////////////////////////////////////////////////////////////////////////
// TpRedoMatchIdsCmd.h: This command class allow user to redo all the general 
// qualifier ids.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPREDOMATCHIDSCMD_H
#define TPREDOMATCHIDSCMD_H
#include "WarnNoUndoCmd.h"

class TpMatchManager;

class TpRedoMatchIdsCmd : public WarnNoUndoCmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();

  public:

    TpRedoMatchIdsCmd(const char *name, int active, TpMatchManager *);
    virtual ~TpRedoMatchIdsCmd() { }

    virtual const char *const className () { return "TpRedoMatchIdsCmd"; }
};

#endif
