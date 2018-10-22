/////////////////////////////////////////////////////////////
// TpListPointsCmd.h: Outputs points to stdout.
/////////////////////////////////////////////////////////////
#ifndef TPLISTPOINTSCMD_H
#define TPLISTPOINTSCMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpListPointsCmd : public NoUndoCmd {

  private:

    TpMatchManager *_matchManager;

  protected:

    virtual void doit();

  public:

    TpListPointsCmd(const char *name, int active, TpMatchManager *matchManager);
    virtual ~TpListPointsCmd() { };

    virtual const char *const className () { return "TpListPointsCmd"; }
};
#endif
