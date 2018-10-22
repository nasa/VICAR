/////////////////////////////////////////////////////////////
// TpSavePointFileCmd.h: Saves currently selected match.
/////////////////////////////////////////////////////////////
#ifndef TPSAVEPOINTFILECMD_H
#define TPSAVEPOINTFILECMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpSavePointFileCmd : public NoUndoCmd {

  private:

    TpMatchManager *_matchManager;

  protected:

    virtual void doit();

  public:

    TpSavePointFileCmd(const char *name, int active, TpMatchManager *matchManager);
    virtual ~TpSavePointFileCmd() { };

    virtual const char *const className () { return "TpSavePointFileCmd"; }
};
#endif
