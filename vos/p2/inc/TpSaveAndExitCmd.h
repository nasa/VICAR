/////////////////////////////////////////////////////////////
// TpSaveAndExitCmd.h: Saves currently selected match.
/////////////////////////////////////////////////////////////
#ifndef TPSAVEANDEXITCMD_H
#define TPSAVEANDEXITCMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpSaveAndExitCmd : public NoUndoCmd {

  private:

    TpMatchManager *_matchManager;

  protected:

    virtual void doit();

  public:

    TpSaveAndExitCmd(const char *name, int active, TpMatchManager *matchManager);
    virtual ~TpSaveAndExitCmd() { };

    virtual const char *const className () { return "TpSaveAndExitCmd"; }
};
#endif
