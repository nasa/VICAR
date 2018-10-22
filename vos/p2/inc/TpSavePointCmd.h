/////////////////////////////////////////////////////////////
// TpSavePointCmd.h: Saves currently selected match.
/////////////////////////////////////////////////////////////
#ifndef TPSAVEPOINTCMD_H
#define TPSAVEPOINTCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpSavePointCmd : public Cmd {

  private:

    TpMatchManager *_matchManager;

  protected:

    virtual void doit();
    virtual void undoit();

  public:

    TpSavePointCmd(const char *name, int active, TpMatchManager *matchManager);
    virtual ~TpSavePointCmd() { };

    virtual const char *const className () { return "TpSavePointCmd"; }
};
#endif
