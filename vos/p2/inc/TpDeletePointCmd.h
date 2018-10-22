/////////////////////////////////////////////////////////////
// TpDeletePointCmd.h: Deletes currently selected match.
/////////////////////////////////////////////////////////////
#ifndef TPDELETEPOINTCMD_H
#define TPDELETEPOINTCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpDeletePointCmd : public Cmd {

  private:

    TpMatchManager *_matchManager;

  protected:

    virtual void doit();
    virtual void undoit();

  public:

    TpDeletePointCmd(const char *name, int active, TpMatchManager *matchManager);
    virtual ~TpDeletePointCmd() { };

    virtual const char *const className () { return "TpDeletePointCmd"; }
};
#endif
