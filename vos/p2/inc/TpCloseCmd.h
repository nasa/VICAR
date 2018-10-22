////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
// TpCloseCmd.h: Select the point file then call the quit command
// so that the application may exit.
////////////////////////////////////////////////////////////
#ifndef TPCLOSECMD_H
#define TPCLOSECMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpCloseCmd : public NoUndoCmd {

  private:

    static void saveCallback(void *clientData);
    static void noSaveCallback(void *clientData);

    void save();
    void noSave();

  protected:

    TpMatchManager *_matchManager;
    Cmd *_quitCmd;
    
    virtual void doit();
    
  public:

    TpCloseCmd(const char *, int, TpMatchManager *, Cmd *);

    virtual const char *const className () { return "TpCloseCmd"; }
};
#endif
