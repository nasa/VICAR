/////////////////////////////////////////////////////////////
// TpSaveConfigCmd.h: Save configuration file.
/////////////////////////////////////////////////////////////
#ifndef TPSAVECONFIGCMD_H
#define TPSAVECONFIGCMD_H
#include "NoUndoCmd.h"

class TpWindow;

class TpSaveConfigCmd : public NoUndoCmd {

  protected:

    TpWindow *_window;

    virtual void doit();

  public:

    TpSaveConfigCmd(const char *name, int active, TpWindow *);

    virtual const char *const className () { return "TpSaveConfigCmd"; }
};
#endif
