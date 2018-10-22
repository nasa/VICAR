///////////////////////////////////////////////////////////////////////////////
// TpSaveConfigAsCmd.h: Saves configuration file as the name provided as a 
// command value.
//////////////////////////////////////////////////////////////////////////////
#ifndef TPSAVECONFIGASCMD_H
#define TPSAVECONFIGASCMD_H
#include "NoUndoCmd.h"

class TpWindow;

class TpSaveConfigAsCmd : public NoUndoCmd {

  private:

    TpWindow *_window;

    CmdValue _tmpValue;

    static void okCallback(void *clientData);

  protected:

    virtual void doit();

  public:

    TpSaveConfigAsCmd(const char *name, int active, TpWindow *);

    virtual void execute(CmdValue new_value = NULL);

    virtual void freeValue(CmdValue value) { if (value) delete (char *)value; }

    virtual const char *const className () { return "TpSaveConfigAsCmd"; }
};
#endif
