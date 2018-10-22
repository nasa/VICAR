/////////////////////////////////////////////////////////////
// TpSavePointFileAsCmd.h: Saves currently selected match.
/////////////////////////////////////////////////////////////
#ifndef TPSAVEPOINTFILEASCMD_H
#define TPSAVEPOINTFILEASCMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpSavePointFileAsCmd : public NoUndoCmd {

  private:

    TpMatchManager *_matchManager;
    Cmd *_quitCmd;

    CmdValue _tmpValue;

    static void okCallback(void *clientData);

  protected:

    virtual void doit();

  public:

    TpSavePointFileAsCmd(const char *name, int active, 
			 TpMatchManager *matchManager, Cmd *quitCmd = NULL);
    virtual ~TpSavePointFileAsCmd() { };

    virtual void execute(CmdValue new_value = NULL);

    virtual void freeValue(CmdValue value) { if (value) delete (char *)value; }

    virtual const char *const className () { return "TpSavePointFileAsCmd"; }
};
#endif
