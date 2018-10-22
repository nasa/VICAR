////////////////////////////////////////////////////////////
// TpLoadPointFileCmd.h: Load ibis-2 point file.
////////////////////////////////////////////////////////////
#ifndef TPLOADPOINTFILECMD_H
#define TPLOADPOINTFILECMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpLoadPointFileCmd : public NoUndoCmd {

  protected:

    TpMatchManager *_matchManager;
    
    virtual void doit();
    
  public:

    TpLoadPointFileCmd(const char *, int, TpMatchManager *);

    virtual void freeValue(CmdValue value) { if (value) delete (char *)value; }

    virtual const char *const className () { return "TpLoadPointFileCmd"; }
};
#endif
