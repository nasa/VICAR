/////////////////////////////////////////////////////////////////////////////
// TpExitStatusCmd.cc: Set application exit status.
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher - July 1997      JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPEXITSTATUSCMD_H
#define TPEXITSTATUSCMD_H
#include "NoUndoCmd.h"

class TpExitStatusCmd : public NoUndoCmd {

  protected:

    virtual void doit();
    
  public:

    TpExitStatusCmd(const char *name, int active);

    virtual const char *const className () { return "TpExitStatusCmd"; }
};
#endif
