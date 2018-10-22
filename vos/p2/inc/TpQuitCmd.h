/////////////////////////////////////////////////////////////////////////////
// TpQuitCmd.h: Exit the application if there is no tiepoint file to save.
// Otherwise execute TpCloseCmd and provide it with 'this' pointer so that
// TpCloseCmd object could call us again when it is done with closing project.
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher - July 1997      JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPQUITCMD_H
#define TPQUITCMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpQuitCmd : public NoUndoCmd {

  protected:

    TpMatchManager *_matchManager;

    virtual void doit();
    
  public:

    TpQuitCmd(const char *name, int active, TpMatchManager *);

    virtual const char *const className () { return "TpQuitCmd"; }
};
#endif
