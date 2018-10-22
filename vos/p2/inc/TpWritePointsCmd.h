////////////////////////////////////////////////////////////
// TpWritePointsCmd.h: Write collected points 
// to the ASCII file.
////////////////////////////////////////////////////////////
#ifndef TPWRITEPOINTSCMD_H
#define TPWRITEPOINTSCMD_H
#include "NoUndoCmd.h"

class TpMatchManager;

class TpWritePointsCmd : public NoUndoCmd {

  protected:

    TpMatchManager *_matchManager;
    
    virtual void doit();
    
  public:

    TpWritePointsCmd(const char *, int, TpMatchManager *);

    virtual const char *const className () { return "TpWritePointsCmd"; }
};
#endif
