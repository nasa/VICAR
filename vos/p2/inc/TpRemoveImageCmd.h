////////////////////////////////////////////////////////////
// TpRemoveImageCmd.h: 
////////////////////////////////////////////////////////////
#ifndef TPREMOVEIMAGECMD_H
#define TPREMOVEIMAGECMD_H
#include "WarnNoUndoCmd.h"

class TpDisplayer;

class TpRemoveImageCmd : public WarnNoUndoCmd {

  protected:

    TpDisplayer *_displayer;
    
    virtual void doit();
    
  public:

    TpRemoveImageCmd(const char *, int, TpDisplayer *);

    virtual const char *const className () { return "TpRemoveImageCmd"; }
};
#endif
