////////////////////////////////////////////////////////////
// TpLoadImageCmd.h: 
////////////////////////////////////////////////////////////
#ifndef TPLOADIMAGECMD_H
#define TPLOADIMAGECMD_H
#include "Cmd.h"

class TpDisplayer;

class TpLoadImageCmd : public Cmd {

  protected:

    TpDisplayer *_displayer;
    
    virtual void doit();
    virtual void undoit();
    
  public:

    TpLoadImageCmd(const char *, int, TpDisplayer *);

    virtual const char *const className () { return "TpLoadImageCmd"; }
};
#endif
