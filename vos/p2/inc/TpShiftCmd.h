////////////////////////////////////////////////////////////
// TpShiftCmd.h: Shift subdisplays either to the left or to
// the right depending on the value passed in the constructor.
////////////////////////////////////////////////////////////
#ifndef TPSHIFTCMD_H
#define TPSHIFTCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>	// for Boolean

class TpDisplayer;

class TpShiftCmd : public Cmd {

  protected:

    TpDisplayer *_displayer;
    Boolean _leftShift;
    
    virtual void doit();
    virtual void undoit();
    
  public:

    TpShiftCmd(const char *, int, TpDisplayer *, Boolean leftShift);

    virtual const char *const className () { return "TpShiftCmd"; }
};
#endif
