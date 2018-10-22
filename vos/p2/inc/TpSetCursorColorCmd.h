///////////////////////////////////////////////////////////////////////////////
// TpSetCursorColorCmd.h: This command class allow user to set cursor color to 
// one of the X predefined colors.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETCURSORCOLORCMD_H
#define TPSETCURSORCOLORCMD_H
#include "Cmd.h"

class TpDisplayer;

class TpSetCursorColorCmd : public Cmd {

  protected:

    TpDisplayer *_displayer;

    void doit();
    void undoit() { }

  public:

    TpSetCursorColorCmd(const char *name, int active, CmdValue, TpDisplayer *);
    virtual ~TpSetCursorColorCmd() { }

    virtual const char *const className () { return "TpSetCursorColorCmd"; }
};

#endif
