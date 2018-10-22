///////////////////////////////////////////////////////////////////////////////
// TpSetPointColorSelCmd.h: This command class allow user to set point's 
// selected color to one of the X predefined colors.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETPOINTCOLORSELCMD_H
#define TPSETPOINTCOLORSELCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpSetPointColorSelCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();
    void undoit() { }

  public:

    TpSetPointColorSelCmd(const char *name, int active, CmdValue, TpMatchManager *);
    virtual ~TpSetPointColorSelCmd() { }

    virtual const char *const className () { return "TpSetPointColorSelCmd"; }
};

#endif
