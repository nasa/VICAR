///////////////////////////////////////////////////////////////////////////////
// TpSetPointColorCmd.h: This command class allow user to set point color to 
// one of the X predefined colors.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETPOINTCOLORCMD_H
#define TPSETPOINTCOLORCMD_H
#include "Cmd.h"

class TpMatchManager;

class TpSetPointColorCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();
    void undoit() { }

  public:

    TpSetPointColorCmd(const char *name, int active, CmdValue, TpMatchManager *);
    virtual ~TpSetPointColorCmd() { }

    virtual const char *const className () { return "TpSetPointColorCmd"; }
};

#endif
