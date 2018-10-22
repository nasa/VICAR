///////////////////////////////////////////////////////////////////////////////
// TpColorCodeCmd.h: This command class allow user to colorcode point color 
// based on the qualifier value
///////////////////////////////////////////////////////////////////////////////
#ifndef TPCOLORCODECMD_H
#define TPCOLORCODECMD_H
#include "Cmd.h"
#include "TpPoint.h"

class TpMatchManager;

class TpColorCodeCmd : public Cmd {

  protected:

    TpMatchManager *_matchManager;

    void doit();
    void undoit() { }

  public:

    TpColorCodeCmd(const char *name, int active, TpMatchManager *);
    virtual ~TpColorCodeCmd() { }

    virtual const char *const className () { return "TpColorCodeCmd"; }
};

#endif
