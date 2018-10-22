///////////////////////////////////////////////////////////////////////////////
// TpSetPointSizeCmd.h: This command class allow user to set point size to 
// one of the predefined shapes.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETPOINTSIZECMD_H
#define TPSETPOINTSIZECMD_H
#include "RadioCmd.h"
#include "TpPoint.h"

class TpMatchManager;

class TpSetPointSizeCmd : public RadioCmd {

  protected:

    TpMatchManager *_matchManager;
    int _size;

    void doit();
    void undoit() { }

  public:

    TpSetPointSizeCmd(const char *name, int active, CmdList *, 
		      int size, TpMatchManager *);
    virtual ~TpSetPointSizeCmd() { }

    int getSize() { return _size; }

    virtual const char *const className () { return "TpSetPointSizeCmd"; }
};

#endif
