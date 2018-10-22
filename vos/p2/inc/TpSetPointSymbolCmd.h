///////////////////////////////////////////////////////////////////////////////
// TpSetPointSymbolCmd.h: This command class allow user to set point symbol to 
// one of the predefined shapes.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETPOINTSYMBOLCMD_H
#define TPSETPOINTSYMBOLCMD_H
#include "RadioCmd.h"
#include "TpPoint.h"

class TpMatchManager;

class TpSetPointSymbolCmd : public RadioCmd {

  protected:

    TpMatchManager *_matchManager;
    TpPointSymbolShapeType _shape;

    void doit();
    void undoit() { }

  public:

    TpSetPointSymbolCmd(const char *name, int active, CmdList *, 
			TpPointSymbolShapeType, TpMatchManager *);
    virtual ~TpSetPointSymbolCmd() { }

    TpPointSymbolShapeType getShape() { return _shape; }

    virtual const char *const className () { return "TpSetPointSymbolCmd"; }
};

#endif
