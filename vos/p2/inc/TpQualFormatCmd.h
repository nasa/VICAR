///////////////////////////////////////////////////////////////////
// TpQualFormatCmd.h
///////////////////////////////////////////////////////////////////
#ifndef TPQUALFORMATCMD_H
#define TPQUALFORMATCMD_H
#include "Cmd.h"

class TpQualGroupMgr;

class TpQualFormatCmd : public Cmd {

  protected:

    TpQualGroupMgr *_qualGroupMgr;

    void doit();
    void undoit() { }

  public:

    TpQualFormatCmd(const char *name, int active, TpQualGroupMgr *);
    virtual ~TpQualFormatCmd() { }

    void collectValue();

    virtual const char *const className () { return "TpQualFormatCmd"; }
};

#endif
