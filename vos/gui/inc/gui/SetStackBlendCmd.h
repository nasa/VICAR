/////////////////////////////////////////////////////////////
// SetStackBlendCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETSTACKBLENDCMD_H
#define SETSTACKBLENDCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetStackBlendCmd : public RadioCmd {

  private:
    HistBox *_menuView;

  protected:
    virtual void doit();   

  public:
    SetStackBlendCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetStackBlendCmd"; }

};
#endif
