/////////////////////////////////////////////////////////////
// SetVertHistGraphCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETVERTHISTGRAPHCMD_H
#define SETVERTHISTGRAPHCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetVertHistGraphCmd : public RadioCmd {
  private:
    OrientType _oldValue;
    HistBox *_menuView;
  protected:
    
    virtual void doit();   
//    virtual void undoit(); 

  public:
    
    SetVertHistGraphCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetVertHistGraphCmd"; }
};
#endif
