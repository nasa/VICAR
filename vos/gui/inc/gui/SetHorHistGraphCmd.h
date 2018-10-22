/////////////////////////////////////////////////////////////
// SetHorHistGraphCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETHORHISTGRAPHCMD_H
#define SETHORHISTGRAPHCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetHorHistGraphCmd : public RadioCmd {
  private:
    OrientType _oldValue;
    HistBox *_menuView;
  protected:
    
    virtual void doit();   
//    virtual void undoit(); 

  public:
    
    SetHorHistGraphCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetHorHistGraphCmd"; }
};
#endif
