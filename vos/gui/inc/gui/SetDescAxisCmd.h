/////////////////////////////////////////////////////////////
// SetDescAxisCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETDESCAXISCMD_H
#define SETDESCAXISCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetDescAxisCmd : public RadioCmd {
  private:
    VerAxisDirType _oldValue;
    HistBox *_menuView;
  protected:
    
    virtual void doit();   

  public:
    
    SetDescAxisCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetDescAxisCmd"; }
};
#endif
