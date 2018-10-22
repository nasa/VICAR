/////////////////////////////////////////////////////////////
// SetAscAxisCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETASCAXISCMD_H
#define SETASCAXISCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetAscAxisCmd : public RadioCmd {
  private:
    VerAxisDirType _oldValue;
    HistBox *_menuView;
  protected:
    
    virtual void doit();   

  public:
    
    SetAscAxisCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetAscAxisCmd"; }
};
#endif
