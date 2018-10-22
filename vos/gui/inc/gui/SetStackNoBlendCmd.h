/////////////////////////////////////////////////////////////
// SetStackNoBlendCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETSTACKNOBLENDCMD_H
#define SETSTACKNOBLENDCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetStackNoBlendCmd : public RadioCmd {

  private:
    HistBox *_menuView;

  protected:    
    virtual void doit();   

  public:    
    SetStackNoBlendCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetStackNoBlendCmd"; }

};
#endif
