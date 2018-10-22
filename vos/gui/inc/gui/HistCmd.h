/////////////////////////////////////////////////////////////
// HistCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef HISTCMD_H
#define HISTCMD_H
#include "Cmd.h"

class HistBox;

class HistCmd : public Cmd {

  private:

    int _oldValue;
    HistBox *_menuView;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    HistCmd ( char *, int, HistBox * );
    virtual const char *const className () { return "HistCmd"; }
};
#endif
