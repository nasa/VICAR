/////////////////////////////////////////////////////////////
// StatsCmd.h : This include file defines the command for
//              turning the Histogram graph on and off
/////////////////////////////////////////////////////////////
#ifndef STATSCMD_H
#define STATSCMD_H
#include "Cmd.h"

class HistBox;

class StatsCmd : public Cmd {

  private:

    int _oldValue;        // Last valid command for Undo
    HistBox *_menuView;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    StatsCmd ( const char *, int, HistBox * );
    virtual const char *const className () { return "StatsCmd"; }
};
#endif
