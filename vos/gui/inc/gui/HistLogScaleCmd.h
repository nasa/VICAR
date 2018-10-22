/////////////////////////////////////////////////////////////
// HistLogScaleCmd.h: Displays histogram using logarithmic scale.
/////////////////////////////////////////////////////////////
#ifndef HISTLOGSCALECMD_H
#define HISTLOGSCALECMD_H
#include "Cmd.h"

class HistBox;

class HistLogScaleCmd : public Cmd {

  private:

    int _oldValue;
    HistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:

    HistLogScaleCmd ( const char *, int, HistBox * );

    virtual const char *const className () { return "HistLogScaleCmd"; }
};
#endif
