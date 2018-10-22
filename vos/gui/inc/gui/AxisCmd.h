/////////////////////////////////////////////////////////////
// AxisCmd.h: Displays or hides histogram axis.
/////////////////////////////////////////////////////////////
#ifndef AXISCMD_H
#define AXISCMD_H
#include "Cmd.h"

class HistBox;

class AxisCmd : public Cmd {

  private:

    int _oldValue;
    HistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    AxisCmd ( const char *, int, HistBox * );

    virtual const char *const className () { return "AxisCmd"; }
};
#endif
