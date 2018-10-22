////////////////////////////////////////////////////////////////
// BlinkImageCmd.h:  Command class for any component within
// the BlinkControl; all it does is tell the BC to do its
// thing.  Used for simple creation of radio buttons.
////////////////////////////////////////////////////////////////
#ifndef BLINKIMAGECMD_H
#define BLINKIMAGECMD_H
#include "RadioCmd.h"

class BlinkControl;

class BlinkImageCmd : public RadioCmd {

  protected:

    BlinkControl *_bc;
    int _which;

    virtual void doit();

  public:
    
    BlinkImageCmd(char *, int, CmdList *, BlinkControl *, int which);

    virtual const char *const className () { return "BlinkImageCmd"; }
};
#endif

