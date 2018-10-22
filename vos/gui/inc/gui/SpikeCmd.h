/////////////////////////////////////////////////////////////
// SpikeCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SPIKECMD_H
#define SPIKECMD_H
#include "Cmd.h"

class HistBox;

class SpikeCmd : public Cmd {

  protected:

    virtual void doit();   
    virtual void undoit(); 
    HistBox *_histBox;
    int _prevValue;

  public:

    SpikeCmd ( const char *, int, HistBox * );
    virtual const char *const className () { return "SpikeCmd"; }
};
#endif
