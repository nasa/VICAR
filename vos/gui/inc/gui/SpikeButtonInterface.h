//////////////////////////////////////////////////////////////
// SpikeButtonInterface.h: An SpikeButton interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef SPIKEBUTTONINTERFACE
#define SPIKEBUTTONINTERFACE
#include "ArrowButtonInterface.h"
//#include "Histogram.h"

class Histogram;

class SpikeButtonInterface : public ArrowButtonInterface {
  protected:    

    int _step;
    Histogram *_histogram;
    virtual void executeCmd(XtPointer);
    virtual void setValue(CmdValue);

  public:
    
    SpikeButtonInterface ( Widget, int, Histogram *, Cmd * );
};
#endif
