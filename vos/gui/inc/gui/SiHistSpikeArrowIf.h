//////////////////////////////////////////////////////////////
// SiHistSpikeArrowIf.h: An arrow button interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef SIHISTSPIKEARROWIF_H
#define SIHISTSPIKEARROWIF_H
#include "ArrowButtonInterface.h"

class SiHistogram;

class SiHistSpikeArrowIf : public ArrowButtonInterface {

  protected:    

    int _step;
    SiHistogram *_histogram;

    virtual void executeCmd(XtPointer);
    virtual void setValue(CmdValue);

  public:
    
    SiHistSpikeArrowIf ( Widget, int, SiHistogram *, Cmd * );
    virtual ~SiHistSpikeArrowIf() { }

};
#endif
