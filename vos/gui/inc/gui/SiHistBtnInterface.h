//////////////////////////////////////////////////////////////
// SiHistBtnInterface.h: A "push button" interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef SIHISTBTNINTERFACE_H
#define SIHISTBTNINTERFACE_H
#include "SgDrawAreaInterface.h"

class SiHistogram;

class SiHistBtnInterface : public SgDrawAreaInterface {

  public:
    
    SiHistBtnInterface ( Widget, Cmd *, 
		SiHistogram*, SiHistogram*, SiHistogram* );
    virtual ~SiHistBtnInterface() { }
};
#endif
