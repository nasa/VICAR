//////////////////////////////////////////////////////////////
// LutBtnInterface.h: A "push button" interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef LUTBTNINTERFACE
#define LUTBTNINTERFACE
#include "SgDrawAreaInterface.h"

class Lut;

class LutBtnInterface : public SgDrawAreaInterface {

  public:
    
    LutBtnInterface ( Widget, Cmd*, Lut*, Lut*, Lut* );
    virtual ~LutBtnInterface ( ) { }
};
#endif
