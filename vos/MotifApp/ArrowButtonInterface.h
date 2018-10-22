//////////////////////////////////////////////////////////////
// ArrowButtonInterface.h: An ArrowButton interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef ARROWBUTTONINTERFACE
#define ARROWBUTTONINTERFACE
#include "CmdInterface.h"

class ArrowButtonInterface : public CmdInterface {
    
  public:
    
    ArrowButtonInterface ( Widget, Cmd * );
};
#endif
