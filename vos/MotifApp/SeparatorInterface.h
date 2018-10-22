//////////////////////////////////////////////////////////////
// SeparatorInterface.h: A separator interface to a (dummy) Cmd object
///////////////////////////////////////////////////////////////
#ifndef SEPARATORINTERFACE_H
#define SEPARATORINTERFACE_H
#include "CmdInterface.h"

class SeparatorInterface : public CmdInterface {
    
  public:
    
    SeparatorInterface ( Widget, Cmd * );
};
#endif
