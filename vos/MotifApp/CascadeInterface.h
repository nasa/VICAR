//////////////////////////////////////////////////////////////
// CascadeInterface.h: A cascade button interface that hooks to a
// MenuCmdList object.  The Cmd object *MUST* be a MenuCmdList;
// the compiler does not check this!
///////////////////////////////////////////////////////////////

#ifndef CASCADEINTERFACE
#define CASCADEINTERFACE
#include "CmdInterface.h"

class CascadeInterface : public CmdInterface {
    
  public:
    
    CascadeInterface ( Widget, Cmd * );
};
#endif
