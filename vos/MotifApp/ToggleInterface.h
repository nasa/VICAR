//////////////////////////////////////////////////////////////
// ToggleInterface.h: A toggle button interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef TOGGLEINTERFACE_H
#define TOGGLEINTERFACE_H
#include "CmdInterface.h"
#include "Cmd.h"

class ToggleInterface : public CmdInterface {

  protected:

    virtual void executeCmd( XtPointer );

  public:
    
    ToggleInterface ( Widget, Cmd *, int );
    void setDefault(Boolean);
};
#endif
