////////////////////////////////////////////////////////////////
// FramedButtonInterface.h: A push button interface with a frame
// around it.  The frame does not "push in"; it is used to get
// different visual effects.
////////////////////////////////////////////////////////////////
#ifndef FRAMEDBUTTONINTERFACE_H
#define FRAMEDBUTTONINTERFACE_H

#include "CmdInterface.h"

class FramedButtonInterface : public CmdInterface {
  protected:
    Widget _pushB;
    
  public:
    
    FramedButtonInterface(Widget, Cmd *, unsigned char frameType);
    ~FramedButtonInterface() { }

    void setButtonLabel(char *);
    Widget buttonWidget() { return _pushB; }
};
#endif
