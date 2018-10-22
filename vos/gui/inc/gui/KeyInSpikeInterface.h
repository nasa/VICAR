//////////////////////////////////////////////////////////////
// KeyInSpikeInterface.h: An Keyin interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef KEYINSPIKEINTERFACE
#define KEYINSPIKEINTERFACE
#include "CmdInterface.h"
#include <Xm/Xm.h>

class KeyinView;

class KeyInSpikeInterface : public CmdInterface {
  protected:

    KeyinView *_spike;
    CmdValue  *_init;
    
    virtual KeyinView *addOneSubView(Widget parent, const char *name);
    virtual void executeCmd(XtPointer);
    virtual void setValue(CmdValue);

  public:
    
    KeyInSpikeInterface ( Widget, Cmd * );
    ~KeyInSpikeInterface();
};
#endif
