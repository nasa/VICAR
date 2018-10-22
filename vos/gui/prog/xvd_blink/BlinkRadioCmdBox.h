//////////////////////////////////////////////////////////////
// BlinkRadioCmdBox.h: A RadioCmdBox that allows us to get pointers
// to the components that make up the box.  This allows e.g. labels
// to be changed.
///////////////////////////////////////////////////////////////
#ifndef BLINKRADIOCMDBOX_H
#define BLINKRADIOCMDBOX_H
#include "RadioCmdBox.h"

#include "RadioButtonInterface.h"

class BlinkRadioCmdBox : public RadioCmdBox {

  public:
    
    BlinkRadioCmdBox(Widget w, const char *name, CmdList *list, CmdList *def=0 )
		: RadioCmdBox(w, name, list, def) { }

    CmdInterface *getInterface(int w) { return _list[w]; }

};
#endif
