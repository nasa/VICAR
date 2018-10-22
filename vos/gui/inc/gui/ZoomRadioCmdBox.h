//////////////////////////////////////////////////////////////
// ZoomRadioCmdBox.h - Just like RadioCmdBox but adds capability to
// find one interface given the command it's attached to.
//////////////////////////////////////////////////////////////
#ifndef ZOOMRADIOCMDBOX_H
#define ZOOMRADIOCMDBOX_H
#include "RadioCmdBox.h"
#include <Xm/Xm.h>
 
class Cmd;
class CmdList;

class ZoomRadioCmdBox : public RadioCmdBox {

 protected:

   CmdList *_cmdList;
 
 public:

   ZoomRadioCmdBox(Widget w, const char *name, CmdList *cmdList,
							CmdList *deferList)
    			: RadioCmdBox(w, name, cmdList, deferList)
		{ _cmdList = cmdList; }

   virtual Widget getInterfaceWidget(Cmd *cmd);
 
};
#endif
