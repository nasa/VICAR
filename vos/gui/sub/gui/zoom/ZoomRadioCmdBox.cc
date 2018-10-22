//////////////////////////////////////////////////////////////
// ZoomRadioCmdBox.cc - Just like RadioCmdBox but adds capability to
// find one interface given the command it's attached to.
//////////////////////////////////////////////////////////////
#include "ZoomRadioCmdBox.h"
#include "CmdList.h"
#include "RadioButtonInterface.h"

Widget ZoomRadioCmdBox::getInterfaceWidget(Cmd *cmd)
{    
   for (int i = 0; i < _numElements; i++) {
      if (cmd == (*_cmdList)[i]) 
         return (_list[i]->baseWidget());
   }
   return NULL;		// shouldn't happen
}

