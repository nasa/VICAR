/////////////////////////////////////////////////////////////////
// ZoomBaseCmd.cc - superclass for all Zoom radio buttons.  Needed in
// order for Undo of the special command to work in the dialog.  Kinda
// kludgy but it works.
////////////////////////////////////////////////////////////////
#include "ZoomBaseCmd.h"
#include "ZoomSpecialCmd.h"
 
ZoomBaseCmd::ZoomBaseCmd(const char *name, int active, CmdValue startState,
		CmdList *radioCmdList, ZoomSpecialCmd *zoomSpecialCmd)
	: RadioCmd(name, active, startState, radioCmdList)
{
   if (zoomSpecialCmd == NULL)
      _zoomSpecialCmd = (ZoomSpecialCmd *)this;		// We're the ZSC
   else
      _zoomSpecialCmd = zoomSpecialCmd;
}

void ZoomBaseCmd::undoit()
{
   if (_lastTrueCmd == _zoomSpecialCmd) {
      _zoomSpecialCmd->undoZoom();
   }

   RadioCmd::undoit();
}

