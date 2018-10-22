/////////////////////////////////////////////////////////////////
// ZoomSpecialCmdList.h - Command list that maintains the desired
// "special" zoom factor.  The keyins for the zoom factor are deferred
// to this list, which is executed by ZoomSpecialCmd.
/////////////////////////////////////////////////////////////////
#ifndef ZOOMSPECIALCMDLIST_H
#define ZOOMSPECIALCMDLIST_H
#include "CmdList.h"
#include "ZoomFactor.h"
#include <Xm/Xm.h>

class ZoomCmd;

class ZoomSpecialCmdList : public CmdList {
 protected:

   Widget _button;			// radiobutton in dialog.

   ZoomFactor _undoZoom;		// Last zoom factor, used for undo
   ZoomCmd *_undoZoomCmd;		// Zoom command, used for undo

   virtual void doit();

 public:
   ZoomSpecialCmdList() : CmdList()
			{ _button = NULL; _undoZoomCmd = NULL; }
   ZoomSpecialCmdList(const char *name) : CmdList(name)
			{ _button = NULL; _undoZoomCmd = NULL; }

   virtual ~ZoomSpecialCmdList() { };

   void setInterfaceWidget(Widget w) { _button = w; }

   // Override CmdList functions to add setting the toggle button
   virtual void add(Cmd *cmd, CmdValue value = NULL);

   virtual void undoZoom(Boolean);

};
#endif

