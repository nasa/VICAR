/////////////////////////////////////////////////////////////////
// ZoomSpecialCmd.h
//
//	This class is a zoom control for managing a 
//	dialog with zoom commands.
////////////////////////////////////////////////////////////////
#ifndef ZOOMSPECIALCMD_H
#define ZOOMSPECIALCMD_H
#include "ZoomBaseCmd.h"

class CmdList;
class ZoomDialog;
class ZoomSpecialCmdList;

class ZoomSpecialCmd : public ZoomBaseCmd {

 protected:

   ZoomDialog *_zoomDialog;
   ZoomSpecialCmdList *_zoomSpecialCmdList;

   virtual void doit();
 public:

   ZoomSpecialCmd(const char *name, int active, CmdList *radioCmdList, 
			CmdValue startState, ZoomSpecialCmdList *zSCL);
   virtual ~ZoomSpecialCmd();

   virtual void undoZoom();

   virtual void setDialog(ZoomDialog *z) { _zoomDialog = z; }

   virtual const char *const className() { return ("ZoomSpecialCmd"); }
};
#endif

