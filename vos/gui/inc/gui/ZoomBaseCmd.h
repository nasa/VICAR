/////////////////////////////////////////////////////////////////
// ZoomBaseCmd.h - superclass for all Zoom radio buttons.  Needed in
// order for Undo of the special command to work in the dialog.  Kinda
// kludgy but it works.
////////////////////////////////////////////////////////////////
#ifndef ZOOMBASECMD_H
#define ZOOMBASECMD_H
#include "RadioCmd.h"
 
class ZoomSpecialCmd;

class ZoomBaseCmd :  public RadioCmd  {
 
 protected:

   ZoomSpecialCmd *_zoomSpecialCmd;
 
   virtual void	undoit();
 
 public:

   ZoomBaseCmd(const char *, int, CmdValue, CmdList *, ZoomSpecialCmd *);

   virtual const char *const className() { return ("ZoomBaseCmd"); }
 
};
#endif

