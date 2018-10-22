////////////////////////////////////////////////////////////////
// SiCursorStretchEnableCmd - Enable or disable the cursor stretch.
// This command has a most unusual Undo... undo does not turn the
// command back on, it ensures that the command is off then resets
// the stretch back to what it was when it was turned on.
////////////////////////////////////////////////////////////////

#ifndef SICURSORSTRETCHENABLECMD_H
#define SICURSORSTRETCHENABLECMD_H
#include "Cmd.h"

class SiCursorStretchInterface;
class StretchValue;

class SiCursorStretchEnableCmd : public Cmd {

 protected:

   SiCursorStretchInterface *_interface;
   StretchValue *_saveStretchForUndo;
   Cmd *_stretchCmd;			// used for undo

 public:

   SiCursorStretchEnableCmd(const char *name, int active,
			SiCursorStretchInterface *csif, Cmd *stretchCmd);
   virtual ~SiCursorStretchEnableCmd();

   virtual void doit();
   virtual void undoit();

};
#endif
