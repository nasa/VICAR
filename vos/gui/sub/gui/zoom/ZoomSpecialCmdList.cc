/////////////////////////////////////////////////////////////////
// ZoomSpecialCmdList.cc
/////////////////////////////////////////////////////////////////
#include "ZoomSpecialCmdList.h"
#include "ZoomCmd.h"
#include <Xm/ToggleB.h>

// We really want to trap addUnique(), but since it calls add() anyway,
// we can catch both birds with the same stone.

void ZoomSpecialCmdList::add(Cmd *cmd, CmdValue value)	// value = NULL
{
   CmdList::add(cmd, value);
   if (_button != NULL) {
      XmToggleButtonSetState(_button, False, False);
      XmToggleButtonSetState(_button, True, True);
   }
}

// Trap doit() so that we can save the zoom value for undo.  Yuck.

void ZoomSpecialCmdList::doit()
{
   int saveNumElements = _numElements;
   if (saveNumElements > 0) {
      _undoZoomCmd = NULL;

      for (int i=0; i<_numElements; i++) {
         if (strcmp(_contents[i]->className(), "ZoomCmd") == 0)
            _undoZoomCmd = (ZoomCmd *)_contents[i];
      }
   }

   CmdList::doit();

   if (saveNumElements > 0) {
      if (_undoZoomCmd)
         _undoZoom = *((ZoomFactor *)_undoZoomCmd->getValue());
   }

}

// If the current and previous commands are both Special zooms, we need to
// use the ZoomCmd's undo capability.  If the current is not Special, then
// we need to undo the zoom by executing the command with the saved value
// (we can't do that in the first case because the saved value is the *new*
// zoom factor.  yucky kludge).

void ZoomSpecialCmdList::undoZoom(Boolean useUndo)
{
   if (_undoZoomCmd) {
      if (useUndo) {
         _undoZoomCmd->undo();
         _undoZoom = _undoZoomCmd->getUndoZoom();
      }
      else
         _undoZoomCmd->execute(new ZoomFactor(_undoZoom));
   }
}

