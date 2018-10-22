////////////////////////////////////////////////////////////
// BorderCmd.cc: Turn the Motif border on and off for the shell
// ancestor of any BasicComponent subclass.
///////////////////////////////////////////////////////////
#include "BorderCmd.h"
#include "BasicComponent.h"
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>

BorderCmd::BorderCmd(const char *name, int active, BasicComponent *component)
		: Cmd(name, active)
{
   // Find the shell widget for this component.

   _shell = component->baseWidget();
   if (_shell && !XtIsShell(_shell)) {
      do {
         _shell = XtParent(_shell);
      } while (_shell && !XtIsShell(_shell));
   }

   if (_shell == NULL)
      return;				// huh?

   // Now check and save the current state of the borders.  If anything is
   // on, we assume that's the set the user wants when borders are enabled.
   // If nothing is on, we assume that borders are off and set up to display
   // all when turned back on.

   XtVaGetValues(_shell, XmNmwmDecorations, &_decorations, NULL);

   if (_decorations == 0) {
      _value = (CmdValue) False;
      _decorations = (int)MWM_DECOR_ALL;
   }
   else {
      _value = (CmdValue) True;
   }
   _lastValue = _value;
   newValue();
}

void BorderCmd::doit()
{
   int decor;

   // Save previous state for undo

   XtVaGetValues(_shell, XmNmwmDecorations, &decor, NULL);
   if (decor == 0)
      _lastValue = (CmdValue) False;
   else
      _lastValue = (CmdValue) True;

   if (_shell == NULL)
      return;			// huh?

   // We should be able to simply set XmNmwmDecorations and be done with it.
   // Alas, this is not enough, because mwm is too stupid to notice
   // _MOTIF_WM_HINTS property changes.  So, we must unmap and remap
   // the window instead, since it will notice when it receives a
   // MappingNotify event.  We must check the map state first so we
   // don't e.g. uniconify a window just by mapping it!
   // Also, if the window is iconified, mwm is too stupid to set the
   // state correctly when it is uniconified.  Holy cow.  So, we simply
   // don't allow the state to be changed in that case.  A little weird
   // but what else are you supposed to do about Motif bugs?

   if (XtIsRealized(_shell)) {

      XWindowAttributes attr;
      XGetWindowAttributes(XtDisplay(_shell), XtWindow(_shell), &attr);
      if (attr.map_state == IsViewable) {

         if (_value)				// Turn the border on
            XtVaSetValues(_shell, XmNmwmDecorations, _decorations, NULL);
         else					// Turn the border off
            XtVaSetValues(_shell, XmNmwmDecorations, 0, NULL);

         XtUnmapWidget(_shell);
         XtMapWidget(_shell);
      }
      else {
         _value = _lastValue;		// It's iconic, don't allow the change
         newValue();
      }
   }
}

void BorderCmd::undoit()
{
   _value = _lastValue;
   newValue();
   doit();
}

