///////////////////////////////////////////////////////////
// MenuBarCmd.cc: Creates the MenuBar Command for turning a
//                MenuWindow's menu bar on and off
///////////////////////////////////////////////////////////
#include "MenuBarCmd.h"
#include "MenuWindow.h"
#include <stdint.h>

MenuBarCmd::MenuBarCmd(const char *name, int active, MenuWindow *obj,
			Boolean initial) // = True
	: Cmd(name, active)
{
   // Save the menu window and set the initial state.  We can't just ask the
   // window for its state because it might not be managed yet when this
   // command is created!

   _menuWindow = obj;
   int value = (int)initial;
   _value = (CmdValue) (uintptr_t) value;
   newValue();

}

void MenuBarCmd::doit()
{
   // Save the old value for the undoit command and change to
   //  the new value;

   _oldValue = _menuWindow->isMenuBarVisible();
   if (_value)
      _menuWindow->showMenuBar();
   else
      _menuWindow->hideMenuBar();
}      

void MenuBarCmd::undoit()
{

   // Undo the command to the last state

   if (_oldValue)
      _menuWindow->showMenuBar();
   else
      _menuWindow->hideMenuBar();
   _value = (CmdValue) (uintptr_t) _oldValue;
   newValue();

}       

