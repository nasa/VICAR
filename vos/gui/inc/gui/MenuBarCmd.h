/////////////////////////////////////////////////////////////
// MenuBarCmd.h: Include file for turning a MenuWindow's menu bar
//                on and off
/////////////////////////////////////////////////////////////
#ifndef MENUBARCMD_H
#define MENUBARCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>		// only for Boolean!

class MenuWindow;

class MenuBarCmd : public Cmd {
 private:
   int _oldValue;    // Last valid command for Undo
   MenuWindow *_menuWindow;
 protected:

   virtual void doit();   
   virtual void undoit(); 

 public:

   MenuBarCmd(const char *, int, MenuWindow *, Boolean=True);
   void SetValue(CmdValue value) { _value = value; }
   virtual const char *const className () { return "MenuBarCmd"; }
};
#endif

