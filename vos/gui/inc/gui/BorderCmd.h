/////////////////////////////////////////////////////////////
// BorderCmd.h: Turn the Motif border on and off for the shell
// ancestor of any BasicComponent subclass.
/////////////////////////////////////////////////////////////
#ifndef BORDERCMD_H
#define BORDERCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>

class BasicComponent;

class BorderCmd : public Cmd {
 private:
   Widget _shell;
   int _decorations;
   CmdValue _lastValue;		// for Undo

 protected:

   virtual void doit();   
   virtual void undoit(); 

 public:

   BorderCmd(const char *, int, BasicComponent *);
   void SetValue(CmdValue value) { _value = value; }
   virtual const char *const className () { return "BorderCmd"; }
};
#endif

