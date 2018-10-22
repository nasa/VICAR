/////////////////////////////////////////////////////////////////
// PseudoModeCmd.h
////////////////////////////////////////////////////////////////
#ifndef PSEUDOMODECMD_H
#define PSEUDOMODECMD_H
#include "Cmd.h"
#include <Xm/Xm.h>

class PseudoModeCmd : public Cmd {

 protected:

   Widget _iw;

   virtual void doit();
   virtual void undoit();

 public:

   PseudoModeCmd(const char *name, int active, Widget iw);

   virtual const char *const className() { return ("PseudoModeCmd"); }

};
#endif
