/////////////////////////////////////////////////////////
// SavePseudoFileCmd: A Command class that saves pseudocolor tables in  an 
// IBIS file format.  The Command value is a dynamically allocated single 
// string, suitable for passing in to a PseudoValue subclass.
/////////////////////////////////////////////////////////
#ifndef SAVESINGLEFILECMD_H
#define SAVESINGLEFILECMD_H
#include "NoUndoCmd.h"

class PseudoValue;

class SavePseudoFileCmd : public NoUndoCmd {

 protected:

   PseudoValue *_pseudoValue;
    
 public:

   SavePseudoFileCmd(const char *, int, PseudoValue *);

   virtual void doit();  
    
   virtual void freeValue(CmdValue value) { if (value) delete[] (char *)value; }

   virtual const char *const className () { return "SavePseudoFileCmd"; }
};
#endif
