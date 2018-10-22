/////////////////////////////////////////////////////////
// SaveLutFileCmd: A Command class that saves pseudocolor tables in  an 
// IBIS file format.  The Command value is a dynamically allocated single 
// string.
/////////////////////////////////////////////////////////
#ifndef SAVELUTFILECMD_H
#define SAVELUTFILECMD_H
#include "NoUndoCmd.h"

class Lut;

class SaveLutFileCmd : public NoUndoCmd {

 protected:

    Lut *_lutRed, *_lutGrn, *_lutBlu;
    
 public:

   SaveLutFileCmd(const char *, int, Lut *lutRed, Lut *lutGrn, Lut *lutBlu);

   virtual void doit();  
    
   virtual void freeValue(CmdValue value) 
       { if (value) delete [] (char *)value; }

   virtual const char *const className () { return "SaveLutFileCmd"; }
};
#endif
