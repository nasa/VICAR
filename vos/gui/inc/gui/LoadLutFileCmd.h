/////////////////////////////////////////////////////////
// LoadLutFileCmd: A Command class that loads an IBIS file.  The Command 
// value is a dynamically allocated single string.
/////////////////////////////////////////////////////////
#ifndef LOADSINGLEFILECMD_H
#define LOADSINGLEFILECMD_H
#include "NoUndoCmd.h"

class Cmd;

class LoadLutFileCmd : public NoUndoCmd {

  protected:

    Cmd *_stretchCmd;
    
  public:
    
    LoadLutFileCmd(const char *, int, Cmd *);
    
    virtual void doit();
    
    virtual void freeValue(CmdValue value) 
	{ if (value) delete [] (char *)value; }
    
    virtual const char *const className () { return "LoadLutFileCmd"; }
};
#endif
