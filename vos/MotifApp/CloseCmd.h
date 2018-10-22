////////////////////////////////////////////////////////////
// CloseCmd.h: Exit an application without checking with user.
////////////////////////////////////////////////////////////
#ifndef CLOSECMD_H
#define CLOSECMD_H
#include "NoUndoCmd.h"
#include <stdlib.h>

class CloseCmd : public NoUndoCmd {
    
  protected:
    
    virtual void doit() { exit ( 0 ); }
    
  public:
    
    CloseCmd ( const char *name, int active ) 
		: NoUndoCmd ( name, active ) { }

    virtual const char *const className () { return "CloseCmd"; }
};
#endif
