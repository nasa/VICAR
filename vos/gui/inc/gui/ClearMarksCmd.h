/////////////////////////////////////////////////////////
// ClearMarksCmd.h.
/////////////////////////////////////////////////////////
#ifndef CLEARMARKSCMD_H
#define CLEARMARKSCMD_H
#include "NoUndoCmd.h"

class PseudoMarks;

class ClearMarksCmd : public NoUndoCmd {

 protected:

   PseudoMarks *_pseudoMarks;
    
 public:

   ClearMarksCmd(const char *, int, PseudoMarks *);

   virtual void doit();  
    
   virtual const char *const className () { return "ClearMarksCmd"; }
};
#endif
