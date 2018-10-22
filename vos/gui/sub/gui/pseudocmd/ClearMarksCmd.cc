/////////////////////////////////////////////////////////
// ClearMarksCmd: A Command class that clears all the marks off
// the screen except for those with DN 0 and 255.
/////////////////////////////////////////////////////////
#include "ClearMarksCmd.h"
#include "CmdInterface.h"
#include "PseudoMarks.h"

ClearMarksCmd::ClearMarksCmd(const char *name, int active, 
		PseudoMarks *pseudoMarks)
		: NoUndoCmd(name, active)
{
   _pseudoMarks = pseudoMarks;
}

void ClearMarksCmd::doit()
{
	_pseudoMarks->clearMarks();
}

