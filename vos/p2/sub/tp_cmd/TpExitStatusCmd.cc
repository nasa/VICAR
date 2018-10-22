/////////////////////////////////////////////////////////////////////////////
// TpExitStatusCmd.cc: Set application exit status.
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher - July 1997      JPL
//////////////////////////////////////////////////////////////////////////////
#include "TpExitStatusCmd.h"
#include "TpApplication.h"

TpExitStatusCmd::TpExitStatusCmd(const char *name, int active) 
	: NoUndoCmd(name, active)
{
    // Empty
}

void TpExitStatusCmd::doit()
{
    if (_value)
	theTpApplication->setExitStatus(1);
    else 
	theTpApplication->setExitStatus(0);
}

