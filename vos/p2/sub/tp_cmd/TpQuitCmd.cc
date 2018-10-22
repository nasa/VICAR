/////////////////////////////////////////////////////////////////////////////
// TpQuitCmd.cc: Exit the application if there is no tiepoint file to save.
// Otherwise execute TpCloseCmd and provide it with 'this' pointer so that
// TpCloseCmd object could call us again when it is done with closing project.
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher - July 1997      JPL
//////////////////////////////////////////////////////////////////////////////
#include "TpQuitCmd.h"
#include "TpMatchManager.h"
#include "TpCloseCmd.h"
#include "TpApplication.h"

TpQuitCmd::TpQuitCmd(const char *name, int active, TpMatchManager *mm) 
	: NoUndoCmd(name, active)
{
    _matchManager = mm;
}

void TpQuitCmd::doit()
{
    if (_matchManager->isDirty()) {
	Cmd *saveCmd = new TpCloseCmd ("close", True, 
				       _matchManager, (Cmd *)this);
	saveCmd->execute();
    }
    else {
	exit(theTpApplication->getExitStatus());
    }
}

