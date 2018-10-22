////////////////////////////////////////////////////////////////////////
// LoadMenuCmd.h: Handle posting the File dialog box.
////////////////////////////////////////////////////////////////////////
#include "LoadMenuCmd.h"
#include "FileSelWindow.h"

LoadMenuCmd::LoadMenuCmd(const char *name, int active, Cmd *loadFileCmd)
		: NoUndoCmd ( name, active )
{
   _loadFileCmd = loadFileCmd;

   // Dialog is created here rather than when needed so that the SAGE
   // communication (if any) will get set up regardless of whether the
   // dialog is displayed or not.

   _fileSelWindow = new FileSelWindow("FileSelWindow", _loadFileCmd);
   _fileSelWindow->initialize();
}

void LoadMenuCmd::doit()
{
   _fileSelWindow->manage();
}      

