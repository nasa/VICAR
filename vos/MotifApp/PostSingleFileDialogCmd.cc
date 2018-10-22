////////////////////////////////////////////////////////////////////////
// PostSingleFileDialogCmd.cc:  Handles posting the File dialog box.  The 
// caller must supply a command class object loadFileCmd
// that takes dynamically allocated string filename
// as a CmdValue.  The command is expected to free the string in 
// freeValue() using XtFree.
////////////////////////////////////////////////////////////////////////
#include "PostSingleFileDialogCmd.h"
#include "SingleFileSelWindow.h"

PostSingleFileDialogCmd::PostSingleFileDialogCmd(const char *name, int active, 
		Cmd *loadFileCmd) : NoUndoCmd ( name, active )
{
    _created = FALSE;
    _loadFileCmd = loadFileCmd;
}

void PostSingleFileDialogCmd::doit()
{
    // Create dialog window only once and then display it.

    if (!_created) {                 // Do only once
        _fileSelWindow = new SingleFileSelWindow((char *)name(), _loadFileCmd);
        _fileSelWindow->initialize();

        _created = TRUE;
    }

    _fileSelWindow->manage();
}      

