/////////////////////////////////////////////////////////
// SavePseudoFileCmd: A Command class that saves pseudocolor tables in  an
// IBIS file format.  The Command value is a dynamically allocated single
// string, suitable for passing in to a PseudoValue subclass.
/////////////////////////////////////////////////////////
#include "SavePseudoFileCmd.h"
#include "CmdInterface.h"
#include "PseudoValue.h"
#include "ErrorDialogManager.h"
#include <stdio.h>
#include <assert.h>

SavePseudoFileCmd::SavePseudoFileCmd(const char *name, int active, PseudoValue *pseudoValue)
		: NoUndoCmd(name, active)
{
   _pseudoValue = pseudoValue;
}

void SavePseudoFileCmd::doit()
{
   assert(_pseudoValue != NULL);

   int status = _pseudoValue->saveFile((char *)_value);

   if (status != 1) {
	char *msg = new char[1024];
	sprintf( msg, "Can not write to selected file\n%s", (char *)_value );
	theErrorDialogManager->post(msg);
	delete [] msg;
   }
}

