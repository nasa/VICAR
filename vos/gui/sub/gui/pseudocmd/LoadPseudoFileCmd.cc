/////////////////////////////////////////////////////////
// LoadPseudoFileCmd: A Command class that loads a IBIS file.  The Command
// value is a dynamically allocated single string, suitable for passing in
// to a PseudoValue subclass.
/////////////////////////////////////////////////////////
#include "LoadPseudoFileCmd.h"
#include "CmdInterface.h"
#include "PseudoValue.h"
#include "ErrorDialogManager.h"
#include <stdio.h>
#include <assert.h>

LoadPseudoFileCmd::LoadPseudoFileCmd(const char *name, int active, 
		PseudoValue *pseudoValue, char *filename)
		: NoUndoCmd(name, active)
{
   _filename = filename;
   _pseudoValue = pseudoValue;
}

void LoadPseudoFileCmd::doit()
{
   assert(_pseudoValue != NULL);

   int status;

   if (_filename) 
	status = _pseudoValue->loadFile(_filename);
   else 
	status = _pseudoValue->loadFile((char *)_value);

   if (status != 1) {
	_pseudoValue->linear(0, 255, 0, 255, 0, 255); // Ramp the tables
	_pseudoValue->setDefRedAsArray(_pseudoValue->getRedAsArray());
	_pseudoValue->setDefGrnAsArray(_pseudoValue->getGrnAsArray());
	_pseudoValue->setDefBluAsArray(_pseudoValue->getBluAsArray());
	char msg[1024];
	if (_filename)
	   sprintf( msg, "Can not process selected file\n%s", _filename );
	else 
	   sprintf( msg, "Can not process selected file\n%s", (char *)_value );
	theErrorDialogManager->post(msg);
   }
}

