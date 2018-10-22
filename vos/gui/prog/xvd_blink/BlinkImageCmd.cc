////////////////////////////////////////////////////////////////
// BlinkImageCmd.h:  Command class for any component within
// the BlinkControl; all it does is tell the BC to do its
// thing.  Used for simple creation of radio buttons.
////////////////////////////////////////////////////////////////

#include "BlinkImageCmd.h"
#include "BlinkControl.h"
#include <stdint.h>

BlinkImageCmd::BlinkImageCmd(char *name, int active,
		CmdList *radioList, BlinkControl *bc, int which)
	: RadioCmd(name, active, radioList)
{
    _bc = bc;
    _which = which;
}

void BlinkImageCmd::doit()
{
    if ((int)(uintptr_t)_value) {
	_bc->makeActiveWindow(_which);
    }
}

