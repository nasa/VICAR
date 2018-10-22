/////////////////////////////////////////////////////////////
// SiHistShowAxisCmd.h: Displays or hides histogram axis.
/////////////////////////////////////////////////////////////
#include "SiHistShowAxisCmd.h"
#include "SiHistBox.h"
#include <stdint.h>

SiHistShowAxisCmd::SiHistShowAxisCmd ( const char *name, int active,
							SiHistBox *box )
	: Cmd ( name, active )
{
    _box = box;
    _value = (CmdValue) (uintptr_t) _box->axisIsDisplayed(); 
    newValue();
}

void SiHistShowAxisCmd::doit()
{
    _oldValue = _box->axisIsDisplayed();
    _box->showAxis ( (_value != 0) );
}

void SiHistShowAxisCmd::undoit()
{
    _box->showAxis ( _oldValue );
    _value = (CmdValue) (uintptr_t) _oldValue;
    newValue();
}
