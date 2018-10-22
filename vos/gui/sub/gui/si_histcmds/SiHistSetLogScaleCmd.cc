/////////////////////////////////////////////////////////////
// SiHistSetLogScaleCmd.h: Displays histogram in a logarithmic scale.
/////////////////////////////////////////////////////////////
#include "SiHistSetLogScaleCmd.h"
#include "SiHistBox.h"
#include <stdint.h>

SiHistSetLogScaleCmd::SiHistSetLogScaleCmd ( const char *name, int active, SiHistBox *box ) 
	: Cmd ( name, active )
{
    _box = box;
    _value = (CmdValue) (uintptr_t) _box->logScaleIsSet(); 
    newValue();
}

void SiHistSetLogScaleCmd::doit()
{
    _oldValue = _box->logScaleIsSet();
    _box->setLogScale ( (_value != 0) );
}

void SiHistSetLogScaleCmd::undoit()
{
    _box->setLogScale ( _oldValue );
    _value = (CmdValue) (uintptr_t) _oldValue;
    newValue();
}
