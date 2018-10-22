///////////////////////////////////////////////////////////
// SiHistSpikeCmd.C:  Set histogram spike value.
//////////////////////////////////////////////////////////
#include "SiHistSpikeCmd.h"
#include "SiHistBox.h"
#include <stdint.h>

SiHistSpikeCmd::SiHistSpikeCmd ( const char *name, int active, SiHistBox *box ) 
	: Cmd ( name, active)
{
    _box = box;
    _value = (CmdValue) (uintptr_t) _box->getSpike();
    newValue();
}

void SiHistSpikeCmd::doit()
{
    _oldValue = _box->getSpike();
    _box->setSpike( (int) (uintptr_t)_value );
}

void SiHistSpikeCmd::undoit()
{
    _value = (CmdValue) (uintptr_t) _oldValue;
    _box->setSpike( (int) (uintptr_t) _value );
    newValue();
}       
