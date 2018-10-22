///////////////////////////////////////////////////////////
// SpikeCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SpikeCmd.h"
#include "HistBox.h"
#include "Application.h"
#include <iostream>
#include <stdint.h>

SpikeCmd::SpikeCmd ( const char *name, int active, HistBox *obj ) : Cmd ( name, active)
{
    _histBox = obj;
    _value = (CmdValue)(uintptr_t)  _histBox->getSpike();
}

void SpikeCmd::doit()
{

    _prevValue = _histBox->getSpike();
    _histBox->setSpike((int) (uintptr_t)_value );

}      

void SpikeCmd::undoit()
{  
    _value = (CmdValue) (uintptr_t) _prevValue;
    _histBox->setSpike((int) (uintptr_t)_value );
    newValue();
}       


