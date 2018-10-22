///////////////////////////////////////////////////////////
// SiHistShowStatsCmd.C:  
//////////////////////////////////////////////////////////
#include "SiHistShowStatCmd.h"
#include "SiHistBox.h"
#include <stdint.h>

SiHistShowStatCmd::SiHistShowStatCmd ( const char *name, int active,
							SiHistBox *box ) 
	: Cmd ( name, active )
{
    _box = box;
    _value = (CmdValue) (uintptr_t) _box->statIsDisplayed();; 
    newValue ();
}

void SiHistShowStatCmd::doit()
{
    _oldValue = _box->statIsDisplayed();
    _box->showStat( (_value != 0) );
}

void SiHistShowStatCmd::undoit()
{
    _box->showStat ( _oldValue );
    _value = (CmdValue) (uintptr_t) _oldValue;
    newValue();
}
