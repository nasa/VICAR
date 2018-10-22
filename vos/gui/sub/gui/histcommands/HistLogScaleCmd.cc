/////////////////////////////////////////////////////////////
// HistLogScaleCmd.h: Displays or hides histogram axis.
/////////////////////////////////////////////////////////////
#include "HistLogScaleCmd.h"
#include "HistBox.h"
#include <iostream>
#include <stdint.h>

HistLogScaleCmd::HistLogScaleCmd ( const char *name, int active, HistBox *box ) 
	: Cmd ( name, active )
{
    _box = box;
    int value = (int) _box->logScaleIsSet ( );
    _value = (CmdValue) ((uintptr_t) value); 
    newValue ( );
}

void HistLogScaleCmd::doit ( )
{
    _oldValue = _box->logScaleIsSet ( );
    _box->setLogScale( (_value != 0) );
}

void HistLogScaleCmd::undoit ( )
{
    _box->setLogScale( _oldValue );
    _value = (CmdValue)((uintptr_t)  _oldValue);
    newValue ( );
}
