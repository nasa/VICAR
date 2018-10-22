/////////////////////////////////////////////////////////////
// AxisCmd.h: Displays or hides histogram axis.
/////////////////////////////////////////////////////////////
#include "AxisCmd.h"
#include "HistBox.h"
#include <iostream>
#include <stdint.h>
using namespace std;

AxisCmd::AxisCmd ( const char *name, int active, HistBox *box ) 
	: Cmd ( name, active )
{
    _box = box;
    int value = (int) _box->AxisIsDisplayed ( );
    _value = (CmdValue) (uintptr_t) value; 
    newValue ( );
}

void AxisCmd::doit ( )
{
    _oldValue = _box->AxisIsDisplayed ( );
    _box->showAxis( (_value != 0) );
}      

void AxisCmd::undoit ( )
{
    _box->showAxis( _oldValue );
    _value = (CmdValue) ((uintptr_t) _oldValue);
    newValue ( );
}
