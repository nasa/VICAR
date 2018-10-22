///////////////////////////////////////////////////////////
// StatsCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "Application.h"
#include "Histogram.h"
#include "HistBox.h"
#include "StatsCmd.h"
#include <iostream>
#include <stdint.h>

StatsCmd::StatsCmd ( const char *name, int active, HistBox *obj ) : Cmd ( name, active )
{

    _menuView = obj;
    int value = (int) _menuView->StatIsDisplayed();
    _value = (CmdValue) (uintptr_t) value; 
    newValue ();
}

void StatsCmd::doit()
{

    _oldValue = _menuView->StatIsDisplayed();
    _menuView->showStat(  (_value != 0));

}      

void StatsCmd::undoit()
{
    // Just print a message that allows us to trace the execution
    _menuView->showStat( _oldValue);
    _value = (CmdValue) (uintptr_t) _oldValue;
    newValue();
}       














