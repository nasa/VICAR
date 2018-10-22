///////////////////////////////////////////////////////////
// HistCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "Application.h"
#include "Histogram.h"
#include "HistBox.h"
#include "HistCmd.h"
#include <iostream>
using namespace std;
#include <stdint.h>

HistCmd::HistCmd ( char *name, int active, HistBox *obj ) : Cmd ( name, active )
{

    _menuView = obj;
    int value = (int) _menuView->HistIsDisplayed();
    _value = (CmdValue) ((uintptr_t) value); 
    newValue ();
}

void HistCmd::doit()
{

    _oldValue = _menuView->HistIsDisplayed();
    _menuView->showHist( (_value != 0));

}      

void HistCmd::undoit()
{
    // Just print a message that allows us to trace the execution
    _menuView->showHist( _oldValue);
    _value = (CmdValue) ((uintptr_t) _oldValue);
    newValue();
}       

