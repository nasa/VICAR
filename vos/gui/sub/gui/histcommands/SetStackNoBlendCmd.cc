///////////////////////////////////////////////////////////
// SetStackNoBlendCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetStackNoBlendCmd.h"
#include "Application.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>

SetStackNoBlendCmd::SetStackNoBlendCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if (_menuView->getMethodType() == STACKED  || _menuView->getHistB() == NULL ) {
      _value = ( CmdValue ) TRUE;
      newValue();
    }
}

void SetStackNoBlendCmd::doit()
{
    if (_value) {
       _menuView->setMethodType ( STACKED );
     }
}      



