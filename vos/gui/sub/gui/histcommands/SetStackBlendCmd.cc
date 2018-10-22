///////////////////////////////////////////////////////////
// SetStackBlendCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetStackBlendCmd.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>

SetStackBlendCmd::SetStackBlendCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if ( _menuView->getMethodType()== BLEND || _menuView->getHistB() == NULL ) {
      _value = ( CmdValue ) TRUE;
      newValue();
    }      
}

void SetStackBlendCmd::doit()
{
    if (_value) {
       _menuView->setMethodType( BLEND );
    }
}      


