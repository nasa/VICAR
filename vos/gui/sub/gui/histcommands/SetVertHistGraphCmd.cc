///////////////////////////////////////////////////////////
// SetVertHistGraphCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetVertHistGraphCmd.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>

SetVertHistGraphCmd::SetVertHistGraphCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if (_menuView->getOrientType() == HORIZONTAL) {
       _value = (CmdValue) TRUE;
       newValue ();
     }
}

void SetVertHistGraphCmd::doit()
{

    if (_value) {
      _menuView->setOrientType( HORIZONTAL );
    }

}      

