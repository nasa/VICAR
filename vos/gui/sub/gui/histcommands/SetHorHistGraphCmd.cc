///////////////////////////////////////////////////////////
// SetHorHistGraphCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetHorHistGraphCmd.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>
using namespace std;

SetHorHistGraphCmd::SetHorHistGraphCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if (_menuView->getOrientType() == VERTICAL) {
       _value = (CmdValue) TRUE;
       newValue ();
     }
}

void SetHorHistGraphCmd::doit()
{

    if (_value) {
      _menuView->setOrientType( VERTICAL );
    }

}      

