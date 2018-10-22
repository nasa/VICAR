///////////////////////////////////////////////////////////
// SetDescAxisCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetDescAxisCmd.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>
using namespace std;

SetDescAxisCmd::SetDescAxisCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if ( _menuView->getVerAxisDirType() == DESC) {
      _value = ( CmdValue ) TRUE;
      newValue();
    }
}

void SetDescAxisCmd::doit()
{

    if (_value ) {
      _menuView->setVerAxisDirType( DESC );
    }

}      

