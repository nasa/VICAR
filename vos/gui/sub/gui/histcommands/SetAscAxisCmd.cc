///////////////////////////////////////////////////////////
// SetAscAxisCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetAscAxisCmd.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>
using namespace std;

SetAscAxisCmd::SetAscAxisCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if (_menuView->getVerAxisDirType() == ASC) {
      _value = ( CmdValue ) TRUE;
      newValue();
    }
}

void SetAscAxisCmd::doit()
{

  if (_value) {
      _menuView->setVerAxisDirType( ASC );
  }

}      

