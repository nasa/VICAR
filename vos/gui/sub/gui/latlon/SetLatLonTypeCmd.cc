/////////////////////////////////////////////////////////////
// SetLatLonTypeCmd.cc: 
/////////////////////////////////////////////////////////////
#include "SetLatLonTypeCmd.h"
//#include <iostream>
using namespace std;

#include "CursorLatLonView.h"

SetLatLonTypeCmd::SetLatLonTypeCmd ( const char *name, int active,
				     CursorLatLonView *cursorView,
				     LatLonType latLonType, 
				     CmdList *list=NULL )
	: RadioCmd ( name, active, list )
{
    _cursorView = cursorView;
    _latLonType = latLonType;

    LatLonType value = (LatLonType) _cursorView->getLatLonType();
    if (value == _latLonType)
      _value = (CmdValue) TRUE;
    else
      _value = (CmdValue) FALSE;
    newValue ( );
}

void SetLatLonTypeCmd::doit ( )
{

  if (!_value) return;

  LatLonType type = (LatLonType) _cursorView->getLatLonType();
  if (type != _latLonType) 
    _cursorView->setLatLonType(_latLonType);
}      
