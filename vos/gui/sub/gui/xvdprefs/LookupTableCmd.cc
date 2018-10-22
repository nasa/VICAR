///////////////////////////////////////////////////////////
// LookupTableCmd.cc: Radio Command Class to set the
//      XvicNstretchPolicy resource to the
//      value stored in _LUTMode.  The LUT mode is
//      is found from the option menu which is under the 
//      Preferences Menu
//////////////////////////////////////////////////////////
#include "ColorMapModes.h"
#include "LookupTableCmd.h"
#include "XvicBasicImage.h"
#include <iostream>
using namespace std;

LookupTableCmd::LookupTableCmd ( const char *name, int active,
  unsigned char LUTMode, 
  Widget widget, ColorMapModes * colorMap, CmdList *list=NULL ) 
  : RadioCmd ( name, active, list )
{
  unsigned char value;

    _imageWidget = widget;
    _LUTMode = LUTMode;
    _colorMapModes = colorMap;

    XtVaGetValues( _imageWidget, XvicNstretchPolicy, &value, NULL);
    if ( value == _LUTMode) {
      _value = ( CmdValue ) TRUE;
      newValue();
    } else {
      _value = ( CmdValue ) FALSE;
      newValue();
    }
}

void LookupTableCmd::doit()
{
  unsigned char policy;

  if (_value) {
    XtVaGetValues( _imageWidget, XvicNstretchPolicy, &policy, NULL);
    if ( policy != _LUTMode ) {
       XtVaSetValues( _imageWidget, XvicNstretchPolicy, _LUTMode, NULL );
       _colorMapModes->SetStretchPolicy( _LUTMode );
       _colorMapModes->SetColorMapButtons();
     }
  }
}      


