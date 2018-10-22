//////////////////////////////////////////////////////////
// DitherCmd.cc: Radio Command Class to set the
//      XvicNditherMode resource to the
//      value stored in _ditherMode.  The dither mode is
//      is found from the option menu which is under the 
//      Preferences Menu
//////////////////////////////////////////////////////////
#include "DitherCmd.h"
#include "XvicBasicImage.h"
#include "ColorMapModes.h"
#include <iostream>
using namespace std;

DitherCmd::DitherCmd ( const char *name, int active, unsigned char ditherMode, 
  Widget widget, ColorMapModes *colorMapModes, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
  unsigned char value;

    _imageWidget = widget;
    _ditherMode = ditherMode;
    _colorMapModes = colorMapModes;

    XtVaGetValues( _imageWidget, XvicNditherMode, &value, NULL);

    if ( value == _ditherMode) setValue(TRUE);
    else setValue(FALSE);

}

void DitherCmd::doit()
{

  if (_value) {
    _colorMapModes->SetColorMapModes();
    _colorMapModes->SetColorMapButtons();
  }
}
