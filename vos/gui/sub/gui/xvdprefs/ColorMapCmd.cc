///////////////////////////////////////////////////////////
// ColorMapCmd.cc: Radio Command Class to set the
//      XvicNcolormapPolicy  resource to the
//      value stored in _colorMapMode.  The dither mode is
//      is found from the option menu which is under the 
//      Preferences Menu
//////////////////////////////////////////////////////////
#include "ColorMapCmd.h"
#include "ColorMapModes.h"
#include "XvicBasicImage.h"
#include <iostream>
using namespace std;
#include <stdio.h>

ColorMapCmd::ColorMapCmd ( const char *name, int active,
   unsigned char colorMapValue,
   Widget widget, ColorMapModes *colorMapModes, CmdList *list=NULL ) 
  : RadioCmd ( name, active, list )
{
  unsigned char policy;

    _imageWidget = widget;
    _colorMapValue = colorMapValue;
    _colorMapModes = colorMapModes;

    XtVaGetValues( _imageWidget, XvicNcolormapPolicy, &policy, NULL);
 
    switch(policy) {
      case XvicFULL: 
      case XvicFULL_COLOR: if ( _colorMapValue==XvicFULL || 
			       _colorMapValue==XvicFULL_COLOR) setValue(TRUE);
			   else setValue(FALSE);
			   break;
      case XvicHALF:       if ( _colorMapValue==XvicHALF) setValue(TRUE);
			   else setValue(FALSE);
			   break;
      case XvicDITHER: 
      case XvicALLOC:      if ( _colorMapValue==XvicDITHER ||
			       _colorMapValue==XvicALLOC ) setValue(TRUE);
			   else setValue(FALSE);
			   break;
      default:             printf("Unknown ColorMapPolicy %d\n",policy);
    }

}

void ColorMapCmd::doit()
{

//  unsigned char value;


  if (_value) {
    _colorMapModes->SetColorMapModes();
    _colorMapModes->SetColorMapButtons();
  }
}      

