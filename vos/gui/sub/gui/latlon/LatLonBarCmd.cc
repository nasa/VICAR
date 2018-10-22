////////////////////////////////////////////////////////////
// LatLonBarCmd.cc: Creates the LatLonBar Command for managing
//                whether the LatLonBar is displayed or not
//                from the dialog check box in Preferences
///////////////////////////////////////////////////////////
#include "LatLonBarCmd.h"
#include "ImageDisplayer.h"
#include <iostream>
using namespace std;
#include <stdint.h>


LatLonBarCmd::LatLonBarCmd( const char *name, int active, ImageDisplayer *obj )
                                                       : Cmd ( name, active )
{
  // Save the image view and determine the current state to set
  //  the check box appropriately.
  
  _imageView = obj;
  int value = (int) _imageView->IsLatLonBarDisplayed();
  _value = (CmdValue) (uintptr_t) value;
  newValue();
}

void LatLonBarCmd::doit()
{
  // Save the old value for a rainy day and change to the new value;
  
  _oldValue = _imageView->IsLatLonBarDisplayed();
  _imageView->showLatLonBar((_value != 0));
}

void LatLonBarCmd::undoit()
{
  // just set the value to FALSE (don't keep track of the old value)
  
  _imageView->showLatLonBar(FALSE);
  _value = (CmdValue) FALSE;
  newValue();
}

