////////////////////////////////////////////////////////////
// SideBarCmd.cc: Creates the SideBar Command for managing
//                whether the SideBar is displayed or not
//                from the dialog check box in Preferences
///////////////////////////////////////////////////////////
#include "SideBarCmd.h"
#include "ImageDisplayer.h"
#include <iostream>
using namespace std;
#include <stdint.h>

SideBarCmd::SideBarCmd( const char *name, int active, ImageDisplayer *obj )
		: Cmd ( name, active )
{
    // Save the image view and determine the current state to set
    //  the check box appropriately.

    _imageView = obj;
    int value = (int) _imageView->IsSideBarDisplayed();
    _value = (CmdValue) (uintptr_t) value;
    newValue();

}

void SideBarCmd::doit()
{
    // Save the old value for the undoit command and change to
    //  the new value;

    _oldValue = _imageView->IsSideBarDisplayed();
    _imageView->showSideBar( (_value != 0));

}      

void SideBarCmd::undoit()
{

     // Undo the command to the last SideBar state

    _imageView->showSideBar( _oldValue );
    _value = (CmdValue) (uintptr_t) _oldValue;
    newValue();

}       














