//////////////////////////////////////////////////////////////
// RadioButtonInterface.h: A radio box button interface to a Cmd object
// The value is either TRUE or FALSE.  Two differences from CheckBox:
// the type of indicator used, and the user can only turn this on
// (it is assumed that setValue will turn it off).
///////////////////////////////////////////////////////////////
#ifndef RADIOBUTTONINTERFACE_H
#define RADIOBUTTONINTERFACE_H
#include "CheckBoxInterface.h"

class RadioButtonInterface : public CheckBoxInterface {

  protected:

    virtual void executeCmd ( XtPointer );

  public:
    
    RadioButtonInterface ( Widget, Cmd * );
    RadioButtonInterface ( Widget, const char * );
};
#endif
