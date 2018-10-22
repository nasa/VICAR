///////////////////////////////////////////////////////////////////
// BasicWedgeView.h:
///////////////////////////////////////////////////////////////////
#ifndef BASICWEDGEVIEW_H
#define BASICWEDGEVIEW_H
#include "UIComponent.h"
#include "XvicImage.h"
#include <stdio.h>
#include <iostream>

class BasicWedgeView : public UIComponent {

  private:

  protected:

  public:

	BasicWedgeView ( const char * name ) : UIComponent (name)  {}

	virtual const char *const className() { return  "BasicWedgeView"; }
};
#endif

