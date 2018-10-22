////////////////////////////////////////////////////////////////
// LutBox.h
////////////////////////////////////////////////////////////////
#ifndef LUTBOX_H
#define LUTBOX_H
#include "UIComponent.h"
#include "Lut.h"

class Lut;

class LutBox : public UIComponent {

  public:

	LutBox(Widget, const char *, Lut *, Lut *, Lut *);

	virtual const char *const className() { return "LutBox"; }

};
#endif
