////////////////////////////////////////////////////////////////
// BasicWedgeOverlay.h: 
////////////////////////////////////////////////////////////////

#ifndef BASICWEDGEOVERLAY_H
#define BASICWEDGEOVERLAY_H
#include "UIComponent.h"

class PseudoMarks;
class PseudoValue;

class BasicWedgeOverlay : public UIComponent {

  public:

	BasicWedgeOverlay(const char *name) 
				: UIComponent (name) { }

	virtual Widget getWidget() { return NULL; }

	virtual void update ( PseudoMarks * ) = 0;
	virtual void update ( PseudoValue * ) = 0;

	virtual const char *const className() { return ("BasicWedgeOverlay"); }
};
#endif
