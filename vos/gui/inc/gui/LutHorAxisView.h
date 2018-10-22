///////////////////////////////////////////////////////
// LutVerAxisView.C: This class implements update function
// that draws axis oriented vertically.
////////////////////////////////////////////////////////
#ifndef LUTHORAXISVIEW_H
#define LUTHORAXISVIEW_H
#include "LutAxisView.h"
#include "Lut.h"

class LutHorAxisView : public LutAxisView {

  protected:

	XmString        xmstr;

	Dimension _width, _height;

    	virtual void update();

  public:

    	LutHorAxisView ( Widget parent, const char *name, 
			Lut *lut, Lut *lut1, Lut *lut2 )
		: LutAxisView (parent, name, lut, lut1, lut2) 
			{ }

	LutHorAxisView ( Widget parent, const char *name, Lut *lut)
		: LutAxisView (parent, name, lut)
			{ }

	virtual ~LutHorAxisView() { } 

    	virtual const char *const className() { return "LutHorAxisView"; }
};
#endif

