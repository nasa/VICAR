///////////////////////////////////////////////////////
// LutHorAxisView.C: This class implements update function
// that draws axis oriented horizontally.
////////////////////////////////////////////////////////
#ifndef LUTVERAXISVIEW_H
#define LUTVERAXISVIEW_H
#include "LutAxisView.h"

class Lut;

class LutVerAxisView : public LutAxisView {

  protected:

	XmString        xmstr;

	Dimension _width, _height;

    	virtual void update();

  public:

    	LutVerAxisView ( Widget parent, const char *name, 
			Lut *lut, Lut *lut1, Lut *lut2 )
		: LutAxisView (parent, name, lut, lut1, lut2)
			{ if (_lut) _lut->attachView(this); }

	LutVerAxisView ( Widget parent, const char *name, Lut *lut )
                : LutAxisView (parent, name, lut)
			{ if (_lut) _lut->attachView(this); }

	virtual ~LutVerAxisView() { }

    	virtual const char *const className() { return "LutVerAxisView"; }
};
#endif

