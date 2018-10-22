////////////////////////////////////////////////////////////////
// WedgeOverlayView.h: Provides overlay graphics support for wedge class
// 		       The class allows to put set marks that are used
//		       by pseudocolor tool
////////////////////////////////////////////////////////////////
#ifndef WEDGEOVERLAYVIEW_H
#define WEDGEOVERLAYVIEW_H
#include "BasicWedgeOverlay.h"
#include "XvicImage.h"

class PseudoMarks;
class PseudoValue;

class WedgeOverlayView : public BasicWedgeOverlay {

  private:

        static XtResource _resources[];

  protected:

	PseudoValue 	*_pseudoValue;		// Model for pseudocolor value

	Widget _iw;

	XvicGC    _lineGC;
        XvicColor _lineColor, _curLineColor;

        String    _markColor, _selectedMarkColor;    // From the resource
	Dimension _markLength, _markThickness, _barThickness;

  public:

	WedgeOverlayView(Widget, const char *);

	Widget getWidget() { return _iw; };

	virtual void update ( PseudoMarks * );
	virtual void update ( PseudoValue * ) { };

	virtual const char *const className() { return ("WedgeOverlayView"); }
};
#endif
