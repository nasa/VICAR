//////////////////////////////////////////////////////////////
// HistHorAxisView.h:
/////////////////////////////////////////////////////////////
#ifndef HISTHORAXISVIEW_H
#define HISTHORAXISVIEW_H
#include "HistAxisView.h"
#include "HistDefs.h"

class HistHorAxisView : public HistAxisView {

  protected:

	XmString        xmstr;

	Dimension _width, _height;

    	virtual void display();

  public:

    	HistHorAxisView ( Widget, const char *,
			Histogram *, Histogram *, Histogram *, OrientType );


    	virtual const char *const className() { return "HistHorAxisView"; }
};
#endif

