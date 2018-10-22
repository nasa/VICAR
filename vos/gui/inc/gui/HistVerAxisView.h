//////////////////////////////////////////////////////////////
// HistVerAxisView.h:
/////////////////////////////////////////////////////////////
#ifndef HISTVERAXISVIEW_H
#define HISTVERAXISVIEW_H
#include "HistAxisView.h"
#include "HistDefs.h"

class HistVerAxisView : public HistAxisView {

  protected:

	XmString  xmstr;

	Dimension _width, _height;

    	virtual void display();

  public:

    	HistVerAxisView ( Widget, const char *,
				Histogram *, Histogram *, Histogram *, 
				OrientType, VerAxisDirType );

    	virtual const char *const className() { return "HistVerAxisView"; }
};
#endif

