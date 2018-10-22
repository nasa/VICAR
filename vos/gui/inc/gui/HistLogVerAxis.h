//////////////////////////////////////////////////////////////////
// HistVerAxisView.h:  This module is designed for use on MPF
// display.  It displays an vertical axis without tick marks, but
// with labels indicating the DN count for a logarithmically scaled
// histogram.  Labels are 1e1, 1e2, 1e3, etc.
//////////////////////////////////////////////////////////////////
#ifndef HISTLOGVERAXIS_H
#define HISTLOGVERAXIS_H
#include "HistAxisView.h"
#include "HistDefs.h"

class HistLogVerAxis : public HistAxisView {

  protected:

	Dimension _width, _height;

    	virtual void display();

  public:

    	HistLogVerAxis ( Widget, const char *, 
			 Histogram *, Histogram *, Histogram *, 
			 OrientType );
	virtual ~HistLogVerAxis() { }

    	virtual const char *const className() { return "HistLogVerAxis"; }
};
#endif

