//////////////////////////////////////////////////////////////////
// HistVerAxisView.cc:  This module is designed for use on MPF 
// display.  It displays an vertical axis without tick marks, but 
// with labels indicating the DN count for a logarithmically scaled 
// histogram.  Labels are 1e1, 1e2, 1e3, etc.  User can set resources 
// *fontname and *drawOffset to specify position and appearance of 
// labels.  Note that this component will not work properly if 
// histogram's spike is greater than one.
//////////////////////////////////////////////////////////////////
#include "HistLogVerAxis.h"
#include "Histogram.h"
#include <stdio.h>
#include <math.h>

#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif

HistLogVerAxis::HistLogVerAxis ( Widget parent, const char *name, 
		Histogram *hist, Histogram *hist1, Histogram *hist2,
		OrientType hor) 
		: HistAxisView (parent,name,hist,hist1,hist2, hor)
{
    // Empty
}

void HistLogVerAxis::display ( )
{
    if ( !XtIsRealized(_ruler) )
	return;

    // Make any resize cause expose event
    XSetWindowAttributes attrs;
    attrs.bit_gravity = ForgetGravity;
    XChangeWindowAttributes ( XtDisplay(_ruler),
		  XtWindow(_ruler), CWBitGravity, &attrs );

    // Save geometry
    XtVaGetValues ( _ruler,
                  XmNwidth,  &_width,
                  XmNheight, &_height,
                  NULL );
 
    if (_hor == HORIZONTAL) { 

	// Always clear window before start drawing
	XClearWindow ( XtDisplay(_ruler), XtWindow(_ruler) );

	double maxValue = 0.0;
	if (_hist && _hist1 && _hist2) 
	    maxValue = (double)MAX(MAX(_hist->spike(1),
                                           _hist1->spike(1)),
                                       _hist2->spike(1));
	else if (_hist)
	    maxValue = _hist->spike(1);
	else if (_hist1)
	    maxValue = _hist1->spike(1);
	else if (_hist2)
	    maxValue = _hist2->spike(1);

	if ( maxValue > 0 ) {
	    maxValue = ceil(log10(maxValue));
	}
	else {
	    maxValue = 1.0;
	}

	for (int i = 1; i <= (int)maxValue; i++) {
		char str [8];
		sprintf(str, "1e%d", i);
		double r;
		r = log10(1 * pow(10.0, (double)i)) / maxValue;
		int strHeight = _fontStruct->ascent;
		if (i != maxValue)
		    XDrawString ( XtDisplay(_ruler), XtWindow(_ruler), _gc,
			_drawOffset, (int)(_height-1-(_height*r)+strHeight/2),
			str, strlen(str) );
		else 
		    XDrawString ( XtDisplay(_ruler), XtWindow(_ruler), _gc,
			_drawOffset, (int)(_height-1-(_height*r)+strHeight),
			str, strlen(str) );
	}
    }
    else {
	XtVaSetValues ( _ruler, XmNwidth, 0, NULL );
    }
}
