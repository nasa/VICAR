///////////////////////////////////////////////////////
// HistHorAxisView.C:
////////////////////////////////////////////////////////
#include "HistHorAxisView.h"
#include "Histogram.h"
#include <stdio.h>
#include <iostream>
using namespace std;

HistHorAxisView::HistHorAxisView ( Widget parent, const char *name, 
		Histogram *hist, Histogram *hist1, Histogram *hist2,
		OrientType hor ) 
		: HistAxisView ( parent,name,hist, hist1, hist2, hor )
{
    // Empty
}

void HistHorAxisView::display ( )
{
 if ( XtIsRealized(_w) )
 {
	// make any resize cause expose event
        XSetWindowAttributes attrs;
        attrs.bit_gravity = ForgetGravity;
        XChangeWindowAttributes ( XtDisplay(_ruler),
               			XtWindow(_ruler), CWBitGravity, &attrs );

	// Remember geometry
        XtVaGetValues ( _ruler,
                        XmNwidth,  &_width,
                        XmNheight, &_height,
                        NULL );
 
   if (this->_hor == VERTICAL) {

        double min = _hist->getLowerLimit();		// min label
        double max = _hist->getUpperLimitBound();	// max label

        double lbl[16];		
        char buf[100][16];

	// Always clear window before start drawing
	XClearWindow ( XtDisplay(_ruler), XtWindow(_ruler) );

	// Draw a long line from side to side
	XDrawLine ( XtDisplay(_ruler), XtWindow(_ruler), _gc, 
					0, 	_drawOffset, 
					_width, _drawOffset );

	// Calculate how many ticks we need at this screen width
	int numTicks = 16;
	if ( _width < _twoTicks ) numTicks = 2;
	else if ( _width < _fourTicks ) numTicks = 4;
	     else if ( _width < _eightTicks ) numTicks = 8;

	// These calculations are necessary to achieve acceptable precision 
	// in putting ticks along the ruler
	int step = int(_width) / numTicks;
	int temp = int(_width) % numTicks;
	double temp1 = (double)temp / (double)numTicks;
	double temp2 = temp1;

	// Draw ticks

	Dimension strOffset = 1;
	Dimension strWidth, strHeight;

	for ( int i=0; i<=numTicks; ++i)
	{
	   if ( ((i%2) == 0) && (i!=numTicks))  // every other tick is longer
	   	XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc, 
					int((i*step)+temp1), 
					_drawOffset,
					int((i*step)+temp1), 
					_drawOffset + _longTickLength);
	   else if (i ==numTicks)		// last label
		XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                                        _width - 1,
                                        _drawOffset,
                                        _width - 1,
                                        _drawOffset + _longTickLength);
	   else 
                XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                                        int((i*step)+temp1), 
					_drawOffset,
                                        int((i*step)+temp1), 
					_drawOffset + _shortTickLength);

	   // Draw a label to the tick, 
	   // all labels except for the first and last are centered around its tick
           lbl[i] = min + ((max - min) / numTicks ) * i;

           if (_hist->isIntRange()) {
	      // subtract 1 from the max label so range looks right
	      if (i == numTicks) sprintf ( buf[i], "%d", (int)(lbl[i]-1));
	      else sprintf ( buf[i], "%d", (int)lbl[i]);
           }
           else
              sprintf(buf[i], "%.3g", lbl[i]);

	   strWidth = XTextWidth ( _fontStruct, buf[i], strlen(buf[i]) );
	   strHeight = Dimension ( _fontStruct->ascent );

	   // draw label to every other tick but not the first or the last one
	   if ( ((i%2) == 0) && (i!=0) && (i!=numTicks) ) 
	   	XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
				int((i*step) + temp1 - int(strWidth)/2), 
				_drawOffset + _longTickLength + strOffset + strHeight,
				buf[i], strlen(buf[i]) );
	   if ( i==0 )	// first label 
		XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                                0,
                                _drawOffset + _longTickLength + strOffset + strHeight,
                                buf[i], strlen(buf[i]) );

	   if ( i== numTicks ) // last label
		XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
				int((i*step)+temp1) - strWidth,
				_drawOffset + _longTickLength + strOffset + strHeight,
				buf[i], strlen(buf[i]) );	
	   temp1 = temp1 + temp2;
	}
   }
   else
	XtVaSetValues ( _ruler,
                        XmNheight,              0,
                        NULL );

 }
}
