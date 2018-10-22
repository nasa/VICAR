///////////////////////////////////////////////////////
// HistVerAxisView.cc:
////////////////////////////////////////////////////////
#include "HistVerAxisView.h"
#include "Histogram.h"
#include <stdio.h>
#include <iostream>
using namespace std;

HistVerAxisView::HistVerAxisView ( Widget parent, const char *name, 
		Histogram *hist, Histogram *hist1, Histogram *hist2,
		OrientType hor, VerAxisDirType verAxisDir) 
		: HistAxisView (parent,name,hist,hist1,hist2, hor)
{
	_verAxisDir = verAxisDir;
}

void HistVerAxisView::display ( )
{
    if ( !XtIsRealized(_w) )
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

        double min = _hist->getLowerLimit();		// min label
        double max = _hist->getUpperLimitBound();	// max label

        double lbl[16];
        char buf[100][16];

	// Always clear window before start drawing
	XClearWindow ( XtDisplay(_ruler), XtWindow(_ruler) );

	// Draw a long line from top to bottom
	XDrawLine ( XtDisplay(_ruler), XtWindow(_ruler), _gc, 
			_width - _drawOffset, 0, 
			_width - _drawOffset, _height );

	// Calculate how many ticks we need at this screen height
	int numTicks = 16;
	if ( _height < _twoTicks ) numTicks = 2;
	else if ( _height < _fourTicks ) numTicks = 4;
	     else if ( _height < _eightTicks ) numTicks = 8;

	// These calculations are necessary to achieve acceptable precision in putting
	// ticks along the ruler
	int step = int(_height) / numTicks;
	int temp = int(_height) % numTicks;
	double temp1 = (double)temp / (double)numTicks;
	double temp2 = temp1;

	// Draw ticks

	Dimension strOffset = 1;
	Dimension strWidth, strHeight;

	for ( int i=0; i<=numTicks; i++ )
	{
	   if ( (i % 2) == 0 && (i != numTicks))  // every other tick is longer
	   	XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc, 
					_width - _drawOffset - _longTickLength,
					int((i*step)+temp1),
					_width - _drawOffset,
					int((i*step)+temp1) );
	   else if (i == numTicks) 		// last tick
		XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc,
					_width - _drawOffset - _longTickLength,
					int((i*step)+temp1) - 1,
					_width - _drawOffset,
					int((i*step)+temp1) - 1 );
	   else 
                XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                                        _width - _drawOffset - _shortTickLength,
					int((i*step)+temp1),
                                        _width - _drawOffset, 
					int((i*step)+temp1) );

	   // Draw a label to the tick, the top label is positioned lower
	   // to be visible, the bottom label is above its tick
	   if (_verAxisDir == DESC)
	   {
             lbl[i] = min + ((max - min) / numTicks ) * i ;

             if (_hist->isIntRange()) {
	        // subtract 1 from the max label so range looks right
	        if ( i == numTicks ) sprintf ( buf[i], "%d", (int)(lbl[i]-1));
                else sprintf ( buf[i], "%d", (int)lbl[i]);
             }
             else
                sprintf(buf[i], "%.3g", lbl[i]);

	     strWidth  = Dimension ( XTextWidth ( _fontStruct, buf[i], strlen(buf[i]) ) );
	     strHeight = Dimension ( _fontStruct->ascent );

             // draw label to every other tick but not the first or the last one
             if ( ((i%2) == 0) && (i != numTicks) && (i != 0) )
                XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                             _width - strWidth - strOffset - _longTickLength - _drawOffset,
                             int((i*step)+temp1)+strHeight/2,
                             buf[i], strlen(buf[i]));
             if ( i == 0 )
                XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                             _width - strWidth - strOffset - _longTickLength - _drawOffset,
                             int((i*step)+temp1)+strHeight,
                             buf[i], strlen(buf[i]));
             if ( i == numTicks )
                XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                             _width - strWidth - strOffset - _longTickLength - _drawOffset,
                             int((i*step)+temp1),
                             buf[i], strlen(buf[i]));
	   }

	   else
	   {
             lbl[i] = max - (min + ((max - min) / numTicks ) * i);

             if (_hist->isIntRange()) {
	        // subtract 1 from the max label so range looks right
	        if ( i == 0 ) sprintf ( buf[i], "%d", (int)(lbl[i]-1));
                else sprintf ( buf[i], "%d", (int)lbl[i]);
             }
             else
                sprintf(buf[i], "%.3g", lbl[i]);

	     strWidth  = Dimension ( XTextWidth ( _fontStruct, buf[i], strlen(buf[i]) ) );
	     strHeight = Dimension ( _fontStruct->ascent );

	     // draw label to every other tick but not the first or the last one
	     if ( ((i%2) == 0) && (i != numTicks) && (i != 0) )
	   	XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
				_width - strWidth - strOffset - _longTickLength - _drawOffset,
				int((i*step)+temp1)+strHeight/2, 
				buf[i], strlen(buf[i]));
	     if ( i == 0 )
		XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                                _width - strWidth - strOffset - _longTickLength - _drawOffset,
                                int((i*step)+temp1)+strHeight,
                                buf[i], strlen(buf[i]));
	     if ( i == numTicks )
                XDrawString (XtDisplay(_ruler), XtWindow(_ruler), _gc,
                                _width - strWidth - strOffset - _longTickLength - _drawOffset,
                                int((i*step)+temp1),
                                buf[i], strlen(buf[i]));
	   }

	   temp1 = temp1 + temp2;
	}
    }

    else {
	XtVaSetValues ( _ruler, XmNwidth, 0, NULL );
    }
}
