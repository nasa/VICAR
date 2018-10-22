//////////////////////////////////////////////////////////////////////////
// HistGraphView.cc:  This is a view component that draws histogram.  
// BW histogram gets drawn in the appropriate color -- red, green, or blue.
// Color histogram is drawn with colors blended (displayBlend method) or 
// stacked on top of each other (displayStacked method).
//////////////////////////////////////////////////////////////////////////
#include "HistGraphView.h"
#include "Histogram.h"
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <math.h>
using std::cerr;
using std::endl;

#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif

// Resources for this class

XtResource HistGraphView::_resources [ ] = {
 {
    (char *)XvicNspike,
    (char *)XvicCSpike,
    XmRInt,
    sizeof ( int ),
    XtOffset ( HistGraphView *, _spike ),
    XmRImmediate,
    ( XtPointer ) 1,
 },

 {
    (char *)"redColor",
    (char *)"RedColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _red ),
    XmRImmediate,
    ( XtPointer ) "red",
 },
 {
    (char *)"greenColor",
    (char *)"GreenColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _green ),
    XmRImmediate,
    ( XtPointer ) "green",
 },
 {
    (char *)"blueColor",
    (char *)"BlueColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _blue ),
    XmRImmediate,
    ( XtPointer ) "blue",
 },
 {
    (char *)"magentaColor",
    (char *)"MagentaColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _magenta ),
    XmRImmediate,
    ( XtPointer ) "magenta",
 },
 {
    (char *)"yellowColor",
    (char *)"YellowColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _yellow ),
    XmRImmediate,
    ( XtPointer ) "yellow",
 },
 {
    (char *)"cyanColor",
    (char *)"CyanColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _cyan ),
    XmRImmediate,
    ( XtPointer ) "cyan",
 },
 {
    (char *)"whiteColor",
    (char *)"WhiteColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _white ),
    XmRImmediate,
    ( XtPointer ) "white",
 },

 {
    (char *)"drawInsideTicks",
    (char *)"DrawInsideTicks",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( HistGraphView *, _drawInsideTicks ),
    XmRImmediate,
    ( XtPointer ) False,
 },
 {
    (char *)"insideTickColor",
    (char *)"InsideTickColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistGraphView *, _insideTickColor ),
    XmRImmediate,
    ( XtPointer ) "white",
 },
 {
    (char *)"insideTickLength", 
    (char *)"InsideTickLength",
    XmRInt, 
    sizeof ( int ),
    XtOffset ( HistGraphView *, _insideTickLength ),
    XmRImmediate,
    ( XtPointer ) 10, 
 },
 {
    (char *)"insideMinorTickLength",
    (char *)"InsideMinorTickLength",
    XmRInt,
    sizeof ( int ),
    XtOffset ( HistGraphView *, _insideMinorTickLength ),
    XmRImmediate,
    ( XtPointer ) 5,
 },
 {
    (char *)"insideTickInterval",
    (char *)"InsideTickInterval",
    XmRInt,
    sizeof ( int ),
    XtOffset ( HistGraphView *, _insideTickInterval ),
    XmRImmediate,
    ( XtPointer ) 64,
 },
 {
    (char *)"insideMinorTickInterval",
    (char *)"InsideMinorTickInterval",
    XmRInt,
    sizeof ( int ),
    XtOffset ( HistGraphView *, _insideMinorTickInterval ),
    XmRImmediate,
    ( XtPointer ) 16,
 },
};

HistGraphView::HistGraphView ( Widget parent, const char *name, 
		Histogram *hist, Histogram *hist1, Histogram *hist2,
		MethodType method, OrientType hor, VerAxisDirType verAxisDir ) 
	: HistView (name)
{
	_method = method;
	_hor = hor;
	_verAxisDir = verAxisDir;
	_log = False;

	_hist = hist;
	_hist1 = hist1;
	_hist2 = hist2;

	_w  = XtVaCreateWidget ( _name,
                                xmDrawingAreaWidgetClass, parent,
				NULL );
	installDestroyHandler ();

	getResources ( _resources, XtNumber ( _resources ) );

	XtAddCallback ( _w, XmNexposeCallback,
			&HistGraphView::displayCallback,
			( XtPointer ) this );

        _gc = XtGetGC ( _w, 0, 0 );


	if (_hist)	_hist->attachView(this);
	if (_hist1)	_hist1->attachView(this);
	if (_hist2)	_hist2->attachView(this);
}

HistGraphView::~HistGraphView()
{
	if (_hist)      _hist->detachView(this);
        if (_hist1)     _hist1->detachView(this);
        if (_hist2)     _hist2->detachView(this);

	if ( _w && _gc ) 
		XtReleaseGC ( _w, _gc );
}

void HistGraphView::displayCallback ( Widget,
				 XtPointer client_data,
				 XtPointer )
{
	HistGraphView *obj;
	obj = ( HistGraphView * ) client_data;
	if (obj != NULL)
		obj->update();
}

/************************************************************************
* update: Draw histogram.
************************************************************************/
void HistGraphView::update ( )
{
	if ( !XtIsRealized(_w) )
		return;

	// Make any resize cause expose event
	XSetWindowAttributes attrs;
	attrs.bit_gravity = ForgetGravity;
	XChangeWindowAttributes ( XtDisplay(_w),
		XtWindow(_w), CWBitGravity, &attrs );

	// Save geometry
	XtVaGetValues ( _w,
			XmNwidth,  &_width,
			XmNheight, &_height,
			NULL );

	// At least one histogram model must exist
	if (!_hist && !_hist1 && !_hist2) 
		return;

	// Always clear the window before drawing
	XClearWindow ( XtDisplay(_w), XtWindow(_w) );

	Boolean RedOnly   = (_hist1 == NULL) && (_hist2 == NULL);
	Boolean GreenOnly = (_hist == NULL) && (_hist2 == NULL);
	Boolean BlueOnly  = (_hist == NULL) && (_hist1 == NULL);

	if ( RedOnly || GreenOnly || BlueOnly )
		displayBW ( RedOnly, GreenOnly, BlueOnly);

	if ( _hist && _hist1 && _hist2 )
	    switch (_method) {
		case STACKED:	displayStacked();
		break;

		case BLEND:	displayBlend();
		break;

		default: 	cerr << "Undefined method" << endl;
		break;
	    }

        XSetForeground ( XtDisplay(_w), _gc, 
			 WhitePixel( XtDisplay(_w),
                                    DefaultScreen(XtDisplay(_w)) ) );
}

/************************************************************************
* displayBW: Draw Black & White Histogram
*
************************************************************************/
void HistGraphView::displayBW ( Boolean R, Boolean G, Boolean B )
{
	Histogram *h;	// Pointer to the histogram model to be displayed
	if ( R )          h = _hist; 
	else if ( G )     h = _hist1;
	else if ( B )     h = _hist2;
	else return;

        XColor exact;
        Colormap cmap = DefaultColormap ( XtDisplay(_w), 
					  DefaultScreen ( XtDisplay(_w) ) );
	XColor  color;
	if ( R ) {
        	XAllocNamedColor ( XtDisplay(_w), cmap, _red, &color, &exact);
	}
	else if ( G ) {
        	XAllocNamedColor ( XtDisplay(_w), cmap, _green, &color, &exact);
	}
	else if ( B ) {
		XAllocNamedColor ( XtDisplay(_w), cmap, _blue, &color, &exact);
	}
	XSetForeground ( XtDisplay(_w), _gc, color.pixel );

        int nbins = h->numBins();
	double max_value = h->spike(_spike);
	if ((int)max_value == 0) 
		max_value = 1.0;

	if (_log == True) {
		if (_drawInsideTicks == True) {
			max_value = ceil(log10(max_value));
		}
		else {
			max_value = log10(max_value);
		}
		if ((int)max_value == 0)
			max_value = 1.0;
	}

	double ppb;				// pixels-per-bin
	if (_hor == VERTICAL) {
		if (nbins > 0)
			ppb = (double)_width / (double)nbins;
		else
			ppb = (double)_width;
	}
        else {
		if (nbins > 0)
			ppb = (double)_height / (double)nbins;
		else
			ppb = (double)_height;
	}


        if ( !max_value ) max_value = 1.0;

        for (int i=0; i<nbins; i++)
        {
                double value = (double)h->getBin(i);
		if (_log == True)
			if (value > 0) 
				value = log10(value);

		double ratio;
		if (max_value > 0)
                	ratio = value / max_value;
		else 
			ratio = 0;

		int x, y, width, height; 
                if (_hor == HORIZONTAL) {
		   x = 0;
		   if (_verAxisDir == DESC) {		// Descending axis
			y = int (i*ppb);
			width = int (_width*ratio) + 1;
			height = int (ppb) + 1;
		   }
		   else	{				// Ascending axis
			y = int (_height - ((i+1)*ppb));
			width = int (_width*ratio) + 1;
			height = int (ppb) + 1;
		   }
		}
		else {
			x = int(i*ppb);
			y = int (_height * (1 - ratio));
			width = int (ppb) + 1;
			height = int (_height*ratio) + 1;
		}
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                x, y, width, height);
	}

	if (_drawInsideTicks == True) {
                drawInsideTicks(max_value, ppb, nbins);
        }
}

/************************************************************************
* Display histogram using "Stacked Colors" method - for color images only.
* At each bin, the one with the max count goes into the back, and the one 
* with the min count goes in the front of the display.
************************************************************************/
void HistGraphView::displayStacked ( )
{
        XColor exact;
        Colormap cmap = DefaultColormap ( XtDisplay(_w), 
					  DefaultScreen ( XtDisplay(_w) ) );
	XColor colorR, colorG, colorB;
	XAllocNamedColor ( XtDisplay(_w), cmap, _red, &colorR, &exact);
	XAllocNamedColor ( XtDisplay(_w), cmap, _green, &colorG, &exact);
	XAllocNamedColor ( XtDisplay(_w), cmap, _blue, &colorB, &exact);

        int nbins  = _hist->numBins();
        int nbins1 = _hist1->numBins();
        int nbins2 = _hist2->numBins();

        if ( nbins != nbins1 && nbins1 != nbins2 ) {
                cerr << "Number of bins is not the same!" << endl;
		return;
	}

        double ppb;				// pixels-per-bin
	if (_hor == HORIZONTAL) {
		if (nbins != 0)
			ppb = (double)_height / (double)nbins;
		else
			ppb = (double)_height;
	}
        else {
		if (nbins != 0)
			ppb = (double)_width / (double)nbins;
		else
			ppb = (double)_width;
	}

        double max_value = (double)MAX(MAX(_hist->spike(_spike), 
					   _hist1->spike(_spike)),
				       _hist2->spike(_spike));

	if ( max_value > 0 ) {
		if ( _log == True ) {
			if ( _drawInsideTicks == True ) {
				max_value = ceil(log10(max_value));
			}
			else {
				max_value = log10(max_value);
			}
		}
	}
        else 
		max_value = 1.0;

        for (int i = 0; i < nbins; i++)
        {
		// Get the value to draw
                double value  = (double)_hist->getBin(i);
                double value1 = (double)_hist1->getBin(i);
                double value2 = (double)_hist2->getBin(i);

		if (_log == True) {
                	if (value  > 0) value  = log10(value);
			if (value1 > 0) value1 = log10(value1);
			if (value2 > 0) value2 = log10(value2);
		}

                double ratio;
                double ratio1;
                double ratio2;

		if (max_value > 0)
		{
			ratio = value  / max_value;
			ratio1 = value1 / max_value;
			ratio2 = value2 / max_value;
		}
		else 
			ratio = ratio1 = ratio2 = 0.0;

		// identify the highest, lowest, and middle points in this bin
                const double mn = MIN ( MIN ( value, value1 ), value2 );
                const double mx = MAX ( MAX ( value, value1 ), value2 );
                double md;	// The remaining band is the middle value
                if (value != mn && value != mx) { md = value; }
                else if (value1 != mn && value1 != mx) { md = value1; }
                else if (value2 != mn &&  value2 != mx) { md = value2; }
		else if (value == value1 || value == value2) { md = value; }
		else { md = value1; }

                double curRatioMn = 0.0;
                double curRatioMd = 0.0;
                double curRatioMx = 0.0;

		// Draw the lower (minimum) portion of the histogram
                if ( mn == value )
                {
                        curRatioMn = ratio;
                        XSetForeground ( XtDisplay(_w), _gc, colorR.pixel );
                }
                else if ( mn == value1 )
                {
                        curRatioMn = ratio1;
                        XSetForeground ( XtDisplay(_w), _gc, colorG.pixel );
                }

                else if ( mn == value2 )
                {
                        curRatioMn = ratio2;
                        XSetForeground ( XtDisplay(_w), _gc, colorB.pixel );
                }
                else cerr << "Error: mn must be defined!" << endl;

		if (_hor == HORIZONTAL)
		  if (_verAxisDir == DESC)
                	XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
				0,
				int (i*ppb),
				int (ceil(_width*curRatioMn)),
				int (ceil(ppb)) );
		   else
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                0,
                                int (_height - ((i+1)*ppb)),
                                int (ceil(_width*curRatioMn)),
                                int (ceil(ppb)) );
		else
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*ppb), 
				int (_height * (1 - curRatioMn)),
                                int (ceil(ppb)),  
				int (ceil(_height*curRatioMn)) );


                // Draw the middle portion of the histogram
                if ( md == value )
                {
                        curRatioMd = ratio;
                        XSetForeground ( XtDisplay(_w), _gc, colorR.pixel );
                }
                else if ( md == value1 )
                {
                        curRatioMd = ratio1;
                        XSetForeground ( XtDisplay(_w), _gc, colorG.pixel );
                }
                else if ( md == value2 )
                {
                        curRatioMd = ratio2;
                        XSetForeground ( XtDisplay(_w), _gc, colorB.pixel );
                }
                else cerr << "Error: md must be defined!" << endl;

		if (_hor == HORIZONTAL)
		  if (_verAxisDir == DESC)
                        XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMn),
                                int (i*ppb),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(ppb)) );
		   else
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMn),
                                int (_height - int((i+1)*ppb)),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(ppb)) );
		else
                	XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*ppb), 
				int (_height * (1 - curRatioMd)),
                                int (ceil(ppb)),
				int (ceil(_height*curRatioMd - _height*curRatioMn)) );


                // Draw the top (max) portion of the histogram
                if ( mx == value )
                {
                        curRatioMx = ratio;
                        XSetForeground ( XtDisplay(_w), _gc, colorR.pixel );
                }
                else if ( mx == value1 )
                {
                        curRatioMx = ratio1;
                        XSetForeground ( XtDisplay(_w), _gc, colorG.pixel );
                }
                else if ( mx == value2 )
                {
                        curRatioMx = ratio2;
                        XSetForeground ( XtDisplay(_w), _gc, colorB.pixel );
                }
                else cerr << "Error: there must be a max point!" << endl;

		if (_hor == HORIZONTAL) 
		  if ( _verAxisDir == DESC)
                        XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMd),
                                int (i*ppb),
                                int (ceil(_width*(curRatioMx - curRatioMd))),
                                int (ceil(ppb)) );
		   else
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width * curRatioMd),
                                int (_height - int((i+1)*ppb)),
                                int (ceil(_width * (curRatioMx - curRatioMd))),
                                int (ceil(ppb)) );
                else 
	                XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*ppb), 
				int (_height * (1 - curRatioMx)),
                                int (ceil(ppb)), 
				int (ceil(_height*curRatioMx-_height*curRatioMd)) );
        }

	if (_drawInsideTicks == True) {
                drawInsideTicks(max_value, ppb, nbins);
        }
}

/*******************************************************************************
* Blended Colors Method
*   Color arithmetics:
*	R + G + B = White
*	R + G = Yellow
*	R + B = Magenta
*	B + G = Cyan
*******************************************************************************/
void HistGraphView::displayBlend ( )
{
	// Allocate all necessary colors

        XColor exact;
        Colormap cmap = DefaultColormap ( XtDisplay(_w), 
					DefaultScreen ( XtDisplay(_w) ) );
	XColor colorR, colorG, colorB, colorM,  colorC, colorY, colorW;
        XAllocNamedColor ( XtDisplay(_w), cmap, _red, &colorR, &exact );
        XAllocNamedColor ( XtDisplay(_w), cmap, _green, &colorG, &exact );
        XAllocNamedColor ( XtDisplay(_w), cmap, _blue, &colorB, &exact );
        XAllocNamedColor ( XtDisplay(_w), cmap, _magenta, &colorM, &exact );
        XAllocNamedColor ( XtDisplay(_w), cmap, _cyan, &colorC, &exact );
        XAllocNamedColor ( XtDisplay(_w), cmap, _yellow, &colorY, &exact );
        XAllocNamedColor ( XtDisplay(_w), cmap, _white, &colorW, &exact );

	// Check bands for consistency

        int nbins  = _hist->numBins();
        int nbins1 = _hist1->numBins();
        int nbins2 = _hist2->numBins();

        if ( nbins != nbins1 && nbins1 != nbins2 ) {
                cerr << "Number of bins is not the same!" << endl;
		return;
	}

	// Calculate how many pixels each bin occupies

	double ppb;
	if (_hor == VERTICAL) {
		if (nbins != 0)
			ppb = (double)_width / (double)nbins;
		else
			ppb = (double)_width;
	}
        else {
		if (nbins != 0)
			ppb = (double)_height / (double)nbins;
		else
			ppb = (double)_height;
	}

	// Calculate maximum number of pixels

        double max_value = (double)MAX(MAX(_hist->spike(_spike), 
					   _hist1->spike(_spike)),
				       _hist2->spike(_spike));
        if ( max_value > 0 ) {
		if ( _log == True ) {
			if ( _drawInsideTicks == True ) {
				max_value = ceil(log10(max_value));
			}
			else {
				max_value = log10(max_value);
			}
		}
	}
        else
                max_value = 1.0;



	int i;
        for (i = 0; i < nbins; i++)
        {
                double value  = (double)_hist->getBin(i);
                double value1 = (double)_hist1->getBin(i);
                double value2 = (double)_hist2->getBin(i);

		if (_log == True) {
                        if (value  > 0) value =  log10(value);
                        if (value1 > 0) value1 = log10(value1);
                        if (value2 > 0) value2 = log10(value2);
                }

                double ratio;
                double ratio1;
                double ratio2;

		if (max_value != 0)
		{
			ratio  = value  / max_value;
			ratio1 = value1 / max_value;
			ratio2 = value2 / max_value;
		}
		else 
			ratio = ratio1 = ratio2 = 0.0;

                const double mn = MIN ( MIN ( value, value1 ), value2 );
                const double mx = MAX ( MAX ( value, value1 ), value2 );
                double md;
                if (value != mn && value != mx) { md=value; }
                else if (value1 != mn && value1 != mx) { md=value1; }
                else if (value2 != mn &&  value2 != mx) { md=value2; }
                else if (value == value1 || value == value2) { md = value; }
                else { md = value1; }

                double curRatioMn = 0.0;
                double curRatioMd = 0.0;
                double curRatioMx = 0.0;

		////////////////////////////////////////////////////////////
                if ( mn == value )
                {
                        curRatioMn = ratio;
                        XSetForeground ( XtDisplay(_w), _gc, colorW.pixel );
                }
                else if ( mn == value1 )
                {
                        curRatioMn = ratio1;
                        XSetForeground ( XtDisplay(_w), _gc, colorW.pixel );
                }

                else if ( mn == value2 )
                {
                        curRatioMn = ratio2;
                        XSetForeground ( XtDisplay(_w), _gc, colorW.pixel );
                }
                else cerr << "Something is very wrong !" << endl;

		if (_hor == HORIZONTAL)
		   if ( _verAxisDir == DESC)
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                0,
                                int (i*ppb),
                                int (ceil(_width*(curRatioMn-0))),
                                int (ceil(ppb)) );
		   else
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                0,
                                int (_height - ((i+1)*ppb)),
                                int (ceil(_width*(curRatioMn-0))),
                                int (ceil(ppb)) );
		else
                	XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*ppb), 
				int (_height * (1 - curRatioMn)),
                                int (ceil(ppb)),
				int (ceil(_height*curRatioMn)) );

		////////////////////////////////////////////////////////////
                if ( md == value )
                {
                        curRatioMd = ratio;
                        if (curRatioMn == ratio1) 
				XSetForeground ( XtDisplay(_w), _gc, colorM.pixel );
                        else 
				XSetForeground ( XtDisplay(_w), _gc, colorY.pixel );
                }
                else if ( md == value1 )
                {
                        curRatioMd = ratio1;
                        if (curRatioMn == ratio) 
				XSetForeground ( XtDisplay(_w), _gc, colorC.pixel );
                        else 
				XSetForeground ( XtDisplay(_w), _gc, colorY.pixel );
                }
                else if ( md == value2 )
                {
                        curRatioMd = ratio2;
                        if (curRatioMn == ratio) 
				XSetForeground ( XtDisplay(_w), _gc, colorC.pixel );
                        else 
				XSetForeground ( XtDisplay(_w), _gc, colorM.pixel );
                }
                else cerr << "Something is very wrong 2!" << endl;

		if (_hor == HORIZONTAL)
		   if ( _verAxisDir == DESC )
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMn),
                                int (i*ppb),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(ppb)) );
		   else 
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int(_width*curRatioMn),
                                int (_height - ((i+1)*ppb)),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(ppb)) );
                else 
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                               	int (i*ppb),
				int (_height * (1 - curRatioMd)),
                               	int (ceil(ppb)),
				int (ceil(_height*(curRatioMd - curRatioMn))) );

		////////////////////////////////////////////////////////////
                if ( mx == value )
                {
                        curRatioMx = ratio;
                        XSetForeground ( XtDisplay(_w), _gc, colorR.pixel );
                }
                else if ( mx == value1 )
                {
                        curRatioMx = ratio1;
                        XSetForeground ( XtDisplay(_w), _gc, colorG.pixel );
                }
                else if ( mx == value2 )
                {
                        curRatioMx = ratio2;
                        XSetForeground ( XtDisplay(_w), _gc, colorB.pixel );
                }
                else cerr << "Something is very wrong 3!" << endl;

                if (_hor == HORIZONTAL) 
		    if ( _verAxisDir == DESC )
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMd),
                                int(i*ppb),
                                int(ceil(_width*(curRatioMx - curRatioMd))),
                                int(ceil(ppb)) );
		    else 
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMd),
                                _height - (int((i+1)*ppb)),
                                int(ceil(_width*(curRatioMx - curRatioMd))),
                                int(ceil(ppb)) );
		else 
			XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int(i*ppb), 
				int(_height * (1 - curRatioMx)),
                                int(ceil(ppb)), 
				int(ceil(_height*(curRatioMx-curRatioMd))) );
        }

	if (_drawInsideTicks == True) {
		drawInsideTicks(max_value, ppb, nbins);
	}
}


/////////////////////////////////////////////////////////////////////////
// These macros define code that repeats over and over
/////////////////////////////////////////////////////////////////////////

#define DRAW_HOR_LOG_TICK( a ) 					\
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc, 		\
		int(_width*r), 0, int(_width*r), a); 		\
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
		int(_width*r), _height-1-a, 			\
		int(_width*r), _height-1);

#define DRAW_VER_LOG_TICK( a ) 					\
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
		0, (int)(_height-1-(_height*r)),		\
		a, (int)(_height-1-(_height*r)) );		\
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
		_width-1, (int)(_height-1-(_height*r)),		\
		_width-1-a, (int)(_height-1-(_height*r)) );

#define DRAW_BIN_TICK( i, interval, length )				\
        for (i = 0; i < numBins; i+=interval) {				\
            if (_hor == HORIZONTAL) {					\
                if ( _verAxisDir == DESC ) {				\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                0, (int)(i*ppb),			\
                                length, (int)(i*ppb) );			\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                _width-1, (int)(i*ppb),			\
                                _width-1-length, (int)(i*ppb) );	\
                }							\
                else {							\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                0, (int)(_height-i*ppb),		\
                                length, (int)(_height-i*ppb) );		\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                _width-1, (int)(_height-i*ppb),		\
                                _width-1-length, (int)(_height-i*ppb));	\
                }							\
            }								\
            else {							\
                XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
                                (int)(i*ppb), _height-1,		\
                                (int)(i*ppb), _height-1-length );	\
                XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
                                (int)(i*ppb), 0,			\
                                (int)(i*ppb), length );			\
            }								\
        }


/////////////////////////////////////////////////////////////////////////
// drawInsideTicks: Draw tick marks on the edge of the histogram 
// display.  Note that ticks specifying the DN count are displayed only 
// if logarithmic scale was used to draw a histogram.  Otherwise only 
// ticks along the bin axis are displayed.  Also note that ticks are 
// drawn on both left and right (top and bottom) edges.
/////////////////////////////////////////////////////////////////////////
void HistGraphView::drawInsideTicks(double maxValue, double ppb, int numBins)
{
	XColor exact, color;
	Colormap cmap = DefaultColormap ( XtDisplay(_w),
                                          DefaultScreen ( XtDisplay(_w) ) );
	XAllocNamedColor ( XtDisplay(_w), cmap, _insideTickColor, &color, &exact);
	XSetForeground ( XtDisplay(_w), _gc, color.pixel );

	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				0, 0, _width-1, 0 );
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				0, 0, 0, _height-1 );
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				0, _height-1, _width-1, _height-1 );
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				_width-1, 0, _width-1, _height-1 );


	if (_log) {
	    	for (int i = 0; i < maxValue; i++) {
			double r; 

			// Draw long ticks at 10^i
			r = log10(1 * pow(10.0, (double)i)) / maxValue;
			if (_hor == HORIZONTAL) {
				DRAW_HOR_LOG_TICK(_insideTickLength);
			}
			else {
				DRAW_VER_LOG_TICK(_insideTickLength);
			}

			// Draw minor ticks at 2*10^i, 4*10^i, 6*10^i, 8*10^i
			for (int j = 2; j < 10; j+=2) {
				r = log10(j * pow(10.0, (double)i)) / maxValue;
				if (_hor == HORIZONTAL) {
					DRAW_HOR_LOG_TICK(_insideMinorTickLength);
				}
				else {
					DRAW_VER_LOG_TICK(_insideMinorTickLength);
				}
			}
		}
	}

	int i;
	DRAW_BIN_TICK(i, _insideTickInterval, _insideTickLength);
	DRAW_BIN_TICK(i, _insideMinorTickInterval, _insideMinorTickLength);
}
