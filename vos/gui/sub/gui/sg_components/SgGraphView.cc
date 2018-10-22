//////////////////////////////////////////////////////////////////////////
// SgGraphView.cc:  This is a view component that draws one or three 
// graphs.
//////////////////////////////////////////////////////////////////////////
#include "SgGraphView.h"
#include "ErrorManager.h"
#include <Xm/DrawingA.h>
#include <iostream>
using namespace std;
#include <stdio.h>
#include <math.h>

#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif

// Resources for this class

XtResource SgGraphView::_resources [ ] = {
 {
    (char *)"blended",
    (char *)"Blended",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgGraphView *, _blended ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)"horizontal",
    (char *)"Horizontal",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgGraphView *, _horizontal ),
    XmRString,
    ( XtPointer ) "FALSE",
 },
 {
    (char *)"ascending",
    (char *)"Ascending",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgGraphView *, _ascending ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)"spike",
    (char *)"Spike",
    XmRInt,
    sizeof ( int ),
    XtOffset ( SgGraphView *, _spike ),
    XmRString,
    ( XtPointer ) "1",
 },
 {
    (char *)"redColor",
    (char *)"RedColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _red ),
    XmRImmediate,
    ( XtPointer ) "red",
 },
 {
    (char *)"greenColor",
    (char *)"GreenColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _green ),
    XmRImmediate,
    ( XtPointer ) "green",
 },
 {
    (char *)"blueColor",
    (char *)"BlueColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _blue ),
    XmRImmediate,
    ( XtPointer ) "blue",
 },
 {
    (char *)"magentaColor",
    (char *)"MagentaColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _magenta ),
    XmRImmediate,
    ( XtPointer ) "magenta",
 },
 {
    (char *)"yellowColor",
    (char *)"YellowColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _yellow ),
    XmRImmediate,
    ( XtPointer ) "yellow",
 },
 {
    (char *)"cyanColor",
    (char *)"CyanColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _cyan ),
    XmRImmediate,
    ( XtPointer ) "cyan",
 },
 {
    (char *)"whiteColor",
    (char *)"WhiteColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _white ),
    XmRImmediate,
    ( XtPointer ) "white",
 },
 {
    (char *)"filled",
    (char *)"Filled",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgGraphView *, _filled ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)"maxValue",
    (char *)"MaxValue",
    XmRFloat,
    sizeof ( float ),
    XtOffset ( SgGraphView *, _maxUserValue ),
    XmRString,
    ( XtPointer ) "-1",
 },
 {
    (char *)"topMargin",
    (char *)"TopMargin",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgGraphView *, _topMargin ),
    XmRString,
    ( XtPointer ) "0",
 },
 {
    (char *)"logScale",
    (char *)"LogScale",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgGraphView *, _log ),
    XmRString,
    ( XtPointer ) "FALSE",
 },
 {
    (char *)"drawInsideTicks",
    (char *)"DrawInsideTicks",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgGraphView *, _drawInsideTicks ),
    XmRString,
    ( XtPointer ) "FALSE",
 },
 {
    (char *)"insideTickColor",
    (char *)"InsideTickColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgGraphView *, _insideTickColor ),
    XmRImmediate,
    ( XtPointer ) "white",
 },
 {
    (char *)"insideMajorTickLength", 
    (char *)"InsideMajorTickLength",
    XmRInt, 
    sizeof ( int ),
    XtOffset ( SgGraphView *, _insideTickLength ),
    XmRString,
    ( XtPointer ) "10", 
 },
 {
    (char *)"insideMinorTickLength",
    (char *)"InsideMinorTickLength",
    XmRInt,
    sizeof ( int ),
    XtOffset ( SgGraphView *, _insideMinorTickLength ),
    XmRString,
    ( XtPointer ) "5",
 },
 {
    (char *)"insideTickInterval",
    (char *)"InsideTickInterval",
    XmRInt,
    sizeof ( int ),
    XtOffset ( SgGraphView *, _insideTickInterval ),
    XmRString,
    ( XtPointer ) "64",
 },
 {
    (char *)"insideMinorTickInterval",
    (char *)"InsideMinorTickInterval",
    XmRInt,
    sizeof ( int ),
    XtOffset ( SgGraphView *, _insideMinorTickInterval ),
    XmRString,
    ( XtPointer ) "16",
 },
};

String SgGraphView::_defaults[] = {
    (char *)".height:           256",
    (char *)".width:            256",
    (char *)"*background:	black",
     NULL,
};

SgGraphView::SgGraphView ( Widget parent, const char *name, 
		float *r, float *g, float *b, int numElements )
	: UIComponent (name)
{
    setDefaultResources ( parent, _defaults );

    _numElements = numElements;
    _singleGraph = FALSE;

    // Allocate memory for three arrays

    int i;
    for ( i = 0; i < 3; i++) {
	_dataSet[i] = new float [numElements];
	if ( _dataSet[i] == NULL )
	    theErrorManager->process ( Error, "SgGraphView", 
				       "Memory allocation error" );
    }

    // Make local copy of arrays.  If any of the arrays is NULL, 
    // fill it with zeros

    if ( r ) {
	for ( i = 0; i < numElements; i++ )
	    _dataSet[0][i] = r[i];
    }
    else {
	for ( i = 0; i < numElements; i++ )
	    _dataSet[0][i] = 0.0;
    }

    if ( g ) {
        for ( i = 0; i < numElements; i++ )
            _dataSet[1][i] = g[i];
    }
    else {
        for ( i = 0; i < numElements; i++ )
            _dataSet[1][i] = 0.0;
    }

    if ( b ) {
        for ( i = 0; i < numElements; i++ )
            _dataSet[2][i] = b[i];
    }
    else {
        for ( i = 0; i < numElements; i++ )
            _dataSet[2][i] = 0.0;
    }

    _w  = XtVaCreateWidget ( _name,
			     xmDrawingAreaWidgetClass, parent,
			     NULL );
    installDestroyHandler();

    getResources ( _resources, XtNumber ( _resources ) );

    XtAddCallback ( _w, XmNexposeCallback,
		    &SgGraphView::displayCallback,
		    ( XtPointer ) this );

    // Allocate private GC 

    _gc = XCreateGC ( XtDisplay ( _w ), 
                RootWindowOfScreen ( XtScreen ( _w ) ),
                0L, NULL );

    allocateColors();

    _maxValue = maxSpikeValue();
}

SgGraphView::SgGraphView ( Widget parent, const char *name,
                float *ds, int numElements )
        : UIComponent (name)
{
    setDefaultResources ( parent, _defaults );

    _numElements = numElements;
    _singleGraph = TRUE;

    _dataSet[0] = new float [_numElements];
    if ( _dataSet[0] == NULL )
        theErrorManager->process ( Error, "SgGraphView", "Memory allocation error" );
    for ( int i = 0; i < _numElements; i++)
	_dataSet[0][i] = ds[i];

    _dataSet[1] = NULL;
    _dataSet[2] = NULL;

    _w  = XtVaCreateWidget ( _name,
                             xmDrawingAreaWidgetClass, parent,
                             NULL );
    installDestroyHandler();

    getResources ( _resources, XtNumber ( _resources ) );

    XtAddCallback ( _w, XmNexposeCallback,
                    &SgGraphView::displayCallback,
                    ( XtPointer ) this );

    // Allocate private GC

    _gc = XCreateGC ( XtDisplay ( _w ),
                RootWindowOfScreen ( XtScreen ( _w ) ),
                0L, NULL );
    allocateColors();
}

SgGraphView::SgGraphView ( Widget parent, const char *name) : UIComponent (name)
{
    setDefaultResources ( parent, _defaults );

    _numElements = 0;
    _singleGraph = TRUE;

    for ( int i = 0; i < 3; i++)
	_dataSet[i] = NULL;

    _w  = XtVaCreateWidget ( _name,
			     xmDrawingAreaWidgetClass, parent,
			     NULL );
    installDestroyHandler();

    getResources ( _resources, XtNumber ( _resources ) );

    XtAddCallback ( _w, XmNexposeCallback,
		&SgGraphView::displayCallback,
		( XtPointer ) this );

    // Allocate private GC

    _gc = XCreateGC ( XtDisplay ( _w ),
		RootWindowOfScreen ( XtScreen ( _w ) ),
		0L, NULL );
    allocateColors();
}

SgGraphView::~SgGraphView()
{
    for ( int i = 0; i < 3; i++)
	if ( _dataSet[i] )
		delete [] _dataSet[i];

    XFreeGC ( XtDisplay ( _w ), _gc );
}

void SgGraphView::displayCallback ( Widget, XtPointer clientData, XtPointer )
{
    SgGraphView *obj = ( SgGraphView * ) clientData;
    obj->display();
}

void SgGraphView::display()
{
    if ( !XtIsRealized ( _w ) )
	return;

    // Make any resize cause expose event

    XSetWindowAttributes attrs;
    attrs.bit_gravity = ForgetGravity;
    XChangeWindowAttributes ( XtDisplay(_w), XtWindow(_w), 
			      CWBitGravity, &attrs );

    // Get current size

    XtVaGetValues ( _w,
		    XmNwidth,  &_width,
		    XmNheight, &_height,
		    NULL );

    // Always clear the window before drawing

    XClearWindow ( XtDisplay ( _w ), XtWindow ( _w ) );

    if ( _singleGraph ) {
	if ( _filled )
	    displayOneFilled();
	else
	    displayOneEmpty();
    }
    else {
	if ( !_filled )
	    displayThreeEmpty();
	else {
	    if ( _blended )
		displayThreeBlended();
	    else 
		displayThreeStacked();
	}
    }
}

void SgGraphView::allocateColors()
{
    Colormap cmap = DefaultColormap ( XtDisplay ( _w ),
				DefaultScreen ( XtDisplay ( _w ) ) );

    XColor exact;

    XAllocNamedColor ( XtDisplay(_w), cmap, _red,     &_colorR, &exact );
    XAllocNamedColor ( XtDisplay(_w), cmap, _green,   &_colorG, &exact );
    XAllocNamedColor ( XtDisplay(_w), cmap, _blue,    &_colorB, &exact );
    XAllocNamedColor ( XtDisplay(_w), cmap, _magenta, &_colorM, &exact );
    XAllocNamedColor ( XtDisplay(_w), cmap, _cyan,    &_colorC, &exact );
    XAllocNamedColor ( XtDisplay(_w), cmap, _yellow,  &_colorY, &exact );
    XAllocNamedColor ( XtDisplay(_w), cmap, _white,   &_colorW, &exact );
}

void SgGraphView::setDrawingColor ( XColor color )
{
    XSetForeground ( XtDisplay(_w), _gc, color.pixel );
}

float SgGraphView::spike ( float *ds, int size, int spikeCount )
{
    float oldMaxValue = -1.0;
    for ( int i = 0; i < spikeCount; i++ ) {
	float maxValue = -1.0;
	for ( int j = 0; j < size; j++) {
	    if ( ( ds[j] > maxValue ) && ( oldMaxValue < 0 || ds[j] < oldMaxValue ) )
		maxValue = ds[j];
	}
	if ( maxValue >= 0 )
	    oldMaxValue = maxValue;
    }
    return oldMaxValue;
} 

void SgGraphView::displayOneFilled()
{
    if ( _dataSet[0] == NULL ) return;

    setDrawingColor(_colorR);
    float maxValue = spike ( _dataSet[0], _numElements, _spike );
    if ( _log && (maxValue > 0.0) )
	    maxValue = log10(maxValue);

    if ( maxValue < 1.0 ) maxValue = 1.0;

    float bw;				// bin width in pixels
    if ( _horizontal == FALSE )
	bw = (double)_width / (double)_numElements;
    else
	bw = (double)_height / (double)_numElements;

    for ( int i = 0; i < _numElements; i++) {
	float ratio = _dataSet[0][i] / maxValue;
	if ( _log && (_dataSet[0][i] > 0.0) )
	    ratio = log10(_dataSet[0][i]) / maxValue;

	int x, y, width, height; 
        if (_horizontal == TRUE) {
	    x = 0;
	    if ( _ascending == FALSE ) {		// Descending axis
		y = int(i * bw);
		width = int(_width * ratio) + 1;
		height = int(bw) + 1;
	    }
	    else {				// Ascending axis
		y = int (_height - ((i+1)*bw));
		width = int(_width * ratio) + 1;
		height = int(bw) + 1;
	    }
	}
	else {
	    x = int(i*bw);
	    y = int(_height * (1 - ratio));
	    width = int(bw) + 1;
	    height = int(_height*ratio) + 1;
	}

	XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
			 x, y, width, height);
    }

    if (_drawInsideTicks == True)
	drawInsideTicks ( maxValue, bw, _numElements );
}

void SgGraphView::displayOneEmpty()
{
    XPoint *graph = new XPoint [_numElements * 2];
    float horScale = (float)_width / (float)_numElements;
    float verScale = (float)_height / arrayMax(_dataSet[0], _numElements);

    for ( int i = 0; i < _numElements; i++) {
	graph[i*2].x   = (int)(i * horScale);
	graph[i*2+1].x = (int)((i+1) * horScale);
	int y = _height - (int)(_dataSet[0][i] * verScale);
	if ( _log && (_dataSet[0][i] > 0.0) ) 
	    y = _height - (int)(log10(_dataSet[0][i]) * verScale);
	graph[i*2].y   = y;
	graph[i*2+1].y = y;
    }
    setDrawingColor(_colorR);
    XDrawLines ( XtDisplay(_w), XtWindow(_w), 
		 _gc, graph, _numElements*2, CoordModeOrigin );
    delete [] graph;
}

void SgGraphView::displayThreeEmpty()
{
    XPoint *graph = new XPoint [_numElements * 2];
    float horScale = (float)_width / (float)_numElements;

    float rMax = arrayMax(_dataSet[0], _numElements);
    float gMax = arrayMax(_dataSet[1], _numElements);
    float bMax = arrayMax(_dataSet[2], _numElements);

    float rgbMax = MAX ( MAX ( rMax, gMax ), bMax );

    if ( _log && (rgbMax > 0.0) ) rgbMax = log10 ( rgbMax );

    float verScale = (float)_height / rgbMax;

    // Draw red

    int i, y;
    for ( i = 0; i < _numElements; i++) {
	graph[i*2].x   = (int)(i * horScale);
	graph[i*2+1].x = (int)((i+1) * horScale);
	y = _height - (int)(_dataSet[0][i] * verScale);
	if ( _log && (_dataSet[0][i] > 0.0) ) 
	    y = _height - (int)(log10(_dataSet[0][i]) * verScale);
	graph[i*2].y   = y;
	graph[i*2+1].y = y;
    }
    setDrawingColor(_colorR);
    XDrawLines ( XtDisplay(_w), XtWindow(_w),
		 _gc, graph, _numElements*2, CoordModeOrigin );

    // Draw green

    for ( i = 0; i < _numElements; i++) {
	y = _height - (int)(_dataSet[1][i] * verScale);
	if ( _log && (_dataSet[1][i] > 0.0) ) 
	    y = _height - (int)(log10(_dataSet[1][i]) * verScale);
	graph[i*2].y   = y;
	graph[i*2+1].y = y;
    }
    setDrawingColor(_colorG);
    XDrawLines ( XtDisplay(_w), XtWindow(_w),
		 _gc, graph, _numElements*2, CoordModeOrigin );

    // Draw blue

    for ( i = 0; i < _numElements; i++) {
	y = _height - (int)(_dataSet[2][i] * verScale);
	if ( _log && (_dataSet[2][i] > 0.0) ) 
	    y = _height - (int)(log10(_dataSet[2][i]) * verScale);
	graph[i*2].y   = y;
	graph[i*2+1].y = y;
    }
    setDrawingColor(_colorB);
    XDrawLines ( XtDisplay(_w), XtWindow(_w),
		_gc, graph, _numElements*2, CoordModeOrigin );
}

/////////////////////////////////////////////////////////////////////////
// Display histogram using "Stacked Colors" method - for color images only.
// At each bin, the one with the max count goes into the back, and the one 
// with the min count goes in the front of the display.
/////////////////////////////////////////////////////////////////////////
void SgGraphView::displayThreeStacked()
{
    float bw;                           // bin width in pixels
    if ( _horizontal == FALSE )
	bw = (float)_width / (float)_numElements;
    else
	bw = (float)_height / (float)_numElements;

    float maxValue = _maxValue;
    if ( _log && (_maxValue > 0.0) ) 
	maxValue = log10(_maxValue);

    for (int i = 0; i < _numElements; i++) {

	float redValue, grnValue, bluValue;
	if ( _log ) {
	    if ( _dataSet[0][i] )
		redValue = log10(_dataSet[0][i]);
	    else 
		redValue = _dataSet[0][i];
	    if ( _dataSet[1][i] )
		grnValue = log10(_dataSet[1][i]);
	    else 
		grnValue = _dataSet[1][i];
	    if ( _dataSet[2][i] )
		bluValue = log10(_dataSet[2][i]);
	    else 
		bluValue = _dataSet[2][i];
	}
	else {
	    redValue = _dataSet[0][i];
	    grnValue = _dataSet[1][i];
	    bluValue = _dataSet[2][i];
	}

	float ratioRed = redValue / maxValue;
	float ratioGrn = grnValue / maxValue;
	float ratioBlu = bluValue / maxValue;

	// identify the highest, lowest, and middle points in this bin
        const float mn = MIN ( MIN ( redValue, grnValue ), 
			       bluValue );
        const float mx = MAX ( MAX ( redValue, grnValue ), 
			       bluValue );
        const float md = midOfThree ( redValue, grnValue, bluValue );

        float curRatioMn = 0.0;
        float curRatioMd = 0.0;
        float curRatioMx = 0.0;

	// Draw the lower (minimum) portion of the histogram
        if ( mn == redValue ) {
	    curRatioMn = ratioRed;
            setDrawingColor ( _colorR );
        }
        else if ( mn == grnValue ) {
            curRatioMn = ratioGrn;
	    setDrawingColor ( _colorG );
	}
        else if ( mn == bluValue ) {
	    curRatioMn = ratioBlu;
	    setDrawingColor ( _colorB );
        }
        else cerr << "Error: mn must be defined!" << endl;

	if ( _horizontal == TRUE ) {
	    if ( _ascending == FALSE )
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
				0,
				int (i*bw),
				int (ceil(_width*curRatioMn)),
				int (ceil(bw)) );
	    else
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                0,
                                int (_height - ((i+1)*bw)),
                                int (ceil(_width*curRatioMn)),
                                int (ceil(bw)) );
	}
	else
	    XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*bw), 
				int (_height * (1 - curRatioMn)),
                                int (ceil(bw)),  
				int (ceil(_height*curRatioMn)) );


        // Draw the middle portion of the histogram
	if ( md == redValue ) {
	    curRatioMd = ratioRed;
	    setDrawingColor ( _colorR );
        }
        else if ( md == grnValue ) {
	    curRatioMd = ratioGrn;
	    setDrawingColor ( _colorG );
        }
        else if ( md == bluValue ) {
	    curRatioMd = ratioBlu;
	    setDrawingColor ( _colorB );
        }
        else cerr << "SgGraphView: md must be defined!" << endl;

	if ( _horizontal == TRUE )
	    if ( _ascending == FALSE )
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMn),
                                int (i*bw),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(bw)) );
	    else
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMn),
                                int (_height - int((i+1)*bw)),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(bw)) );
	else
	    XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*bw), 
				int (_height * (1 - curRatioMd)),
                                int (ceil(bw)),
				int (ceil(_height*curRatioMd - _height*curRatioMn)) );


        // Draw the top (max) portion of the histogram
	if ( mx == redValue ) {
	    curRatioMx = ratioRed;
	    setDrawingColor ( _colorR );
        }
        else if ( mx == grnValue ) {
	    curRatioMx = ratioGrn;
	    setDrawingColor ( _colorG );
	}
        else if ( mx == bluValue ) {
	    curRatioMx = ratioBlu;
	    setDrawingColor ( _colorB );
        }
        else cerr << "Error: there must be a max point!" << endl;

	if ( _horizontal == TRUE ) 
	    if ( _ascending == FALSE )
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMd),
                                int (i*bw),
                                int (ceil(_width*(curRatioMx - curRatioMd))),
                                int (ceil(bw)) );
	    else
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width * curRatioMd),
                                int (_height - int((i+1)*bw)),
                                int (ceil(_width * (curRatioMx - curRatioMd))),
                                int (ceil(bw)) );
	else 
	    XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*bw), 
				int (_height * (1 - curRatioMx)),
                                int (ceil(bw)), 
				int (ceil(_height*curRatioMx-_height*curRatioMd)) );
    }

    if (_drawInsideTicks == TRUE) {
	drawInsideTicks(maxValue, bw, _numElements);
    }
}

/////////////////////////////////////////////////////////////////////////
// Blended Colors Method
//   Color arithmetics:
//	R + G + B = White
//	R + G = Yellow
//	R + B = Magenta
//	B + G = Cyan
/////////////////////////////////////////////////////////////////////////
void SgGraphView::displayThreeBlended()
{
    double bw;
    if (_horizontal == FALSE )
	bw = (float)_width / (float)_numElements;
    else
	bw = (float)_height / (float)_numElements;

    float maxValue = _maxValue;
    if ( _log && (_maxValue > 0.0) ) maxValue = log10(_maxValue);

    for (int i = 0; i < _numElements; i++) {


	float redValue, grnValue, bluValue;
        if ( _log ) {
            if ( _dataSet[0][i] )
                redValue = log10(_dataSet[0][i]);
            else
                redValue = _dataSet[0][i];
            if ( _dataSet[1][i] )
                grnValue = log10(_dataSet[1][i]);
            else
                grnValue = _dataSet[1][i];
            if ( _dataSet[2][i] )
                bluValue = log10(_dataSet[2][i]);
            else
                bluValue = _dataSet[2][i];
        }
	else {
	    redValue = _dataSet[0][i];
	    grnValue = _dataSet[1][i];
	    bluValue = _dataSet[2][i];
	}

	float ratioRed = redValue / maxValue;
	float ratioGrn = grnValue / maxValue;
	float ratioBlu = bluValue / maxValue;

	// Identify the highest, lowest, and middle points in this bin

	const float mn = MIN ( MIN ( redValue, grnValue ), 
			       bluValue );
        const float mx = MAX ( MAX ( redValue, grnValue ), 
                               bluValue );
        const float md = midOfThree ( redValue, grnValue, bluValue );

	float curRatioMn;
	float curRatioMd;
	float curRatioMx;

	// Draw the lower (minimum) portion of the histogram
	if ( mn == redValue ) {
	    curRatioMn = ratioRed;
	    setDrawingColor ( _colorW );
	}
	else if ( mn == grnValue ) {
	    curRatioMn = ratioGrn;
	    setDrawingColor ( _colorW );
	}
	else if ( mn == bluValue ) {
	    curRatioMn = ratioBlu;
	    setDrawingColor ( _colorW );
	}
	else {
	    char buf1[10];
	    sprintf ( buf1, "Can't locate min, skipping bin %d", i);
	    theErrorManager->process ( Error, "PlotView", buf1, "Location 1" );
	    continue;
	}

	if ( _horizontal == TRUE ) {
	    if ( _ascending == FALSE )
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                0,
                                int (i*bw),
                                int (ceil(_width*(curRatioMn-0))),
                                int (ceil(bw)) );
	    else
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                0,
                                int (_height - ((i+1)*bw)),
                                int (ceil(_width*(curRatioMn-0))),
                                int (ceil(bw)) );
	}
	else
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (i*bw), 
				int (_height * (1 - curRatioMn)),
                                int (ceil(bw)),
				int (ceil(_height*curRatioMn)) );


	// Draw the middle portion of the histogram

	if ( md == redValue ) {
	    curRatioMd = ratioRed;
	    if (curRatioMn == ratioGrn)
		setDrawingColor ( _colorM );
	    else
		setDrawingColor ( _colorY );
	}
	else if ( md == grnValue ) {
	    curRatioMd = ratioGrn;
	    if (curRatioMn == ratioRed)
		setDrawingColor ( _colorC );
	    else
		setDrawingColor ( _colorY );
	}
	else if ( md == bluValue ) {
	    curRatioMd = ratioBlu;
	    if (curRatioMn == ratioRed)
		setDrawingColor ( _colorC );
	    else
		setDrawingColor ( _colorM );
	}
        else {
            char buf2[10];
            sprintf ( buf2, "Can't locate mid, skipping bin %d", i );
            theErrorManager->process ( Error, "PlotView", buf2, "Location 2" );
	    continue;
        }

	if ( _horizontal == TRUE )
	    if ( _ascending == FALSE )
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int (_width*curRatioMn),
                                int (i*bw),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(bw)) );
	    else 
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int(_width*curRatioMn),
                                int (_height - ((i+1)*bw)),
                                int (ceil(_width*(curRatioMd - curRatioMn))),
                                int (ceil(bw)) );
	else 
	    XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                             int (i*bw),
			     int (_height * (1 - curRatioMd)),
                             int (ceil(bw)),
			     int (ceil(_height*(curRatioMd - curRatioMn))) );

	// Draw the top (max) portion of the histogram
	if ( mx == redValue ) {
	    curRatioMx = ratioRed;
	    setDrawingColor ( _colorR );
	}
	else if ( mx == grnValue ) {
	    curRatioMx = ratioGrn;
	    setDrawingColor ( _colorG );
	}
	else if ( mx == bluValue ) {
	    curRatioMx = ratioBlu;
	    setDrawingColor ( _colorB );
	}
        else {
            char buf3[10];
            sprintf ( buf3, "Can't locate mid, skipping bin %d", i );
            theErrorManager->process ( Error, "PlotView", buf3, "Location 3" );
            continue;
        }

	if ( _horizontal == TRUE ) 
	    if ( _ascending == FALSE )
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int(_width * curRatioMd),
                                int(i * bw),
                                int(ceil(_width*(curRatioMx - curRatioMd))),
                                int(ceil(bw)) );
	    else 
		XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                                int(_width * curRatioMd),
                                _height - (int((i+1)*bw)),
                                int(ceil(_width*(curRatioMx - curRatioMd))),
                                int(ceil(bw)) );
	else 
	    XFillRectangle ( XtDisplay(_w), XtWindow(_w), _gc,
                             int(i * bw), 
			     int(_height * (1 - curRatioMx)),
                             int(ceil(bw)), 
			     int(ceil(_height*(curRatioMx-curRatioMd))) );
    }

    if (_drawInsideTicks == True) {
	drawInsideTicks(maxValue, bw, _numElements);
    }
}

float SgGraphView::maxSpikeValue()
{
    float sred, sgrn, sblu;
    sred = spike ( _dataSet[0], _numElements, _spike );
    sgrn = spike ( _dataSet[1], _numElements, _spike );
    sblu = spike ( _dataSet[2], _numElements, _spike );

    float maxValue = (float)MAX(MAX(sred, sgrn), sblu);
    if ( maxValue < 1.0 ) maxValue = 1.0;

    return maxValue;
}

void SgGraphView::setDataSet ( float *a, int numElements )
{
    _singleGraph = TRUE;
    _numElements = numElements;

    for ( int j = 0; j < 3; j++ ) {
	if (_dataSet[j]) delete [] _dataSet[j];
	_dataSet[j] = new float [_numElements];
    }

    for ( int i = 0; i < _numElements; i++ ) {
	_dataSet[0][i] = a[i];
	_dataSet[1][i] = 0;
	_dataSet[2][i] = 0;
    }
}

void SgGraphView::setDataSets ( float *r, float *g, float *b, int numElements )
{
    _singleGraph = FALSE;
    _numElements = numElements;

    for ( int j = 0; j < 3; j++ ) {
	if (_dataSet[j]) delete [] _dataSet[j];
	_dataSet[j] = new float [_numElements];
    }

    for ( int i = 0; i < _numElements; i++ ) {
	_dataSet[0][i] = r[i];
	_dataSet[1][i] = g[i];
	_dataSet[2][i] = b[i];
    }

    _maxValue = maxSpikeValue();

    display();
}

////////////////////////////////////////////////////////////////////////////
// setSpike: set the spike value and redisplay the plot
////////////////////////////////////////////////////////////////////////////
void SgGraphView::setSpike ( int spike )
{
    _spike = spike;
    _maxValue = maxSpikeValue();
    display();
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
		0, int(_height-1-(_height*r)),			\
		a, int(_height-1-(_height*r)) );		\
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
		_width-1, int(_height-1-(_height*r)),		\
		_width-1-a, int(_height-1-(_height*r)) );

#define DRAW_BIN_TICK( i, interval, length )				\
        for (i = 0; i < numBins; i+=interval) {				\
            if (_horizontal == TRUE) {					\
                if ( _ascending == FALSE ) {				\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                0, int(i*bw),				\
                                length, int(i*bw) );			\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                _width-1, int(i*bw),			\
                                _width-1-length, int(i*bw) );		\
                }							\
                else {							\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                0, int(_height-i*bw),			\
                                length, int(_height-i*bw) );		\
                    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,	\
                                _width-1, int(_height-i*bw),		\
                                _width-1-length, int(_height-i*bw) );	\
                }							\
            }								\
            else {							\
                XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
                                int(i*bw), _height-1,			\
                                int(i*bw), _height-1-length );		\
                XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,		\
                                int(i*bw), 0,				\
                                int(i*bw), length );			\
            }								\
        }


/////////////////////////////////////////////////////////////////////////
// drawInsideTicks: Draw tick marks on the edge of the histogram 
// display.  Note that ticks specifying the DN count are displayed only 
// if logarithmic scale was used to draw a histogram.  Otherwise only 
// ticks along the bin axis are displayed.  Also note that ticks are 
// drawn on both left and right (top and bottom) edges.
/////////////////////////////////////////////////////////////////////////
void SgGraphView::drawInsideTicks(double maxValue, double bw, int numBins)
{
    if ( _log == FALSE )
	return;

    Colormap cmap = DefaultColormap ( XtDisplay(_w),
				DefaultScreen ( XtDisplay(_w) ) );
    XColor color, exact;
    XAllocNamedColor ( XtDisplay(_w), cmap, _insideTickColor, &color, &exact );
    setDrawingColor ( color );

    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				0, 0, _width-1, 0 );
    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				0, 0, 0, _height-1 );
    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				0, _height-1, _width-1, _height-1 );
    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
				_width-1, 0, _width-1, _height-1 );

    int i;
    for (i = 0; i < maxValue; i++) {
	double r; 

	// Draw long ticks at 10^i
	r = log10(1 * pow(10.0, (double)i)) / maxValue;
	if (_horizontal == TRUE) {
		DRAW_HOR_LOG_TICK(_insideTickLength);
	}
	else {
		DRAW_VER_LOG_TICK(_insideTickLength);
	}

	// Draw minor ticks at 2*10^i, 4*10^i, 6*10^i, 8*10^i
	for (int j = 2; j < 10; j+=2) {
	    r = log10(j * pow(10.0, (double)i)) / maxValue;
	    if ( _horizontal == TRUE ) {
		DRAW_HOR_LOG_TICK(_insideMinorTickLength);
	    }
	    else {
		DRAW_VER_LOG_TICK(_insideMinorTickLength);
	    }
	}
    }

    DRAW_BIN_TICK(i, _insideTickInterval, _insideTickLength);
    DRAW_BIN_TICK(i, _insideMinorTickInterval, _insideMinorTickLength);
}

float SgGraphView::midOfThree ( float a, float b, float c)
{
    if ( a <= b && a >= c ) return a;
    if ( a <= c && a >= b ) return a;
    if ( b <= a && b >= c ) return b;
    if ( b <= c && b >= a ) return b;
    if ( c <= a && c >= b ) return c;
    if ( c <= b && c >= a ) return c;
    return b;		// just to shut up compiler
}

float SgGraphView::arrayMax ( float *a, int size )
{
    float arrayMax = _maxUserValue;
    for ( int i = 0; i < size; i++ )
	arrayMax = MAX ( a[i], _maxUserValue );
    return arrayMax;
}
