///////////////////////////////////////////////////////
// SgAxisView.C: A component class to show a plot axis.
////////////////////////////////////////////////////////
#include "SgAxisView.h"
#include "ErrorManager.h"
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <iostream>
using namespace std;
#include <assert.h>
#include <stdio.h>
#include <ctype.h>

// Resources for this class

XtResource SgAxisView::_resources [ ] = {
 {
    (char *)"min",
    (char *)"Min",
    XmRFloat,
    sizeof ( float ),
    XtOffset ( SgAxisView *, _min ),
    XmRString,
    ( XtPointer ) "0",
 },
 {
    (char *)"max",
    (char *)"Max",
    XmRFloat,
    sizeof ( float ),
    XtOffset ( SgAxisView *, _max ),
    XmRString,
    ( XtPointer ) "255",
 },
 {
    (char *)"drawTicksOnly",
    (char *)"DrawTicksOnly",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgAxisView *, _ticksOnly ),
    XmRString,
    ( XtPointer ) "FALSE",
 },
 {
    (char *)"intRange",
    (char *)"IntRange",
    XmRBoolean,
    sizeof ( Boolean ),
    XtOffset ( SgAxisView *, _intRange ),
    XmRString,
    ( XtPointer ) "TRUE",
 },
 {
    (char *)"fontList",
    (char *)"FontList",
    XmRString,
    sizeof ( String ),
    XtOffset ( SgAxisView *, _fontname ),
    XmRImmediate,
    ( XtPointer ) "6x10",
 },
 {
    (char *)"drawOffset",
    (char *)"DrawOffset",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _drawOffset ),
    XmRImmediate,
    ( XtPointer ) 3,
 },
 {
    (char *)"fistTickMargin",
    (char *)"FistTickMargin",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _fistTickMargin ),
    XmRImmediate,
    ( XtPointer ) 0,
 },
 {
    (char *)"lastTickMargin",
    (char *)"LastTickMargin",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _lastTickMargin ),
    XmRImmediate,
    ( XtPointer ) 0,
 },
 {
    (char *)"tickThickness",
    (char *)"TickThickness",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _tickThickness ),
    XmRImmediate,
    ( XtPointer ) 1,
 },
 {
    (char *)"longTickLength",
    (char *)"LongTickLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _longTickLength ),
    XmRImmediate,
    ( XtPointer ) 8,
 },
 {
    (char *)"shortTickLength",
    (char *)"ShortTickLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _shortTickLength ),
    XmRImmediate,
    ( XtPointer ) 5,
 },
 {
    (char *)"strOffset",
    (char *)"StrOffset",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _strOffset ),
    XmRImmediate,
    ( XtPointer ) 3,
 },
 {
    (char *)"twoTicks",
    (char *)"TwoTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _twoTicks ),
    XmRImmediate,
    ( XtPointer ) 100,
 },
 {
    (char *)"fourTicks",
    (char *)"FourTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _fourTicks ),
    XmRImmediate,
    ( XtPointer ) 200,
 },
 {
    (char *)"eightTicks",
    (char *)"EightTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( SgAxisView *, _eightTicks ),
    XmRImmediate,
    ( XtPointer ) 300,
 },
};

String SgAxisView::_defaults[] = {
    (char *)"*height:           256",
    (char *)"*width:            40",
     NULL,
};

SgAxisView::SgAxisView ( Widget parent, const char *name, Boolean vertical ) 
		: UIComponent (name)
{
    _vertical = vertical;
    _ascending = TRUE;

    setDefaultResources ( parent, _defaults );

    _w = XtVaCreateWidget ( _name,
			    xmDrawingAreaWidgetClass, parent,
			    NULL );

    installDestroyHandler();

    getResources ( _resources, XtNumber ( _resources ) );

    XtAddCallback ( _w, XmNexposeCallback,
		    &SgAxisView::displayCallback,
		    ( XtPointer ) this );

    // Allocate private GC 

    _gc = XCreateGC ( XtDisplay ( _w ), 
		RootWindowOfScreen ( XtScreen ( _w ) ),
		0L, NULL );

    // Modify GC, setting foreground, line attributes, and font

    Pixel pixel;
    XtVaGetValues ( _w, XmNforeground, &pixel, NULL );
    XSetForeground ( XtDisplay ( _w ), _gc, pixel );

    XSetLineAttributes ( XtDisplay(_w), _gc, _tickThickness, 
			 LineSolid, CapButt, JoinMiter );

    _fontStruct = XLoadQueryFont ( XtDisplay(_w), _fontname );
    if ( _fontStruct == NULL ) {
	theErrorManager->process ( Error, "axis", "No such font", _fontname);
	_fontStruct = XQueryFont ( XtDisplay(_w), XGContextFromGC(_gc) );
    }
    else {
	XSetFont ( XtDisplay(_w), _gc, _fontStruct->fid );
    }
}

SgAxisView::~SgAxisView()
{
	if ( _w && _gc )
		XFreeGC ( XtDisplay ( _w ), _gc );

        if ( _w && _fontStruct )
		XFreeFont ( XtDisplay ( _w ), _fontStruct );
}

void SgAxisView::displayCallback ( Widget, XtPointer clientData, XtPointer )
{
        SgAxisView *obj = ( SgAxisView * ) clientData;
        obj->display();
}

void SgAxisView::display()
{
    if ( !XtIsRealized ( _w ) )
	return;

    // Make any resize cause expose event (see vol. 6A, p.346-7)

    XSetWindowAttributes attrs;
    attrs.bit_gravity = ForgetGravity;
    XChangeWindowAttributes ( XtDisplay ( _w ), XtWindow ( _w ),
                              CWBitGravity, &attrs );

    // Get current size

    Dimension width, height;
    XtVaGetValues ( _w,
		    XmNwidth,  &width,
                    XmNheight, &height,
                    NULL );

    // Always clear window before start drawing

    XClearWindow ( XtDisplay(_w), XtWindow(_w) );

    // Draw the ticks and the labels

    if ( _vertical == TRUE )
	drawVertical ( height, width );
    else 
	drawHorizontal ( width );
}

void SgAxisView::drawVertical ( Dimension height, Dimension width )
{
    Dimension drawHeight = height - _fistTickMargin - _lastTickMargin;

    // Draw a long line from side to side

    if ( _ticksOnly == FALSE )
	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
			width - _drawOffset, 
			_fistTickMargin,
			width - _drawOffset, 
			_fistTickMargin + drawHeight );

    // Calculate how many ticks we need at this screen width

    int numTicks = getNumTicks ( drawHeight );

    // Calculate distance between ticks, double precision

    double step = (double)drawHeight / (double)(numTicks-1);

    int i;
    for ( i = 0; i < numTicks - 1; i+=2 ) {

	// Draw a long tick

	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
			width - _drawOffset - _longTickLength,
			(int)(_fistTickMargin + (double)i * step),
			width - _drawOffset,
			(int)(_fistTickMargin + (double)i * step) );
    }

    for ( i = 1; i < numTicks - 1; i+=2 ) {

	// Draw a short tick

	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
			width - _drawOffset - _shortTickLength,
			(int)(_fistTickMargin + (double)i * step),
			width - _drawOffset,
			(int)(_fistTickMargin + (double)i * step) );
    }

    // Draw the last tick

    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
		width - _drawOffset - _longTickLength,
		height - _lastTickMargin - 1,
		width - _drawOffset,
		height - _lastTickMargin - 1 );

    // Draw label to every other tick

    double lbl;
    char buf [16];

    for ( i = 0; i < numTicks; i+=2 ) {
	lbl = _min + (double(_max - _min) / (double)(numTicks-1) ) * (double)i;
	if ( _ascending )
	    lbl = (_max + _min) - lbl;

	if ( _intRange )
	    sprintf ( buf, "%d", (int)lbl );
	else
	    sprintf ( buf, "%.3g", lbl );

	Dimension strWidth = XTextWidth ( _fontStruct, buf, strlen(buf) );
	Dimension strHeight = Dimension ( _fontStruct->ascent );

	// First and last ticks are special cases

	if ( i > 0 && i < numTicks - 1 )
	    strHeight /= 2;
	else if ( i == numTicks - 1 )
	    strHeight = 0;

	XDrawString ( XtDisplay ( _w ), XtWindow ( _w ), _gc,
		width - _drawOffset - _longTickLength - _strOffset - strWidth,
		(int)(_fistTickMargin + (double)i * step + strHeight),
		buf, strlen(buf) );
    }
}

void SgAxisView::drawHorizontal ( Dimension width )
{
    Dimension drawWidth = width - _fistTickMargin - _lastTickMargin;

    // Draw a long line (a ruler?) from side to side

    if ( _ticksOnly == FALSE )
        XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
			_fistTickMargin, 
			_drawOffset,
			width - _lastTickMargin, 
			_drawOffset );

    // Calculate how many ticks we need at this screen width

    int numTicks = getNumTicks ( drawWidth );

    // Calculate distance between ticks, double precision

    double step = (double)drawWidth / (double)(numTicks-1);

    int i;
    for ( i = 0; i < numTicks - 1; i+=2 ) {

	// Draw a long tick

	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
			(int)(_fistTickMargin + (double)i * step),
			_drawOffset, 
			(int)(_fistTickMargin + (double)i * step),
			_drawOffset + _longTickLength );
    }

    for ( i = 1; i < numTicks - 1; i+=2 ) {

	// Draw a short tick

	XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
			(int)(_fistTickMargin + (double)i * step),
			_drawOffset,
			(int)(_fistTickMargin + (double)i * step),
			_drawOffset + _shortTickLength );
    }

    // Draw the last tick

    XDrawLine ( XtDisplay(_w), XtWindow(_w), _gc,
		width - _lastTickMargin - 1, 
		_drawOffset,
		width - _lastTickMargin - 1,
		_drawOffset + _longTickLength );


    // Draw label to every other tick

    double lbl;
    char buf [16];

    for ( i = 0; i < numTicks; i+=2 ) {
        lbl = _min + (double(_max - _min) / (double)(numTicks-1) ) * (double)i;
        if ( _intRange )
	    sprintf ( buf, "%d", (int)lbl );
        else
            sprintf ( buf, "%.3g", lbl );

	Dimension strWidth = XTextWidth ( _fontStruct, buf, strlen(buf) );
	Dimension strHeight = Dimension ( _fontStruct->ascent );

	// First and last ticks are special cases

	if ( i == 0 )
	    strWidth = 0;
	else if ( i != numTicks - 1 )
	    strWidth /= 2;

	XDrawString (XtDisplay(_w), XtWindow(_w), _gc,
			(int)(_fistTickMargin + (double)i * step - strWidth),
			_drawOffset + _longTickLength + _strOffset + strHeight,
			buf, strlen(buf) );
    }
}

int SgAxisView::getNumTicks ( Dimension drawWidth )
{
    if ( drawWidth < _twoTicks )
        return 3;
    else if ( drawWidth < _fourTicks )
        return 5;
    else if ( drawWidth < _eightTicks )
        return 9;
    else
        return 17;
}

void SgAxisView::setLimits ( int min, int max )
{ 
    assert ( min <= max );

    _min = (float)min;
    _max = (float)max;
}

void SgAxisView::setLimits ( float min, float max )
{ 
    assert ( min <= max );

    _min = min;
    _max = max;

    display();
}

void SgAxisView::setIntRange ( Boolean intRange )
{
    if ( _intRange == intRange )
	return;
    else
	_intRange = intRange;

    display();
}

void SgAxisView::setVertical ( Boolean vertical )
{
    if ( _vertical == vertical )
	return;
    else 
	_vertical = vertical;

    Dimension width;
    if ( _vertical ) {
	XtVaGetValues ( _w, 
		XmNheight, &width, 
		NULL);
	XtVaSetValues ( _w,
                XmNwidth, width,
                NULL);
    }
    else {
        XtVaGetValues ( _w,
                XmNwidth, &width,
                NULL);
        XtVaSetValues ( _w,
                XmNheight, width,
                NULL);
    }

    display();
}

void SgAxisView::setAscending ( Boolean ascending )
{
    if ( _ascending == ascending )
	return;
    else
	_ascending = ascending;

    display();
}
