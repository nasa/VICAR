///////////////////////////////////////////////////////
// LutGraphView.C:
////////////////////////////////////////////////////////
#include "LutGraphView.h"
#include "Lut.h"
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <iostream>
using namespace std;
#include <stdio.h>

// Resources for this class

XtResource LutGraphView::_resources [ ] = {
 {
    (char *)"redColor",
    (char *)"RedColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( LutGraphView *, _red ),
    XmRImmediate,
    ( XtPointer ) "red",
 },
 {
    (char *)"greenColor",
    (char *)"GreenColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( LutGraphView *, _green ),
    XmRImmediate,
    ( XtPointer ) "green",
 },
 {
    (char *)"blueColor",
    (char *)"BlueColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( LutGraphView *, _blue ),
    XmRImmediate,
    ( XtPointer ) "blue",
 },
};


LutGraphView::LutGraphView ( Widget parent, const char *name, 
		Lut *lut, Lut *lut1, Lut *lut2 ) 
	: LutView (name, lut, lut1, lut2)

{
	_w  = XtVaCreateWidget ( _name,
			xmDrawingAreaWidgetClass, parent,
			NULL );
	installDestroyHandler ();

	getResources ( _resources, XtNumber ( _resources ) );

	XtAddCallback ( _w, XmNexposeCallback,
			&LutGraphView::displayCallback,
			( XtPointer ) this );

	_gc = XtGetGC ( _w, 0, 0 );

	if (_lut)  _lut->attachView(this);
	if (_lut1) _lut1->attachView(this);
	if (_lut2) _lut2->attachView(this);
}

LutGraphView::LutGraphView ( Widget parent, const char *name, Lut *lut )
        : LutView (name, lut)

{
        _w  = XtVaCreateWidget ( _name,
                        xmDrawingAreaWidgetClass, parent,
                        NULL );
        installDestroyHandler ();

        getResources ( _resources, XtNumber ( _resources ) );

        XtAddCallback ( _w, XmNexposeCallback,
                        &LutGraphView::displayCallback,
                        ( XtPointer ) this );

        _gc = XtGetGC ( _w, 0, 0 );

        if (_lut)  _lut->attachView(this);
}

LutGraphView::~LutGraphView()
{
	if ( _w && _gc ) 
		XtReleaseGC ( _w, _gc );
}

void LutGraphView::displayCallback ( Widget,
				 XtPointer client_data,
				 XtPointer )
{
	LutGraphView *obj;
	obj = ( LutGraphView * ) client_data;
	if (obj != NULL)
		obj->update();
}

void LutGraphView::update ( )
{
	if ( ! XtIsRealized(_w) ) return;

        XSetWindowAttributes attrs;
        attrs.bit_gravity = ForgetGravity;
        XChangeWindowAttributes ( XtDisplay(_w),
                        XtWindow(_w), CWBitGravity, &attrs );

	Colormap cmap = DefaultColormap ( XtDisplay(_w), DefaultScreen ( XtDisplay(_w) ) );
	XColor exact;

        XtVaGetValues ( _w, XmNwidth,   &_width,
                            XmNheight,  &_height,
                            NULL );

	_height--;

	XPoint graph [256];
	double horScale = (double)_width / (double)256;
	double verScale = (double)_height / (double)256;
 
	XClearWindow ( XtDisplay(_w), XtWindow(_w) ); // Always clear the window before drawing

	int x;
	if (_lut)
	{
		for ( x=0; x<256; x++ )
		{
			graph[x].x = (int) (x * horScale);
			graph[x].y = _height - (int) ( (*_lut)[x] * verScale );
		}

        	XAllocNamedColor ( XtDisplay(_w), cmap, "red", &colorR, &exact);
        	XSetForeground ( XtDisplay(_w), _gc, colorR.pixel );
		XDrawLines ( XtDisplay(_w), XtWindow(_w), _gc, graph, 256, CoordModeOrigin );
	}

	if (_lut1)
	{
        	for ( x=0; x<256; x++ )
        	{
                	graph[x].x = (int) (x * horScale);
                	graph[x].y = _height - (int) ( (*_lut1)[x] * verScale );
        	}

        	XAllocNamedColor ( XtDisplay(_w), cmap, "green", &colorG, &exact);
        	XSetForeground ( XtDisplay(_w), _gc, colorG.pixel );
		XDrawLines ( XtDisplay(_w), XtWindow(_w), _gc, graph, 256, CoordModeOrigin );
	}

	if (_lut2)
	{
        	for ( x=0; x<256; x++ )
        	{
                	graph[x].x = (int) (x * horScale);
                	graph[x].y = _height - (int) ( (*_lut2)[x] * verScale );
        	}

        	XAllocNamedColor ( XtDisplay(_w), cmap, "blue", &colorB, &exact);
        	XSetForeground ( XtDisplay(_w), _gc, colorB.pixel );
		XDrawLines ( XtDisplay(_w), XtWindow(_w), _gc, graph, 256, CoordModeOrigin );
	}

	XSetForeground ( XtDisplay(_w), _gc, WhitePixel( XtDisplay(_w), 
				DefaultScreen(XtDisplay(_w)) ) );
}
