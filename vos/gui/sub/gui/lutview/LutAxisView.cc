///////////////////////////////////////////////////////
// LutAxisView.cc:
////////////////////////////////////////////////////////
#include "LutAxisView.h"
#include "Lut.h"
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <stdio.h>
#include <ctype.h>
#include <iostream>
using namespace std;

// Resources for this class

XtResource LutAxisView::_resources [ ] = {
 {
    (char *)"fontname",
    (char *)"Fontname",
    XmRString,
    sizeof ( String ),
    XtOffset ( LutAxisView *, _fontname ),
    XmRImmediate,
    ( XtPointer ) "9x15",
 },
 {
    (char *)"drawOffset",
    (char *)"DrawOffset",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( LutAxisView *, _drawOffset ),
    XmRImmediate,
    ( XtPointer ) 3,
 },
 {
    (char *)"longTickLength",
    (char *)"LongTickLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( LutAxisView *, _longTickLength ),
    XmRImmediate,
    ( XtPointer ) 7,
 },
 {
    (char *)"shortTickLength",
    (char *)"ShortTickLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( LutAxisView *, _shortTickLength ),
    XmRImmediate,
    ( XtPointer ) 5,
 },
 {
    (char *)"twoTicks",
    (char *)"TwoTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( LutAxisView *, _twoTicks ),
    XmRImmediate,
    ( XtPointer ) 100,
 },
 {
    (char *)"fourTicks",
    (char *)"FourTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( LutAxisView *, _fourTicks ),
    XmRImmediate,
    ( XtPointer ) 200,
 },
 {
    (char *)"eightTicks",
    (char *)"EightTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( LutAxisView *, _eightTicks ),
    XmRImmediate,
    ( XtPointer ) 300,
 },
};

String LutAxisView::_defaults[] = {
    (char *)"*height:           40",
    (char *)"*width:            40",
     NULL,
};

LutAxisView::LutAxisView ( Widget parent, const char *name,
		Lut *lut, Lut *lut1, Lut *lut2 ) 
	: LutView (name, lut, lut1, lut2)
{
	// We need at least and only one lut, so just make sure 
	// that _lut is not empty.  User has to make sure that 
	// all three luts are the same size if not empty
	if (!_lut) {
	    if (_lut1)	_lut = _lut1;
	    else if (_lut2) _lut = _lut2;
	    else cerr << "All LUTs are empty!\n";
	}

	// Load the default resources into the database
        setDefaultResources ( parent, _defaults );

        _w = XtVaCreateWidget ( _name,
                                xmFormWidgetClass, parent,
				XmNborderWidth,	0,
                                NULL );
	installDestroyHandler();

	getResources ( _resources, XtNumber ( _resources ) );

        _ruler  = XtVaCreateManagedWidget ( "ruler",
                                        xmDrawingAreaWidgetClass, _w,
					XmNleftAttachment,	XmATTACH_FORM,
                     			XmNtopAttachment,     	XmATTACH_FORM,
                     			XmNrightAttachment,   	XmATTACH_FORM,
                     			XmNbottomAttachment,  	XmATTACH_FORM,
					XmNborderWidth, 0,
                                        NULL );

	XtAddCallback ( _ruler, XmNexposeCallback,
			&LutAxisView::displayCallback,
			( XtPointer ) this );

	_gc = XtGetGC ( _w,0,0);
	Font font = XLoadFont ( XtDisplay(_w), "6x10" );
	XSetFont ( XtDisplay(_w), _gc, font );
	_fontStruct = XQueryFont ( XtDisplay(_w), font );
	if ( _fontStruct == 0 ) cerr << "No such font";
}

LutAxisView::LutAxisView ( Widget parent, const char *name, Lut *lut )
        : LutView (name, lut)
{
        // We need at least one lut, so just make sure
        // that _lut is not empty.
        if (!_lut)
            cerr << "LUT is empty!\n";

        // Load the default resources into the database
        setDefaultResources ( parent, _defaults );

        _w = XtVaCreateWidget ( _name,
                                xmFormWidgetClass, parent,
                                XmNborderWidth, 0,
                                NULL );
        installDestroyHandler();

        getResources ( _resources, XtNumber ( _resources ) );

        _ruler  = XtVaCreateManagedWidget ( "ruler",
                                        xmDrawingAreaWidgetClass, _w,
                                        XmNleftAttachment,      XmATTACH_FORM,
                                        XmNtopAttachment,       XmATTACH_FORM,
                                        XmNrightAttachment,     XmATTACH_FORM,
                                        XmNbottomAttachment,    XmATTACH_FORM,
                                        XmNborderWidth, 0,
                                        NULL );

        XtAddCallback ( _ruler, XmNexposeCallback,
                        &LutAxisView::displayCallback,
                        ( XtPointer ) this );

        _gc = XtGetGC ( _w,0,0);
        Font font = XLoadFont ( XtDisplay(_w), "6x10" );
        XSetFont ( XtDisplay(_w), _gc, font );
        _fontStruct = XQueryFont ( XtDisplay(_w), font );
        if ( _fontStruct == 0 ) cerr << "No such font";
}


LutAxisView::~LutAxisView ()
{
	if ( _w && _gc )
		XtReleaseGC ( _w, _gc );
        if ( _w && _fontStruct )
		XFreeFont ( XtDisplay(_w), _fontStruct );
}

void  LutAxisView::displayCallback ( Widget,
                                 XtPointer clientData,
                                 XtPointer )
{
        LutAxisView *obj = ( LutAxisView * ) clientData;
        obj->update();
}

