///////////////////////////////////////////////////////
// HistAxisView.C:
////////////////////////////////////////////////////////
#include "HistAxisView.h"
#include "HistView.h"
#include "Histogram.h"
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <stdio.h>
#include <ctype.h>
using std::cerr;
using std::endl;

// Resources for this class

XtResource HistAxisView::_resources [ ] = {
 {
    (char *)"fontname",
    (char *)"Fontname",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistAxisView *, _fontname ),
    XmRImmediate,
    ( XtPointer ) "6x10",
 },
 {
    (char *)"drawColor", 
    (char *)"DrawOffset",
    XmRString,
    sizeof ( String ),
    XtOffset ( HistAxisView *, _drawColor ),
    XmRImmediate,
    ( XtPointer ) "black",
 },
 {
    (char *)"drawOffset",
    (char *)"DrawOffset",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( HistAxisView *, _drawOffset ),
    XmRImmediate,
    ( XtPointer ) 3,
 },
 {
    (char *)"longTickLength",
    (char *)"LongTickLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( HistAxisView *, _longTickLength ),
    XmRImmediate,
    ( XtPointer ) 7,
 },
 {
    (char *)"shortTickLength",
    (char *)"ShortTickLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( HistAxisView *, _shortTickLength ),
    XmRImmediate,
    ( XtPointer ) 5,
 },
 {
    (char *)"twoTicks",
    (char *)"TwoTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( HistAxisView *, _twoTicks ),
    XmRImmediate,
    ( XtPointer ) 100,
 },
 {
    (char *)"fourTicks",
    (char *)"FourTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( HistAxisView *, _fourTicks ),
    XmRImmediate,
    ( XtPointer ) 200,
 },
 {
    (char *)"eightTicks",
    (char *)"EightTicks",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( HistAxisView *, _eightTicks ),
    XmRImmediate,
    ( XtPointer ) 300,
 },
};

String HistAxisView::_defaults[] = {
    (char *)"*height:           30",
    (char *)"*width:            45",
     NULL,
};

HistAxisView::HistAxisView ( Widget parent, const char *name, 
		Histogram *hist, Histogram *hist1, Histogram *hist2,
		OrientType hor ) : HistView (name)
{
	_hor = hor;

        if (hist)       _hist = hist;
        else if (hist1) _hist = hist1;
        else if (hist2) _hist = hist2;
	else {
		fprintf (stderr, "Can't create axis, ");
		fprintf (stderr, "histogram model doesn't exist\n");
		_hist = NULL;
	}

	if (hist1) _hist1 = hist1;
	else _hist1 = _hist;
	if (hist2) _hist2 = hist2;
	else _hist2 = _hist;

        // Load the default resources into the database
        setDefaultResources ( parent, _defaults );

        _w = XtVaCreateWidget ( _name,
                                xmFormWidgetClass, parent,
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
			&HistAxisView::displayCallback,
			( XtPointer ) this );

	_gc = XtGetGC ( _w, 0, 0);
	Font font = XLoadFont ( XtDisplay(_w), _fontname );
	XSetFont ( XtDisplay(_w), _gc, font );
	_fontStruct = XQueryFont ( XtDisplay(_w), font );
	if ( _fontStruct == 0 ) 
		cerr << "No such font: " << _fontname << endl;

	XColor exact;
	Colormap cmap = DefaultColormap ( XtDisplay(_w),
					  DefaultScreen ( XtDisplay(_w) ) );
	XColor  color;
	XAllocNamedColor ( XtDisplay(_w), cmap, _drawColor, &color, &exact);
	XSetForeground ( XtDisplay(_w), _gc, color.pixel );

	if (_hist) _hist->attachView(this);
}

HistAxisView::~HistAxisView ()
{
	if (_hist) _hist->detachView(this);

	if ( _w && _gc )
		XtReleaseGC ( _w, _gc );
        if ( _w && _fontStruct )
		XFreeFont ( XtDisplay(_w), _fontStruct );
}

void  HistAxisView::displayCallback ( Widget,
                                 XtPointer clientData,
                                 XtPointer )
{
        HistAxisView *obj = ( HistAxisView * ) clientData;
        obj->display();
}

void HistAxisView::update()
{
        if ( XtIsRealized(_w) )
                display ();
}
