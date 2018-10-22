////////////////////////////////////////////////////////////////
// WedgeOverlayView.cc
////////////////////////////////////////////////////////////////
#include "WedgeOverlayView.h"
#include "PseudoMarks.h"
#include "PseudoValue.h"
#include "WedgeView.h"
#include <Xm/Frame.h>

XtResource WedgeOverlayView::_resources [ ] = {
 {
    (char *)"markColor",
    (char *)"MarkColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( WedgeOverlayView *, _markColor ),
    XmRImmediate,
    ( XtPointer ) "red",
 },
 {
    (char *)"selectedMarkColor",
    (char *)"SelectedMarkColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( WedgeOverlayView *, _selectedMarkColor ),
    XmRImmediate,
    ( XtPointer ) "green",
 },
 {
    (char *)"markLength",
    (char *)"MarkLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( WedgeOverlayView *, _markLength ),
    XmRImmediate,
    ( XtPointer ) 20,
 },
  {
    (char *)"markThickness",
    (char *)"MarkThickness",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( WedgeOverlayView *, _markThickness ),
    XmRImmediate,
    ( XtPointer ) 1,
 },
  {
    (char *)"barThickness",
    (char *)"BarThickness",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( WedgeOverlayView *, _barThickness ),
    XmRImmediate,
    ( XtPointer ) 10,
 },
};

WedgeOverlayView::WedgeOverlayView( Widget parent, const char * name )
		: BasicWedgeOverlay(name)
{
	_w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
	installDestroyHandler();

	getResources ( _resources, XtNumber ( _resources ) );

	WedgeView *wedge = new WedgeView(_w, "wedgeView");
	wedge->manage();
	_iw = wedge->getWidget();

	XtVaSetValues(_iw,
                XvicNlutType, XvicRAW,
                NULL);

	// Set color and GC for graphics overlay
        XColor xcolor;
        XParseColor(XtDisplay(_iw), DefaultColormapOfScreen(XtScreen(_iw)),
                _markColor, &xcolor);
        _lineColor = XvicImageGetGrColor(_iw, &xcolor);

        XParseColor(XtDisplay(_iw), DefaultColormapOfScreen(XtScreen(_iw)),
                _selectedMarkColor, &xcolor);
        _curLineColor = XvicImageGetGrColor(_iw, &xcolor);

        // Set GC for graphics overlay
        XGCValues values;
        values.line_width = _markThickness;
        _lineGC=XvicImageCreateGC(_iw, GCLineWidth, &values);
}

///////////////////////////////////////////////////////////////
//	update 
///////////////////////////////////////////////////////////////
void WedgeOverlayView::update (PseudoMarks *marks)
{
	int numMarks = marks->getNumMarks();
	int *m = marks->getDnAsArray();
	if (!m) return;                      // oops
	XvicImageEraseOverlay(_iw);
	for (int i = 0; i < numMarks; i++) {
	   if ( i != marks->getCurrent()-1 )
		XvicImageDrawLine(_iw, 0, _lineGC, _lineColor, 
			m[i], 0, m[i], _markLength);
	   else 
		XvicImageDrawLine(_iw, 0, _lineGC, _curLineColor,
			m[i], 0, m[i], _markLength);
	}
	int start = marks->getStart();
	int end = marks->getEnd();
	int start1 = marks->getStart1();
	int length = m[end] - m[start];
	if (length >= 0)
	    XvicImageFillRectangle(_iw, 0, _lineGC, _curLineColor, 
		m[start], 0, length, _barThickness); 
	else {
	    length = m[start1] - m[end];
	    XvicImageFillRectangle(_iw, 0, _lineGC, _curLineColor,
		m[start1]-length, 0, length, _barThickness);
	}
}
