//////////////////////////////////////////////////////////////////////////////
// TpWedgeOverlayView.h: Provides overlay graphics support for wedge class
// The class allows to manupulate two marks that are used by contrasting 
// tool in TP.
/////////////////////////////////////////////////////////////////////////////
#include "TpWedgeOverlayView.h"
#include "TpContrastValue.h"
#include "WedgeView.h"
#include "Cmd.h"
#include <Xm/Frame.h>
#include <stdlib.h>

XtResource TpWedgeOverlayView::_resources [ ] = {
 {
    (char *)"markColor",
    (char *)"MarkColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( TpWedgeOverlayView *, _markColor ),
    XmRImmediate,
    ( XtPointer ) "red",
 },
 {
    (char *)"markLength",
    (char *)"MarkLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( TpWedgeOverlayView *, _markLength ),
    XmRImmediate,
    ( XtPointer ) 20,
 },
  {
    (char *)"markThickness",
    (char *)"MarkThickness",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( TpWedgeOverlayView *, _markThickness ),
    XmRImmediate,
    ( XtPointer ) 6,
 },
};

String TpWedgeOverlayView::_defaults[] = {
    (char *)"*wedgeView.scrollBarDisplayPolicy: NEVER",
    (char *)"*wedgeView.shadowThickness:        0", 
    (char *)"*wedgeView.highlightThickness:     0", 
    (char *)"*wedgeView.orientation:            HORIZONTAL", 
    (char *)"*wedgeView.nsteps:                 256",
    (char *)"*wedgeView.viewWidth:              256",
    (char *)"*wedgeView.imageWidth:             256", 
    NULL,
};

TpWedgeOverlayView::TpWedgeOverlayView(Widget parent, const char *name, Cmd *cmd)
    : UIComponent(name)
{
    _cmd = cmd;

    setDefaultResources(parent, _defaults);

    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
    installDestroyHandler();

    getResources(_resources, XtNumber(_resources));
    
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
    
    // Set GC for graphics overlay

    XGCValues values;
    values.line_width = _markThickness;
    _lineGC=XvicImageCreateGC(_iw, GCLineWidth, &values);

    // Declare input callback

    XtAddCallback(_iw, 
		  XvicNinputCallback, 
		  &TpWedgeOverlayView::inputCallback, 
		  (XtPointer)this);
}

void TpWedgeOverlayView::inputCallback(Widget, XtPointer clientData, 
				       XtPointer callData)
{
    TpWedgeOverlayView *obj = (TpWedgeOverlayView *)clientData;
    obj->input(callData);
}

void TpWedgeOverlayView::input(XtPointer callData)
{
    XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)callData;
 
    if (cb->reason != XvicCR_INPUT)
	return;                           // oops
    
    if (cb->input_num_params != 2)
	return;                           // oops
    
    TpContrastValue *value = new TpContrastValue 
	(*(TpContrastValue *)(_cmd->getValue()));
    int distMin = abs(value->getMin() - cb->x);
    int distMax = abs(value->getMax() - cb->x);
    if (distMin < distMax)
	value->setMin(cb->x);
    else
	value->setMax(cb->x);

    _cmd->execute(value);
}

///////////////////////////////////////////////////////////////
// update 
///////////////////////////////////////////////////////////////
void TpWedgeOverlayView::update(TpContrastValue *value)
{
    XvicImageEraseOverlay(_iw);

    _min = value->getMin();
    _max = value->getMax();

    XvicPoint points[5];
    points[0].x = _min; 
    points[0].y = _markLength;

    points[1].x = -1 * _markThickness;
    points[1].y = -1 * _markLength / 2;
    
    points[2].x = 0;
    points[2].y = -1 * _markLength / 2;

    points[3].x = 2 * _markThickness;
    points[3].y = 0;

    points[4].x = 0;
    points[4].y = _markLength / 2;

    XvicImageFillPolygon(_iw, 0, _lineGC, _lineColor,
			 points, 5, 
			 Complex, CoordModePrevious);

    points[0].x = _max;
    points[0].y = _markLength;
 
    points[1].x = -1 * _markThickness;
    points[1].y = -1 * _markLength;
 
    points[2].x = 2 * _markThickness;
    points[2].y = 0;
 
    XvicImageFillPolygon(_iw, 0, _lineGC, _lineColor,
                         points, 3,
                         Complex, CoordModePrevious);
}

