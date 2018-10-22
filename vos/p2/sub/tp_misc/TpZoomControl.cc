///////////////////////////////////////////////////////////////////
// TpZoomControl.cc: Zoom control is not handled through 
// Cmd/CmdInterface mechanism as usually.  There is only one interface, 
// so user interactions are handled directly through callbacks rather
// than commands.
///////////////////////////////////////////////////////////////////
#include "TpZoomControl.h"
#include "TpImageView.h"
#include "PrefManager.h"
#include "TpZoomControlPrefView.h"
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>

XtResource TpZoomControl::_resources [ ] = {
  {
    (char *)"zoomValue",
    (char *)"ZoomValue",
    XmRInt,
    sizeof(int),
    XtOffset (TpZoomControl *, _zoomValue),
    XmRImmediate,
    (XtPointer) 2,
  },
};

void TpZoomControl::reload(TpZoomControl *copy)
{
    if (_zoomValue != copy->_zoomValue)
        setZoom(copy->_zoomValue);
}

TpZoomControl::TpZoomControl(Widget parent, const char *name,
						TpImageView *zoomView)
	: UIComponent(name)
{
    _zoomView = zoomView;

    _w = XtVaCreateWidget(_name, xmFormWidgetClass, parent, NULL);
    installDestroyHandler();

    getResources(_resources, XtNumber(_resources));

    char path[100];
    sprintf(path, "*%s*", _name);
    thePrefManager->registerResources(this,
                                      _resources, XtNumber(_resources),
				      path, new TpZoomControlPrefView());

    // Create zoom label widget 

    Widget label = XtVaCreateManagedWidget("zoomLabel", 
				xmLabelWidgetClass, _w,
				NULL);

    // Create left button to decrement zoom by one

    Widget decZoom = XtVaCreateManagedWidget("decZoom", 
				xmPushButtonWidgetClass, _w, 
				NULL);
    XtAddCallback(decZoom, XmNactivateCallback, 
				&TpZoomControl::decZoomCallback,
				(XtPointer)this);

    // Create text field to keyin zoom value

    _keyinZoom = XtVaCreateManagedWidget("keyinZoom", 
				xmTextFieldWidgetClass, _w,
				NULL);
    XtAddCallback(_keyinZoom, XmNlosingFocusCallback, 
				&TpZoomControl::setZoomCallback, 
				(XtPointer)this);
    XtAddCallback(_keyinZoom, XmNactivateCallback,
				&TpZoomControl::setZoomCallback,
				(XtPointer)this);

    // Create right button to increment zoom by one

    Widget incZoom = XtVaCreateManagedWidget("incZoom",
				xmPushButtonWidgetClass, _w,
				NULL);
    XtAddCallback(incZoom, XmNactivateCallback,
				&TpZoomControl::incZoomCallback,
				(XtPointer)this);

    // Set form attachments

    XtVaSetValues(label,
                XmNtopAttachment,       XmATTACH_NONE,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_NONE,
                XmNbottomAttachment,    XmATTACH_FORM,
                NULL);
    XtVaSetValues(decZoom, 
		XmNtopAttachment,       XmATTACH_NONE,
		XmNleftAttachment,      XmATTACH_WIDGET,
		XmNleftWidget, 		label,
		XmNrightAttachment,     XmATTACH_NONE,
		XmNbottomAttachment,    XmATTACH_FORM,
		NULL);
    XtVaSetValues(_keyinZoom,
                XmNtopAttachment,       XmATTACH_NONE,
                XmNleftAttachment,      XmATTACH_WIDGET,
		XmNleftWidget, 		decZoom,
                XmNrightAttachment,     XmATTACH_WIDGET,
		XmNrightWidget,		incZoom,
                XmNbottomAttachment,    XmATTACH_FORM,
                NULL);
    XtVaSetValues(incZoom,
                XmNtopAttachment,       XmATTACH_NONE,
                XmNleftAttachment,      XmATTACH_NONE,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_FORM,
                NULL);

    this->setZoom(_zoomValue);
}

void TpZoomControl::incZoomCallback(Widget, XtPointer clientData,
			XtPointer)
{
    TpZoomControl *obj;
    obj = (TpZoomControl *)clientData;
    if (obj != NULL)
	obj->incZoom(1);
}

void TpZoomControl::decZoomCallback(Widget, XtPointer clientData,
                        XtPointer)
{
    TpZoomControl *obj;
    obj = (TpZoomControl *)clientData;
    if (obj != NULL)
        obj->incZoom(-1);
}

void TpZoomControl::setZoomCallback(Widget w, XtPointer clientData, 
			XtPointer)
{
    // Accept only positive ineger values.  If illegal value was 
    // typed, assume 1 and set that value back into the text field.

    int i = atoi(XmTextFieldGetString(w));
    //if (i <= 0) i = 1;
    char buf[16]; 
    sprintf(buf, "%d", i);
    XmTextFieldSetString(w, buf);

    TpZoomControl *obj;
    obj = (TpZoomControl *)clientData;
    if (obj != NULL)
	obj->setZoom(i);
}

//////////////////////////////////////////////////////////////////
// incZoom: Increment zoom by an integer.  Note that since zoom 
// values are assumed to be the same in both x and y directions, 
// only x value is obtained from the current zoom factor.
/////////////////////////////////////////////////////////////////
void TpZoomControl::incZoom(int i)
{
    ZoomFactor zoom = _zoomView->getImageZoom();
    int xin = zoom.getXIn();
    if (xin == 1) {
	int xout = zoom.getXOut();
	if (xout != 1)
	    xin = (-1) * xout;
    }
    if ((xin > 0) && (i == (-1)*xin)) xin = -2; // Special case
    else xin += i;

    if (xin == -1) xin = 1;

    setZoom(xin);
}

void TpZoomControl::setZoom(int i)
{
    if (i == -1) i = 1;

    _zoomValue = i;

    // Retain the center of the zoom window while zooming

    int x1, y1, x2, y2;
    XvicImageDisplayBounds(_zoomView->getWidget(), &x1, &y1, &x2, &y2);
    int x = (x2 + x1) / 2;
    int y = (y2 + y1) / 2;

    // Zoom to the required factor

    ZoomFactor *zoom;
    if (i == 0)
	i = 1;
    if (i > 0)
	zoom = new ZoomFactor(i, i, 1, 1);
    else if (i < 0)
	zoom = new ZoomFactor(1, 1, abs(i), abs(i));

    _zoomView->setUserZoom(*zoom);

    Dimension width, height;
    _zoomView->getViewSize(width, height);

    ZoomFactor zf = _zoomView->getImageZoom();
    int xin = zf.getXIn();
    int xout = zf.getXOut();
    int yin = zf.getYIn();
    int yout = zf.getYOut();

    XtVaSetValues(_zoomView->getWidget(),
		XvicNxPan, x-(int)(width/(2.0*((float)xin/(float)xout))),
		XvicNyPan, y-(int)(height/(2.0*((float)yin/(float)yout))),
		NULL);

    char buf[16];
    sprintf(buf, "%d", i);
    XmTextFieldSetString(_keyinZoom, buf);
}

