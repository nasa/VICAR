////////////////////////////////////////////////////////////////
// TpImageView.cc: This class is responsible for displaying
// imaging widget with overlayed points.
////////////////////////////////////////////////////////////////
#include "TpImageView.h"
#include "TpPoint.h"
#include "TpMatchManager.h"
#include "TpSubDisplayer.h"
#include "ImageData.h"
#include "ErrorManager.h"
#include "Application.h"
#include "ImageData.h"

Boolean TpImageView::_actionsAdded = False;

TpImageView::TpImageView(Widget parent, const char *name, 
			 ImageData *imageData, 
			 TpMatchManager *matchManager, 
			 TpSubDisplayer *subDisplayer)
    : ImageOverlayView(parent, name, imageData)
{
    _matchManager = matchManager;
    _subDisplayer = subDisplayer;
    _saved = FALSE;

    XtAddCallback(_iw, XvicNinputCallback,
	&TpImageView::inputCallback, (XtPointer) this);

    _lastClickTime = 0;
}

TpImageView::~TpImageView()
{
    // Empty
}

void TpImageView::setTitle(char *)
{
    ImageOverlayView::setTitle(_matchManager->getIbisFileName());
}

void TpImageView::inputCallback(Widget, XtPointer clientData,
			XtPointer callData)
{
    TpImageView *obj = (TpImageView *)clientData;

    obj->input(callData);
}

//////////////////////////////////////////////////////////////////////
// input: Process user input.  
/////////////////////////////////////////////////////////////////////
void TpImageView::input(XtPointer callData)
{
    XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;

    if (cb->reason != XvicCR_INPUT)
	return;				// oops

    if (cb->input_num_params != 2)
	return;				// oops

    if (!strcmp(cb->input_params[0], "tp")) {
	double x, y;
	_model->transDisplayToImageCoords(cb->x_fp, cb->y_fp, &x, &y);

	if (!strcmp(cb->input_params[1], "new")) {
	    _matchManager->processNewPoint(x, y, _subDisplayer);
	}

        if (!strcmp(cb->input_params[1], "new_zoom")) {
            _matchManager->processNewPoint(x, y, _subDisplayer, False);
        }

        if (!strcmp(cb->input_params[1], "drag")) {
            _matchManager->processNewPoint(x, y, _subDisplayer);
        }

        if (!strcmp(cb->input_params[1], "drag_zoom")) {
            _matchManager->processNewPoint(x, y, _subDisplayer, False);
        }

	if (!strcmp(cb->input_params[1], "select")) {
	    XButtonPressedEvent *bevent = (XButtonPressedEvent *)cb->event;
	    if ((bevent->time - _lastClickTime) < 500)
		_matchManager->processSelectMatch(x, y, _subDisplayer);
	    else
		_matchManager->processSelectPoint(x, y, _subDisplayer);
	    _lastClickTime = bevent->time;
	}

	if (!strcmp(cb->input_params[1], "scrollAll")) {
	    _matchManager->processScrollAll(x, y, _subDisplayer);
	}

	if ((!strcmp(cb->input_params[1], "up")) ||
		(!strcmp(cb->input_params[1], "down")) ||
		(!strcmp(cb->input_params[1], "left")) ||
		(!strcmp(cb->input_params[1], "right"))) {
	    ZoomFactor zf = getImageZoom();
	    double ratioX = (double)zf.getXOut() / (double)zf.getXIn();
	    double ratioY = (double)zf.getYOut() / (double)zf.getYIn();

	    double moveX = 0.0;
	    double moveY = 0.0;
	    if (!strcmp(cb->input_params[1], "up"))
		moveY = ratioY * (-1);	
	    if (!strcmp(cb->input_params[1], "down"))
                moveY = ratioY;
	    if (!strcmp(cb->input_params[1], "left"))
                moveX = ratioX * (-1);
	    if (!strcmp(cb->input_params[1], "right"))
                moveX = ratioX;

	    _matchManager->processMovePoint(_subDisplayer, moveX, moveY);
	}
    }	
}

void TpImageView::setCenter(const double samp, const double line)
{
    double x, y;
    _model->transImageToDisplayCoords(samp, line, &x, &y);

    Dimension width, height;
    getViewSize(width, height);

    ZoomFactor zf = getImageZoom();
    int xin = zf.getXIn();
    int xout = zf.getXOut();
    int yin = zf.getYIn();
    int yout = zf.getYOut();

    XtVaSetValues(_iw, 
		XvicNxPan, (int)x-(int)(width/(2.0*((float)xin/(float)xout))),
		XvicNyPan, (int)y-(int)(height/(2.0*((float)yin/(float)yout))),
		NULL);
}
