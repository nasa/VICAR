//////////////////////////////////////////////////////////////////////////////
// TpPosView.cc
//////////////////////////////////////////////////////////////////////////////
#include "TpPosView.h"
#include "TpCursorModel.h"
#include "KeyinView.h"
#include "ImageData.h"
#include "TpMatchManager.h"
#include "TpSelectionMgr.h"
#include "TpSubDisplayer.h"
#include "TpMatch.h"
#include "TpPointModel.h"
#include "TpQualGroup.h"
#include "TpQualGroupMgr.h"
#include <Xm/RowColumn.h>
#include <stdio.h>

TpPosView::TpPosView(Widget parent, const char *name, 
		     TpCursorModel *cursorModel, ImageData *imageData, 
		     TpMatchManager *mm, TpSubDisplayer *subDisplayer)
    : UIComponent(name)
{
    _cursorModel = cursorModel;
    _imageData = imageData;
    _matchManager = mm;
    _subDisplayer = subDisplayer;

    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent, 
			  XmNorientation, XmHORIZONTAL,
			  XmNpacking, XmPACK_COLUMN,
			  XmNnumColumns, 1,
			  NULL);
    installDestroyHandler();

    _linePosition = new KeyinView(_w, "linePosition");
    _sampPosition = new KeyinView(_w, "sampPosition");
    _qualPosition = new KeyinView(_w, "qualPosition");

    _linePosition->installCallback(&TpPosView::changePosCallback, this);
    _sampPosition->installCallback(&TpPosView::changePosCallback, this);
    _qualPosition->installCallback(&TpPosView::changeQualCallback, this);

    _linePosition->manage();
    _sampPosition->manage();
    _qualPosition->manage();

    startTracking();
}

TpPosView::~TpPosView()
{
    XtRemoveCallback(_cursorModel->getImageWidget(),
		     XvicNcursorCallback,
		     &TpPosView::cursorMovedCallback,
		     (XtPointer)this);
}

Boolean TpPosView::isCursorOnImage(XvicImageCallbackStruct *cb)
{
    return(cb->on_screen &&
	   cb->x >= 0 && cb->x < _imageData->getNumbSamples() &&
	   cb->y >= 0 && cb->y < _imageData->getNumbLines());
}

void TpPosView::cursorIsOnImage(Boolean onImage)
{
    _isOnImage = onImage;
}

void TpPosView::startTracking()
{
    // Declare callbacks on the main image

    XtAddCallback(_cursorModel->getImageWidget(),
		  XvicNcursorCallback,
		  &TpPosView::cursorMovedCallback,
		  (XtPointer)this);

    // Declare callbacks on additional images, in our case zoomed image

    Widget *aiw;
    int n;
    _cursorModel->getImageWidgets(aiw, n);
    for (int i = 0; i < n; i++)
	XtAddCallback(aiw[i],
		      XvicNcursorCallback,
		      &TpPosView::cursorMovedCallback,
		      (XtPointer)this);
}

void TpPosView::stopTracking()
{
    // Remove callbacks on the main image

    XtRemoveCallback(_cursorModel->getImageWidget(),
		     XvicNcursorCallback,
		     &TpPosView::cursorMovedCallback,
		     (XtPointer)this);

    // Remove callbacks on additional images, in our case zoomed image

    Widget *aiw;
    int n;
    _cursorModel->getImageWidgets(aiw, n);
    for (int i = 0; i < n; i++)
	XtRemoveCallback(aiw[i],
			 XvicNcursorCallback,
			 &TpPosView::cursorMovedCallback,
			 (XtPointer)this);
}

void TpPosView::cursorMovedCallback(Widget,
                        XtPointer client_data, XtPointer call_data)
{
    TpPosView *obj = (TpPosView *)client_data;
    XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)call_data;
 
    if (obj->isCursorOnImage(cb) == False)
        obj->cursorIsOnImage(FALSE);
    else
        obj->cursorIsOnImage(TRUE);
 
    obj->cursorMoved(cb);
}

// Update the cursor position only if no points are selected in the 
// current image
void TpPosView::cursorMoved(XvicImageCallbackStruct * cb)
{
    TpSelectionMgr *selectionMgr;
    selectionMgr = _matchManager->getSelectionMgr();
    
    if (!selectionMgr->isEmpty()) {
	SL_ListWatch<TpPointModel *> watch;
	selectionMgr->initScan(&watch);
	TpPointModel *apoint;
	while ((apoint = selectionMgr->nextPoint()) != NULL) {
	    if (apoint->getImageData() == _imageData) {
		cursorMoved(apoint->getX(), apoint->getY());
		return;
	    }
	}
    }
    
    // Note that the program never gets to this point 
    // if current point was found on this image

    if (!_isOnImage)
	blankDisplay();
    else {
	double x, y;
	_imageData->transDisplayToImageCoords(cb->x_fp, cb->y_fp, &x, &y);
	cursorMoved(x, y);
    }
}

void TpPosView::cursorMoved(int x, int y)
{
    cursorMoved((double)x, (double)y);
}

void TpPosView::cursorMoved(double x, double y)
{
    char buf[132];

    sprintf(buf, "%5.3f", x);
    _sampPosition->setFieldValue(buf);

    sprintf(buf, "%5.3f", y);
    _linePosition->setFieldValue(buf);
}

void TpPosView::blankDisplay()
{
    _sampPosition->setFieldValue((char *)"");
    _linePosition->setFieldValue((char *)"");
    _qualPosition->setFieldValue((char *)"");
}

void TpPosView::changePosCallback(Widget, XtPointer clientData,
				  XtPointer)
{
    TpPosView *obj = (TpPosView *)clientData;
    obj->changePos();
}

void TpPosView::changePos()
{
    char *samp = _sampPosition->getFieldValue();
    char *line = _linePosition->getFieldValue();

    _matchManager->processNewPoint(atof(samp), atof(line), _subDisplayer);
	
}

void TpPosView::changeQualCallback(Widget, XtPointer clientData,
                                   XtPointer)
{
    TpPosView *obj = (TpPosView *)clientData;
    obj->changeQual();
}

void TpPosView::changeQual()
{
    char *valueString = _qualPosition->getFieldValue();
    
    // Get selected match

    TpSelectionMgr *selMgr = _matchManager->getSelectionMgr();
    TpMatch *match = selMgr->getMatch();
    if (match == NULL) {
	_qualPosition->setFieldValue((char *)"");
	return;
    }

    // Find affected point and get its qualifier model

    SL_ListWatch<TpPointModel *> watch;
    match->initScan(&watch);
    TpPointModel *apoint;
    while ((apoint = match->next()) != NULL) {
	if (_subDisplayer->getImageData() == apoint->getImageData()) {
	    match->scanDone();
	    break;
	}
    }

    TpQualGroup *pointQual = apoint->getPointQual();

    if (pointQual->getNumQuals() == 0) {
        TpQualType type = TpQualGroup::getValueType(valueString);
        _matchManager->getPointQualMgr()->incNumQuals(type);
    }
 
    pointQual->setValue(0, valueString);

    apoint->updateViews();
}

/////////////////////////////////////////////////////////////////////////////
// This function should be called to refresh displayed values
/////////////////////////////////////////////////////////////////////////////
void TpPosView::displayValues()
{
    TpSelectionMgr *selMgr = _matchManager->getSelectionMgr();
    TpMatch *match = selMgr->getMatch();
    if (match == NULL) {
        _qualPosition->setFieldValue((char *)"");
        return;
    }
 
    SL_ListWatch<TpPointModel *> watch;
    match->initScan(&watch);
    TpPointModel *apoint;
    while ((apoint = match->next()) != NULL) {
        if (_subDisplayer->getImageData() == apoint->getImageData()) {
            match->scanDone();
            break;
        }
    }

    TpQualGroup *pointQual = apoint->getPointQual();
 
    if (pointQual->getNumQuals() > 0) {
	char *buf = pointQual->valueToString(0);
	_qualPosition->setFieldValue(buf);
	delete [] buf;
    }
    else {
	_qualPosition->setFieldValue((char *)"");
    }

    char buf1[16];
    sprintf(buf1, "%.3f", apoint->getX());
    _sampPosition->setFieldValue(buf1);
    sprintf(buf1, "%.3f", apoint->getY());
    _linePosition->setFieldValue(buf1);
}

