///////////////////////////////////////////////////////////////////
// TpSubDisplayer.cc: There is one subdisplayer per image.  It contains 
// all the components associated with that image, such as image view, 
// pan view, zoom view, contast control panel, etc.
///////////////////////////////////////////////////////////////////
#include "TpSubDisplayer.h"
#include "TpMatchManager.h"
#include "TpDisplayer.h"
#include "TpImageView.h"
#include "TpStripPanTool.h"
#include "TpCursorModel.h"
#include "TpPosView.h"
#include "TpImageInfo.h"
#include "TpZoomControl.h"
#include "TpContrastCmd.h"
#include "TpContrastControl.h"
#include "VicarImageData.h"
#include "RotatedImageData.h"
#include "ErrorManager.h"
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>

XtResource TpSubDisplayer::_resources[] = {
 {
   (char *)"autoMin",
   (char *)"AutoMin",
   XmRBoolean,
   sizeof (Boolean),
   XtOffsetOf(TpSubDisplayer, _autoMinRange),
   XmRString,
   (XtPointer)"TRUE",
 },
 {
   (char *)"autoMax",
   (char *)"AutoMax",
   XmRBoolean,
   sizeof(Boolean),
   XtOffsetOf(TpSubDisplayer, _autoMaxRange),
   XmRString,
   (XtPointer)"TRUE",
 },
 {
   (char *)"min",
   (char *)"Min",
   XmRString,
   sizeof(String),
   XtOffsetOf(TpSubDisplayer, _minValue),
   XmRImmediate,
   (XtPointer)NULL,
 },
 {
   (char *)"max",
   (char *)"Max",
   XmRString,
   sizeof(String),
   XtOffsetOf(TpSubDisplayer, _maxValue),
   XmRImmediate,
   (XtPointer)NULL,
 },
};

TpSubDisplayer::TpSubDisplayer(Widget parent, const char *name, 
			       char *filename, int number, 
			       TpMatchManager *matchManager, 
			       TpDisplayer *displayer)
	: UIComponent(name)
{
    _displayer = displayer;
    _matchManager = matchManager;
 
    _filename = sdup(filename);
    _number = number;
    _locked = False;

    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
    installDestroyHandler();

    getResources(_resources, XtNumber(_resources));

    _form = XtVaCreateWidget("dispForm", xmFormWidgetClass, _w, NULL);

    _rotationMode = ROTATE_NO;
    _imageData = new RotatedImageData(_rotationMode);

    // Set up the initial data range before loading
    
    _imageData->setMinAuto(_autoMinRange);
    _imageData->setMaxAuto(_autoMinRange);
    if (_minValue) {
        double min = atof(_minValue);
        _imageData->setMinAuto(False);
        _imageData->setDataMin(min);
    }
    if (_maxValue) {
        double max = atof(_maxValue);
        _imageData->setMaxAuto(False);
        _imageData->setDataMax(max);
    }

    _failed = False;
    if (_imageData->open(_filename) != imSUCCESS) {
	theErrorManager->process(Error, _name, "Cannot open file", _filename);
	_failed = True;
    }

    _imageInfo = new TpImageInfo(_form, "imageInfo", _filename, _number);

    _imageView = new TpImageView(_form, "imageView", 
				 _imageData, 
				 matchManager, this);

    char buf[32];
    sprintf(buf, "mainImageZoomControl%d", _number);
    _mainImageZoom = new TpZoomControl(_form, buf, _imageView);

    _panZoomForm = XtVaCreateWidget("panZoomForm", 
				    xmFormWidgetClass, _form, NULL);

    _panView = new TpStripPanTool(_panZoomForm, "panView", _imageData, 
				_imageView->getWidget(), 150, 150, False);

    _zoomView = new TpImageView(_panZoomForm, "zoomView", 
				_imageData, matchManager, this);

    sprintf(buf, "zoomControl%d", _number);
    _zoomControl = new TpZoomControl(_panZoomForm, buf, _zoomView);

    Widget *aiw = new Widget [1]; 
    aiw[0] = _zoomView->getWidget();
    TpCursorModel *cursorModel = new TpCursorModel(TRUE, 
			_imageView->getWidget(), aiw, 1);
    _posView = new TpPosView(_form, "posView", cursorModel, 
			     _imageData, 
			     matchManager, this);

    sprintf(buf, "contrastControl%d", _number);
    Cmd *contrastCmd = new TpContrastCmd(buf, True, 
					 _imageView->getWidget(), 
					 _zoomView->getWidget(),
					 _panView->getWidget(),
					 _imageData);
    _contrastControl = new TpContrastControl(_form, contrastCmd);

    _lock = XtVaCreateWidget("lock", xmLabelWidgetClass, _form, NULL);

    setCursor(_displayer->getCursor());
    setCursorColor(_displayer->getCursorColor());

    layoutComponents();
    showComponents();
}

TpSubDisplayer::~TpSubDisplayer()
{
    delete [] _filename;
    delete _imageData; // image model will delete all views

    delete _imageInfo;
    delete _zoomControl;
    delete _posView;
    delete _contrastControl;
    XtDestroyWidget(_lock);
    XtDestroyWidget(_panZoomForm);
}

void TpSubDisplayer::layoutComponents() const
{
    XtVaSetValues(_panView->baseWidget(),
                XmNtopAttachment,       XmATTACH_FORM,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_POSITION,
		XmNrightPosition,	50,
                XmNbottomAttachment,    XmATTACH_FORM,
                NULL);
    XtVaSetValues(_zoomView->baseWidget(),
                XmNtopAttachment,       XmATTACH_FORM,
                XmNleftAttachment,      XmATTACH_POSITION,
                XmNleftPosition,        50,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_WIDGET,
                XmNbottomWidget,        _zoomControl->baseWidget(),
                NULL);
    XtVaSetValues(_zoomControl->baseWidget(),
                XmNtopAttachment,       XmATTACH_NONE,
                XmNleftAttachment,      XmATTACH_WIDGET,
                XmNleftWidget,          _panView->baseWidget(),
                XmNrightAttachment,     XmATTACH_NONE,
                XmNbottomAttachment,    XmATTACH_FORM,
                NULL);


    XtVaSetValues(_posView->baseWidget(),
		XmNtopAttachment,	XmATTACH_FORM,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_NONE,
		XmNbottomAttachment,	XmATTACH_NONE,
		NULL);
    XtVaSetValues(_lock,
		XmNtopAttachment,       XmATTACH_FORM,
		XmNleftAttachment,	XmATTACH_NONE,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNbottomAttachment,	XmATTACH_NONE,
		NULL);
    XtVaSetValues(_imageInfo->baseWidget(),
                XmNtopAttachment,       XmATTACH_WIDGET,
		XmNtopWidget,		_posView->baseWidget(),
                XmNleftAttachment,      XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_NONE,
                NULL);
    XtVaSetValues(_imageView->baseWidget(),
                XmNtopAttachment,       XmATTACH_WIDGET,
                XmNtopWidget,           _imageInfo->baseWidget(),
                XmNleftAttachment,      XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_WIDGET,
		XmNbottomWidget,	_mainImageZoom->baseWidget(),
                NULL);
    XtVaSetValues(_mainImageZoom->baseWidget(),
		XmNtopAttachment,       XmATTACH_NONE,
		XmNleftAttachment,      XmATTACH_FORM,
		XmNrightAttachment,     XmATTACH_NONE,
		XmNbottomAttachment,    XmATTACH_WIDGET,
		XmNbottomWidget,        _panZoomForm,
		NULL);
    XtVaSetValues(_panZoomForm,
                XmNtopAttachment,       XmATTACH_NONE,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_WIDGET,
		XmNbottomWidget,	_contrastControl->baseWidget(),
                NULL);

    XtVaSetValues(_contrastControl->baseWidget(),
                XmNtopAttachment,       XmATTACH_NONE,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_FORM,
                NULL);
}

///////////////////////////////////////////////////////////////////
// showComponents: Manages only the components to be shown
///////////////////////////////////////////////////////////////////
void TpSubDisplayer::showComponents() const
{
    // Show image pan window
    _panView->manage();

    // Show zoomed image
    _zoomView->manage();

    // Show zoom controls
    _zoomControl->manage();

    XtManageChild(_panZoomForm);

    // Show position coordinates
    _posView->manage();

    // Show image info
    _imageInfo->manage();

    // Show image view
    _imageView->manage();

    // Show main image view's zoom control
    _mainImageZoom->manage();

    // Show contrast window
    _contrastControl->manage();

    XtManageChild(_panZoomForm);
    XtManageChild(_form);
}

///////////////////////////////////////////////////////////////////
// hideComponents: Unmanages the images so as to hide them.
///////////////////////////////////////////////////////////////////
void TpSubDisplayer::hideComponents() const
{
    _posView->unmanage();
    _imageInfo->unmanage();
    _imageView->unmanage();
    _mainImageZoom->unmanage();
    _zoomView->unmanage();
    _zoomControl->unmanage();
    _contrastControl->unmanage();
    XtUnmanageChild(_panZoomForm);
    XtUnmanageChild(_form);
}

void TpSubDisplayer::setRotationMode(RotationType rotMode)
{
    _rotationMode = rotMode;
    _imageData->setRotationMode(rotMode);
    if (_imageData->open(_filename) != imSUCCESS) {
        theErrorManager->process(Error, _name, "Cannot open file", _filename);
    }
    _matchManager->rotationChanged((ImageData *)_imageData); //!!! Use Unrot???
}

void TpSubDisplayer::setLock(Boolean setIt)
{
    (setIt) ? XtManageChild(_lock) : XtUnmanageChild(_lock);
}

void TpSubDisplayer::setCursor(String newCursor)
{
    Widget iw = _imageView->getWidget();
    XtVaSetValues(iw, XvicNcursor, newCursor, NULL);
    iw = _zoomView->getWidget();
    XtVaSetValues(iw, XvicNcursor, newCursor, NULL);
}

void TpSubDisplayer::setCursorColor(String newCursorColor)
{
    Widget iw = _imageView->getWidget();
    XtVaSetValues(iw, XvicNcursorBackground, newCursorColor, NULL);
    iw = _zoomView->getWidget();
    XtVaSetValues(iw, XvicNcursorBackground, newCursorColor, NULL);
}
