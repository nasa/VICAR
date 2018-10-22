////////////////////////////////////////////////////////////////
// CursorLatLonView.cc
//
//	View for displaying the latitude and longitude 
//      corresponding to the (x,y) coordinates of the cursor
//
//	Subclass of CursorBasicView.  
//
///////////////////////////////////////////////////////////////
#include "CursorLatLonView.h"
#include "ImageData.h"
#include "ErrorDialogManager.h"
#include "CmdList.h"
#include "OptionCmdMenu.h"
#include <Xm/RowColumn.h>	
#include <Xm/Form.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include "SetLatLonTypeCmd.h"
#include <stdio.h>
#include <iostream>
using namespace std;
#include "NoOpCmd.h"
#include "MenuCmdList.h"


//#define DIAG_LL_TYPE // uncomment this to see which projection type is used

///////////////////////////////////////////////////////////////
//	CONSTRUCTOR
///////////////////////////////////////////////////////////////
CursorLatLonView::CursorLatLonView(Widget parent, const char *name, 
			CursorModel *cursorModel, ImageData *imageData, 
			unsigned char bitFlags)
	: CursorBasicView(parent, name, cursorModel, imageData, bitFlags)
{

  _selectionWidget = NULL;  // kept so that later, we can turn it on/off
  
  _lonDir = UNDEFdirection;
  
  _formLat = NULL;
  _labelLat = NULL;
  _textfieldLat = NULL;
  
  _formLon = NULL;
  _labelLon = NULL;
  _textfieldLon = NULL;
  
  _latLonType = CENTRIC; 
  
  createDisplay(); // this will attach the view to the model
  stopTracking();
}

///////////////////////////////////////////////////////////////
//	DESTRUCTOR
///////////////////////////////////////////////////////////////
CursorLatLonView::~CursorLatLonView(  )
{

}

///////////////////////////////////////////////////////////////
//	cursorMoved() .. implements pure virtual in abstract
//		updates each LatLon display with new LatLon
//		values under cursor.
///////////////////////////////////////////////////////////////

void CursorLatLonView::cursorMoved(XvicImageCallbackStruct * cb )
{
  cursorMoved(cb->x, cb->y);
}

#ifdef DIAG_LL_TYPE
   static int num_cursor_moves = 0;
#endif

void CursorLatLonView::cursorMoved(int x, int y)
{
#ifdef DIAG_LL_TYPE
  if((num_cursor_moves % 100) == 0) {
    if (_latLonType == CENTRIC)
      cout << "using CENTRIC projection type" << endl;
    else
      cout << "using DETIC projection type" << endl;
  }
#endif

  
  char buf[132];
  double latVal, lonVal;
  int status, type;
  
  if( _latLonType == CENTRIC )
    type = 1;
  else 
    type = 2;
  
  status = _imageData->lineSampToLatLon( (double) y, (double) x,
					 &latVal, &lonVal, type );
  
  // UPDATE Lon
  if(status == 1)
    sprintf(buf, "%.10g", lonVal);
  else   // cursor was probably out of range
    sprintf(buf, "+++");
  updateValue(_textfieldLon, buf, _lonEnable);
  
  
  // UPDATE Lat
  if(status == 1)
    sprintf(buf, "%.10g", latVal); 
  else
    sprintf(buf, "+++");
  updateValue(_textfieldLat, buf, _latEnable);
}

///////////////////////////////////////////////////////////////
//	createCursorDisplays:
//		Satisfies pure virtual function.  Creates 
//		subviews for (1) cursor lat position 
//		(2) cursor lon position.   Called 
//		by CursorBasicView::createDisplay() which is
//		itself called within constructor of this subclass.
///////////////////////////////////////////////////////////////
void CursorLatLonView::createCursorDisplays()
{ 
  // DECIPHER BIT FLAGS
  _latEnable = False;
  _lonEnable = False;
  if ( _bitFlags && (unsigned char) 1  )
    _latEnable = True;
  if ( _bitFlags && (unsigned char) 2  )
    _lonEnable = True;
  
  if (_latEnable && _lonEnable) {
    
    _latLonForm = XtVaCreateManagedWidget("LatLonForm",
					  xmFormWidgetClass,
					  _w,
					  NULL);
    // CREATE LABEL
    _labelLat = XtVaCreateManagedWidget("LatLabel", 
					xmLabelWidgetClass, 
					_latLonForm,
					XmNleftAttachment, XmATTACH_FORM,
					XmNtopAttachment, XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_FORM,
					NULL ); 
    // LAT stuff:	  
    _textfieldLat = XtVaCreateManagedWidget("textFieldLat", 
					    xmTextFieldWidgetClass,
					    _latLonForm,
					    XmNleftAttachment, XmATTACH_WIDGET,
					    XmNleftWidget, _labelLat,
					    NULL);



    // LON stuff:
    _labelLon = XtVaCreateManagedWidget("LonLabel", 
					xmLabelWidgetClass, _latLonForm,
					XmNleftAttachment, XmATTACH_WIDGET,
					XmNleftWidget, _textfieldLat,
					XmNtopAttachment, XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_FORM,
					NULL );
    
    _textfieldLon = XtVaCreateManagedWidget("textFieldLon", 
					   xmTextFieldWidgetClass, _latLonForm,
					   XmNleftAttachment, XmATTACH_WIDGET,
					   XmNleftWidget, _labelLon,
					   NULL);

    // E/W (longitude direction) stuff:
    _lonDirectionLabel = XtVaCreateManagedWidget("lonDirectionLabel",
					    xmTextFieldWidgetClass,
					    _latLonForm,
					    XmNleftAttachment, XmATTACH_WIDGET,
				            XmNleftWidget, _textfieldLon,
					    XmNshadowThickness, 0,
					    XmNeditable, False,
					    XmNcolumns, 2,
					    XmNvalue, "UU",
					    XmNcursorPositionVisible, False,
					    NULL );


    // set up menu for lat/lon options
    
    MenuCmdList *latLonList = new MenuCmdList();
    
    // this Cmd doesn't do anything, it's just a "label"
    Cmd *dummyLabel = new NoOpCmd("Projection Type", FALSE);
    latLonList->addOption(dummyLabel); 

    latLonList->addSeparator();

    // the last arg, a CmdList, is NULL since we don't want to automatically
    // be added to the list (we do addOption() instead)
    Cmd *deticLatLonCmd = new SetLatLonTypeCmd("Planetodetic", TRUE,
					       this, DETIC, NULL);
    
    latLonList->addOption(deticLatLonCmd);
    

    Cmd *centricLatLonCmd = new SetLatLonTypeCmd("Planetocentric", TRUE,
						 this, CENTRIC, NULL);

    latLonList->addOption(centricLatLonCmd);

    // set up selection / labels
    OptionCmdMenu *ci = new OptionCmdMenu(_latLonForm, "!at/Lon Type",
					  latLonList); // no applyCmdList here
    _latLonTypeWidget = ci->baseWidget();


    XtVaSetValues(_latLonTypeWidget, 
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, _lonDirectionLabel,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);

    int IFactive;

    // check whether we have an MP object; if not, we can't do lat/lon, so
    // we "grey-out" the interface

    if (_imageData == NULL)
      IFactive = FALSE;
    else if (!_imageData->isMapProjInfoPresent())
      IFactive = FALSE;
    else
      IFactive = TRUE;
    
    enableLatLonSelect(IFactive);   // deactivate the interface

    ci->manage();
  }
}


///////////////////////////////////////////////////////////////
//	removeCursorDisplays()
//		Removes widgets created by createCursorDisplays.
///////////////////////////////////////////////////////////////
void CursorLatLonView::removeCursorDisplays()
{
        removeSubView(&_formLat, &_labelLat, &_textfieldLat);
	removeSubView(&_formLon, &_labelLon, &_textfieldLon);
}

///////////////////////////////////////////////////////////////
//	blankSubViews()
//		Blank out all views - cursor out of range.  
//		Called by abstract class.
///////////////////////////////////////////////////////////////
void CursorLatLonView::blankSubViews()
{
	// BLANK OUT Lat
	updateValue(_textfieldLat, "   ", _latEnable);

	// BLANK OUT Lon
	updateValue(_textfieldLon, "   ", _lonEnable);
}

// another way:
void CursorLatLonView::updateLonDirection()
{
  _lonDir = _imageData->getLonDirection();
  
  if (_lonDir == EAST)
    updateValue(_lonDirectionLabel, "E", _lonEnable);
  else if (_lonDir == WEST)
    updateValue(_lonDirectionLabel, "W", _lonEnable);
  else 
    updateValue(_lonDirectionLabel, "X", _lonEnable);
}

void CursorLatLonView::update() 
{
  updateLonDirection();
  CursorBasicView::update();
}
