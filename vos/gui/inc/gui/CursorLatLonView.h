////////////////////////////////////////////////////////////////
// CursorLatLonView.h
//
//	View for displaying the latitude and longitude 
//      corresponding to the (x,y) coordinates of the cursor
//
//	Subclass of CursorBasicView.  
//
///////////////////////////////////////////////////////////////
#ifndef CURSORLATLONVIEW_H
#define CURSORLATLONVIEW_H

#include "CursorBasicView.h"
#include <Xm/Xm.h>
#include "CursorModel.h"

class ImageData;

//enum LongitudeDirection { UNDEFdirection, EAST, WEST };

enum LatLonType   { CENTRIC, DETIC };

class CursorLatLonView : public CursorBasicView {

    protected:

        Widget _latLonForm;
	
	Widget _lonDirectionLabel;

	Widget _selectionWidget;

	// VARIABLES:  Latitude (line) display
	Widget		_labelLat, _textfieldLat, _formLat;
	Boolean		_latEnable;	// on-off 	

	// VARIABLES:  Longitude (samp) display
	Widget		_labelLon, _textfieldLon, _formLon;
	Boolean		_lonEnable;	// on-off 

	virtual void addNewSubView(Widget   /* w */ ,
				   char *   /* displayName */ , 
				   Widget * /* form */ ,
				   Widget * /* label */ ,
				   Widget * /* textfield */ , 
				   Boolean  /* enabled */ ) { }
			
	LatLonType _latLonType;
	LongitudeDirection _lonDir;

	Widget _latLonTypeWidget;  // the "Projection Type" button widget
	
    public:

	CursorLatLonView(Widget, const char *, CursorModel *, ImageData *,
			 unsigned char);
	virtual ~CursorLatLonView() ;

	// CREATE DISPLAYS lat/lon values
	virtual void    createCursorDisplays();
	virtual void    removeCursorDisplays();

	// UPDATE DISPLAYS WHEN CURSOR MOVES
	virtual void cursorMoved(XvicImageCallbackStruct * cb );
	virtual void cursorMoved(int x, int y);

	// BLANK OUT ALL VIEWS WHEN CURSOR IS OUT OF RANGE
	virtual void blankSubViews();

	virtual LatLonType getLatLonType() { return _latLonType; }
	virtual void setLatLonType(LatLonType n) { _latLonType = n; }
	
	virtual const char *const className() { return "CursorLatLonView" ; } 

	virtual void start() { startTracking(); }

	virtual void stop() { stopTracking(); }

	virtual void update();
	
	// call this fcn with False to "grey out" (deactivate) the interface:
	virtual void enableLatLonSelect(Boolean val) 
	     { if (_latLonTypeWidget != NULL)
	       XtSetSensitive(_latLonTypeWidget, val); }
	     
	//virtual void updateLonDirection(LongitudeDirection); 
	virtual void updateLonDirection();
};
#endif

