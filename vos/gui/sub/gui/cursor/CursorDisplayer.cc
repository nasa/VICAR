///////////////////////////////////////////////////////////////////
//  CursorDisplayer.cc
//
//	Instantiates all cursor views, model and maybe 
//	someday a controller.   
//
///////////////////////////////////////////////////////////////////
#include "CursorDisplayer.h"
#include "BasicImageView.h"
#include "ImageData.h"
#include "CursorDnView.h"
#include "CursorPositionView.h"
#include "CursorLatLonView.h"
#include "CursorModel.h"
#include <Xm/RowColumn.h>

///////////////////////////////////////////////////////////////////
//	RESOURCE DEFAULTS
///////////////////////////////////////////////////////////////////
String CursorDisplayer::_defaults [ ] = {
	(char *)"*isAligned:		True",
	(char *)"*orientation:		XmHORIZONTAL",
	(char *)"*entryAlignment:   	XmALIGNMENT_END",
	(char *)"*packing:		XmPACK_COLUMN",
	(char *)".numColumns:	1",
	(char *)"*adjustLast:		False",
              NULL,
};
///////////////////////////////////////////////////////////////////
//	CONSTRUCTOR
///////////////////////////////////////////////////////////////////
CursorDisplayer::CursorDisplayer( Widget parent, char *name, 
				  ImageData *dataModel, 
				  BasicImageView *imageView )
                                                            : UIComponent(name)
{

//	SET DEFAULT RESOURCES
    setDefaultResources ( parent, _defaults );

//	CREATE ROW-COLUMN TO HOLD ALL CURSOR DISPLAYS     
    _w = XtVaCreateManagedWidget ( _name,  xmRowColumnWidgetClass, parent, 
				   NULL, NULL );
    installDestroyHandler();

//	CREATE CURSOR MODEL
    _cursorModel = new CursorModel(  True,  imageView->getWidget() );

//	CREATE VIEW:  CURSOR DN VALUE 
    unsigned char bitFlags = (unsigned char) 255; 
           // see Note #1 for explaination of bitflags, below.
    _cursorDnView = new CursorDnView( _w, "cursorDnView", _cursorModel, 
				      dataModel,  bitFlags );
    _cursorDnView->manage();

//	CREATE VIEW: CURSOR POSITION
    _cursorPositionView = new CursorPositionView( _w,
						  "cursorPositionView", 
						  _cursorModel, dataModel,
						  bitFlags );
    _cursorPositionView->manage();

// CREATE VIEW: CURSOR LAT/LON VALUES
    _cursorLatLonView= new CursorLatLonView(_w, "cursorLatLonView", 
					    _cursorModel, dataModel, bitFlags);
    _cursorLatLonView->manage();
}

///////////////////////////////////////////////////////////////////
//	DESTRUCTOR
///////////////////////////////////////////////////////////////////
CursorDisplayer::~CursorDisplayer()
{
	delete _cursorDnView;
	delete _cursorPositionView;
	
	delete _cursorLatLonView;
	
	delete _cursorModel;
}

/*
Note #1:
    bitflags may be used to turn on/off individual subviews.  For example,
    CursorPositionView creates 2 subviews: cursor x (sample) and cursor y 
    (line).  If the cursor x display is not desired, the appropriate bitflag
    may be used prevent that from being displayed.   See appropriate view to
    see how bitflags are assigned.
*/
