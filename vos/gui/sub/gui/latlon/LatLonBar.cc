///////////////////////////////////////////////////////
// LatLonBar.cc: (un)display the Lat/Lon Bar
////////////////////////////////////////////////////////
#include "LatLonBar.h"
#include "BasicImageView.h"
#include "ImageDisplayView.h"
#include "ImageWindow.h"
#include "CursorModel.h"
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include "CursorLatLonView.h"

LatLonBar::LatLonBar ( Widget parent, const char * name,
			BasicImageView *imageView, ImageData *imageData) 
  : UIComponent (name)
{
	// CREATE LatLon BAR TOOLBOX FOR<

        _w = XtVaCreateWidget(_name,
			      xmFrameWidgetClass,
			      parent,
			      NULL);

	installDestroyHandler();

	Widget form  = XtVaCreateManagedWidget ( "latLonForm",
						 xmFormWidgetClass,
						 _w,
						 NULL );
	// CREATE CURSOR MODEL

	CursorModel *cursorModel = new CursorModel( True,
						    imageView->getWidget() );

	// DISPLAY CURSOR POSITION

	unsigned char bitFlags = (unsigned char) 255;

	_cursorLatLonView = new CursorLatLonView( form, "cursorLatLonView",
						  cursorModel, imageData,
						  bitFlags );

	// check this: (lat needs to be on L, lon on R)
	XtVaSetValues(_cursorLatLonView->baseWidget(),
		      XmNrightAttachment, XmATTACH_FORM, // was "NONE"
 		      XmNleftAttachment, XmATTACH_FORM,
		      XmNtopAttachment, XmATTACH_FORM,
		      XmNorientation, XmHORIZONTAL,
		      XmNpacking,   XmPACK_TIGHT,
		      NULL);
	
	_cursorLatLonView->manage();
}
