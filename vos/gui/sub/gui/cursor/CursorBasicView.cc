////////////////////////////////////////////////////////////////
// CursorBasicView.cc
//
//	Abstract class for views containing cursor information.
//
///////////////////////////////////////////////////////////////
#include "CursorBasicView.h"
#include "CursorModel.h"
#include "ImageData.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>
#include <string.h>

// static String  _basicCursorDefaults[ ] = {
//        		".orientation:		XmVERTICAL",
//        	 	".packing:		XmPACK_COLUMN",
// 		"*adjustLast:		False",
// 		NULL,
// };

///////////////////////////////////////////////////////////////
//	Constructor
///////////////////////////////////////////////////////////////
CursorBasicView::CursorBasicView(Widget parent, const char *name, 
			CursorModel *cursorModel, ImageData *imageData, 
			unsigned char bitFlags)	
		: UIComponent(name)
{
	// SAVE ARGS
	_cursorModel = cursorModel;
	_bitFlags = bitFlags;
	_imageData = imageData;

	// GET RESOURCE DEFAULTS
	//	setDefaultResources( parent, _basicCursorDefaults );	

	// CREATE ROW/COLUMN TO HOLD THESE CURSOR DISPLAYS
        _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent, NULL);
	
	installDestroyHandler();
}

///////////////////////////////////////////////////////////////
//	Destructor
///////////////////////////////////////////////////////////////
CursorBasicView::~CursorBasicView ()
{
	_cursorModel->detachView(this);

	XtRemoveCallback(_cursorModel->getImageWidget(),
		XvicNcursorCallback,
		&CursorBasicView::cursorMovedCallback,
		(XtPointer)this);
}

///////////////////////////////////////////////////////////////
//	updateValue:
//		May be called by "cursorMoved" function
//		in subclass. to update the value (label widget)
//		displayed in the textfield widget.
///////////////////////////////////////////////////////////////
void CursorBasicView::updateValue(Widget w, const char * text, 
				Boolean enabled = True  )
{
	if (enabled) { 
		XmTextFieldSetString (w, (char *)text);
	}
}

///////////////////////////////////////////////////////////////
//	setWidth:
//		Sets (or resets) the width of one of the text widgets.
///////////////////////////////////////////////////////////////
void CursorBasicView::setWidth(Widget w, int width)
{
	if (w) { 
		XtVaSetValues(w, XmNcolumns, width, NULL);
	}
}

///////////////////////////////////////////////////////////////
//	addNewSubView:
//		May be called by "createCursorDisplays"
//		function in subclass to create a single 
//		display group consisting of: label and a 
//		textfield. The textfield is used to show  
//              values that are updated as the cursor 
//		moves around in the image. A name is 
//              automatically created as : 
//		"label" +  <displayName>,   etc.   
///////////////////////////////////////////////////////////////
void CursorBasicView::addNewSubView( Widget w, const char * displayName,
		Widget *form, Widget * label, Widget * textfield, 
		Boolean enabled )
{
	char name[132];

	if (enabled) {
		*form = XtVaCreateManagedWidget("form", 
				xmFormWidgetClass, w, NULL);

		// CREATE VALUE:  ITS REALLY A TEXTFIELD
		strcpy(name, "textfield"); 
		strcat(name, displayName);
		*textfield = XtVaCreateManagedWidget(name, 
				xmTextFieldWidgetClass, *form,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNrightAttachment, XmATTACH_FORM,
				NULL);

		// CREATE LABEL
		strcpy(name, "label"); 
		strcat(name, displayName);
		*label = XtVaCreateManagedWidget(name, 
				xmLabelWidgetClass, *form, 
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNrightAttachment, XmATTACH_WIDGET,
				XmNrightWidget, *textfield,
				NULL ); 
	}
}

///////////////////////////////////////////////////////////////
//	removeSubView:
//		Removes widgets created by addNewSubView.
///////////////////////////////////////////////////////////////
void CursorBasicView::removeSubView(Widget *form, Widget *label,
				Widget *textfield)
{
	if (*form) {
		XtUnmanageChild(*form);
		XtDestroyWidget(*form);
	}
	*form = NULL;

	*label = NULL;
	*textfield = NULL;
}

///////////////////////////////////////////////////////////////
//	cursorMovedCallback:
//		called for all subclasses
///////////////////////////////////////////////////////////////
void CursorBasicView::cursorMovedCallback(Widget, 
			XtPointer client_data, XtPointer call_data)
{
	CursorBasicView * view = (CursorBasicView *) client_data;
	XvicImageCallbackStruct * cb = ( XvicImageCallbackStruct *) call_data;

	// BLANK CURSOR DISPLAY IF NOT IN IMAGE
	if (  view->isCursorOnImage( cb ) == False) 	
		view->blankSubViews( );

	// UPDATE CURSOR DISPLAY IF IN IMAGE
	else 
		view->cursorMoved( cb );
}

///////////////////////////////////////////////////////////////
//	startTracking:
//		is called by model when a cursor controller
//		(not implemented yet) is used to start
//		tracking.  Presumably, the user will select
//		some option in the cursor controller that 
//		will start this.
///////////////////////////////////////////////////////////////
void CursorBasicView::startTracking()
{
	Widget iw = _cursorModel->getImageWidget();

	// ADD CALLBACK
	XtAddCallback ( iw, 	
		XvicNcursorCallback,
		&CursorBasicView::cursorMovedCallback,
		( XtPointer ) this );
}

///////////////////////////////////////////////////////////////
//	stopTracking:
//		See startTracking.
///////////////////////////////////////////////////////////////
void CursorBasicView::stopTracking()
{
	Widget iw = _cursorModel->getImageWidget();

	// REMOVE CALLBACK
	XtRemoveCallback ( iw, 
		XvicNcursorCallback,
		&CursorBasicView::cursorMovedCallback,
		( XtPointer ) this );

	// SHOW BLANKS IN DISPLAY
	blankSubViews();
}

///////////////////////////////////////////////////////////////
//	createDisplay:
//		Must be called by constructor in subclass.  This
//		will call createCursorDisplays in subclass, and 
//		attach view to model.
///////////////////////////////////////////////////////////////
void CursorBasicView::createDisplay()
{
	// CREATE CURSOR DISPLAYS 
	createCursorDisplays();					

	// ATTACH VIEW  TO DATA MODEL    
	_cursorModel->attachView(this);	
}

///////////////////////////////////////////////////////////////
//	update:
//		is called by model to update all views.  This 
//		checks whether tracking controls have changed.
//		May be overridden in subclass.
///////////////////////////////////////////////////////////////
void CursorBasicView::update()
{
	// UPDATE FOR TRACKING CONTROLS
	if ( _cursorModel->isTrackingEnabled() ) {
		startTracking();
	}
	else
		stopTracking();
}



