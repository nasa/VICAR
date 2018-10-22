////////////////////////////////////////////////////////////////
// CursorBasicView.h
//
//	Abstract class for views containing cursor information.
//	
///////////////////////////////////////////////////////////////
#ifndef CURSORBASICVIEW_H
#define CURSORBASICVIEW_H
#include "UIComponent.h"
#include "XvicImage.h"
#include "ImageData.h"

class CursorModel;

class CursorBasicView : public UIComponent {

    protected:

	static void cursorMovedCallback( Widget,
			XtPointer clientData, XtPointer callData);

	CursorModel *_cursorModel;	

	ImageData *_imageData;	

	unsigned char _bitFlags;

	// ADD A LABEL-TEXTFIELD COMPONENT FOR DISPLAYING CURSOR INFO - 
	// MAY BE CALLED MORE THAN ONCE BY SUBCLASS
	virtual void addNewSubView(Widget w, const char *displayName, 
			Widget *form, Widget *label, Widget *textfield, 
			Boolean enabled);
	virtual void removeSubView(Widget *form, Widget *label, Widget *textfield);

	// UPDATE THE DISPLAY WITH THE LATEST VALUE
	virtual void updateValue(Widget w, const char *text, Boolean enabled);

	// Set the width of the text field
	virtual void setWidth(Widget w, int width);

	// START-STOP TRACKING CALLED BY MODEL (WHEN USER SETS CONTROL)
 	virtual void startTracking();
	virtual void stopTracking();

	Boolean isCursorOnImage(XvicImageCallbackStruct * cb) 
		{ return (cb->on_screen &&
			cb->x >= 0 && cb->x < _imageData->getNumbSamples() &&
			cb->y >= 0 && cb->y < _imageData->getNumbLines());
		}

	// PURE VIRTUAL FUCTION:  CALLED TO CREATE INDIVIDUAL SUBVIEWS
	virtual void createCursorDisplays() =0;  // pure virtual called from constructor
	virtual void removeCursorDisplays() =0;  // pure virtual

	// CREATE DISPLAY IS CALLED BY CONSTRUCTOR  IN SUBCLASS
	virtual void createDisplay();

	CursorBasicView(Widget parent, const char * name,
			CursorModel *cursorModel, ImageData *imageData,
			unsigned char bitFlags);
	virtual ~CursorBasicView();

    public:

	// UPDATE CALLED BY MODEL TO UPDATE ITS VIEWS
	virtual void  update();

	virtual void cursorMoved ( XvicImageCallbackStruct * cb ) = 0;
	virtual void cursorMoved ( int, int ) { }

	// TO FILL IN BLANK VALUES IN ALL ITS DISPLAYS
	virtual void blankSubViews() = 0;

	// CLASSNAME
	virtual const char *const className() { return "CursorBasicView" ; } 
};
#endif
