////////////////////////////////////////////////////////////////
// CursorPositionView.h
//
//	View for displaying coordinates of cursor location 
//	within image.   Subclass of CursorBasicView.  
//
///////////////////////////////////////////////////////////////
#ifndef CURSORPOSITIONVIEW_H
#define CURSORPOSITIONVIEW_H
#include "CursorBasicView.h"
#include "XvicImage.h"

class CursorModel;

class CursorPositionView : public CursorBasicView {

    protected:

	// VARIABLES:  POSITION X DISPLAY
	Widget	_labelCursorX, _textfieldCursorX, _formCursorX;
	Boolean	_cursorXEnable;
	char 	*_xLabel; // label set by resource

	// VARIABLES:  POSITION Y DISPLAY
	Widget	_labelCursorY, _textfieldCursorY, _formCursorY;
	Boolean	_cursorYEnable;
	char 	*_yLabel; // label set by resource

	// CREATE DISPLAYS FOR EACH OF X AND Y (OR LINE AND SAMPLE)
	virtual void createCursorDisplays();
	virtual void removeCursorDisplays();

    public:

	CursorPositionView(Widget parent, const char *name, 
			CursorModel *cursorModel, ImageData *imageData, 
			unsigned char bitFlags);
	virtual ~CursorPositionView();

	// UPDATE DISPLAYS WHEN CURSOR MOVES
	virtual void cursorMoved(XvicImageCallbackStruct * cb );
	virtual void cursorMoved(int x, int y);

	// BLANK OUT ALL VIEWS WHEN CURSOR IS OUT OF RANGE
	virtual void blankSubViews();

	virtual const char *const className() { return "CursorPositionView"; }

};
#endif

