////////////////////////////////////////////////////////////////
// CursorDnView.h
//
//	View for displaying DN pixel values under cursor.
//	Subclass of CursorBasicView.  
//
///////////////////////////////////////////////////////////////
#ifndef CURSORDNVIEW_H
#define CURSORDNVIEW_H
#include "CursorBasicView.h"
#include <Xm/Xm.h>

class ImageToCursorDnViewGlue;
class CursorModel;
class ImageData;

class CursorDnView : public CursorBasicView {

    protected:

	// VARIABLES:  RED DN DISPLAY 
	Widget		_labelRedDn, _textfieldRedDn, _formRedDn;
	Boolean		_redDnEnable;	// on-off 	

	// VARIABLES:  GREEN DN DISPLAY
	Widget		_labelGreenDn, _textfieldGreenDn, _formGreenDn;
	Boolean		_greenDnEnable;	// on-off 

	// VARIABLES:  BLUE DN DISPLAY
	Widget		_labelBlueDn,  _textfieldBlueDn, _formBlueDn;
	Boolean		_blueDnEnable;	// on-off

	// VARIABLES:  BW DN DISPLAY
	Widget		_labelBwDn, _textfieldBwDn, _formBwDn;
	Boolean		_bwDnEnable;	// on-off 

	ImageToCursorDnViewGlue *_glue;

	// LOCAL FUNCTION FOR GETTING A DN VALUE AND ITS STRING EQUIV.   
	virtual void getValueString(ColorType color, int x, int y, 
			char * newValueString , Boolean enabled = True);

    public:

	CursorDnView(Widget, const char *, CursorModel *, ImageData *,
							unsigned char);
	virtual ~CursorDnView() ;

	// CREATE DISPLAYS FOR RED, GREEN, BLUE, BLACK-WHITE PIXEL DN VALUES
	virtual void    createCursorDisplays();
	virtual void    removeCursorDisplays();

	// UPDATE DISPLAYS WHEN CURSOR MOVES
	virtual void cursorMoved(XvicImageCallbackStruct * cb );
	virtual void cursorMoved(int x, int y);

	// BLANK OUT ALL VIEWS WHEN CURSOR IS OUT OF RANGE
	virtual void blankSubViews();

	virtual const char *const className() { return "CursorDnView" ; } 

};
#endif

