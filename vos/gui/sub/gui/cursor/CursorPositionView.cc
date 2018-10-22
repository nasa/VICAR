///////////////////////////////////////////////////////////////
// CursorPositionView.cc
//
//	View for displaying coordinates of cursor location 
//	within image.   Subclass of CursorBasicView.  
//
///////////////////////////////////////////////////////////////
#include "CursorPositionView.h"
#include "CursorModel.h"
#include "ImageData.h"
#include <stdio.h>

///////////////////////////////////////////////////////////////
// Constructor
///////////////////////////////////////////////////////////////
CursorPositionView::CursorPositionView(Widget parent, const char *name,
			CursorModel *cursorModel, ImageData *imageData,
			unsigned char bitFlags)
	: CursorBasicView(parent, name, cursorModel, imageData, bitFlags)
{
	_formCursorY = NULL;
	_labelCursorY = NULL;
	_textfieldCursorY = NULL;

	_formCursorX = NULL;
	_labelCursorX = NULL;
	_textfieldCursorX = NULL;

	_cursorXEnable = False;
	_cursorYEnable = False;

	createDisplay();
}

///////////////////////////////////////////////////////////////
// Destructor
///////////////////////////////////////////////////////////////
CursorPositionView::~CursorPositionView(  )
{
	// empty
}

///////////////////////////////////////////////////////////////
//	cursorMoved:
//		Satisfies pure virtual function.  Called by
//		CursorMovedCallback when the cursor 
//		moves so that cursor position displays are 
//		are updated with new values.  
// 		Note adding 1 to the position value because 
// 		image origin is at 0 from widget's perspective.
///////////////////////////////////////////////////////////////
void CursorPositionView::cursorMoved(XvicImageCallbackStruct * cb)
{
        int Line, Sample;

        Line=1;
        Sample=1;

        // Transform display coords to image coords
        _imageData->transDisplayToImageCoords(cb->x, cb->y, &Sample, &Line);

	cursorMoved(Sample, Line);
}

///////////////////////////////////////////////////////////////
//      cursorMoved:
//		Coordinates are specified explicitly
//		Note that the origin is at (1, 1)
///////////////////////////////////////////////////////////////
void CursorPositionView::cursorMoved(int x, int y)
{
	char buf[132];

	// UPDATE CURSOR X
	sprintf ( buf, "%5d", x );
	updateValue( _textfieldCursorX, buf, _cursorXEnable );

	// UPDATE CURSOR Y
	sprintf ( buf, "%5d", y );
	updateValue( _textfieldCursorY, buf, _cursorYEnable );	
}

///////////////////////////////////////////////////////////////
//	createCursorDisplays:
//		Satisfies pure virtual function.  Creates 
//		subviews for (1) cursor x position 
//		(2) cursor y position.   Called 
//		by CursorBasicView::createDisplay() which is
//		itself called within constructor of this subclass.
//		Sounds messy but it simplifies creating
//		subclasses to CursorBasicView.  
///////////////////////////////////////////////////////////////
void CursorPositionView::createCursorDisplays()
{
	// DECIPHER BIT FLAGS
	_cursorXEnable = False;
	_cursorYEnable = False;
	if ( _bitFlags && (unsigned char) 1  )
		_cursorXEnable = True;
	if ( _bitFlags && (unsigned char) 2  )
		_cursorYEnable = True;

	// CREATE DISPLAYS
	addNewSubView( _w,  "CursorY",
			&_formCursorY, &_labelCursorY, &_textfieldCursorY,
			_cursorYEnable );
	addNewSubView( _w, "CursorX",
			&_formCursorX, &_labelCursorX, &_textfieldCursorX,
			_cursorXEnable );
}

///////////////////////////////////////////////////////////////
//	removeCursorDisplays:
//		Removes widgets created by createCursorDisplays.
///////////////////////////////////////////////////////////////
void CursorPositionView::removeCursorDisplays()
{
	removeSubView(&_formCursorY, &_labelCursorY, &_textfieldCursorY);
	removeSubView(&_formCursorX, &_labelCursorX, &_textfieldCursorX);
}

///////////////////////////////////////////////////////////////
//	blankSubViews:
//		Satisfies pure virtual function to blank
//		out cursor display subviews when 
//		cursor is out of range.  Called by CursorBasicView.
///////////////////////////////////////////////////////////////
void CursorPositionView::blankSubViews()	
{
	// UPDATE CURSOR X
	updateValue( _textfieldCursorX, "     ", _cursorXEnable );

	// UPDATE CURSOR Y 
	updateValue( _textfieldCursorY, "     ", _cursorYEnable );
}
