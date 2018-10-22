////////////////////////////////////////////////////////////////
// CursorDnView.cc
//
//	View for displaying DN pixel values under cursor.
//	Subclass of CursorBasicView.  
//
///////////////////////////////////////////////////////////////
#include "CursorDnView.h"
#include "ImageToCursorDnViewGlue.h"
#include "ImageData.h"
#include "ErrorDialogManager.h"
#include <Xm/RowColumn.h>	

///////////////////////////////////////////////////////////////
//	CONSTRUCTOR
///////////////////////////////////////////////////////////////
CursorDnView::CursorDnView(Widget parent, const char *name, 
			CursorModel *cursorModel, ImageData *imageData, 
			unsigned char bitFlags)
	: CursorBasicView(parent, name, cursorModel, imageData, bitFlags)
{

	_formRedDn = NULL;
	_labelRedDn = NULL;
	_textfieldRedDn = NULL;

	_formGreenDn = NULL;
	_labelGreenDn = NULL;
	_textfieldGreenDn = NULL;

	_formBlueDn = NULL;
	_labelBlueDn = NULL;
	_textfieldBlueDn = NULL;

	_formBwDn = NULL;
	_labelBwDn = NULL;
	_textfieldBwDn = NULL;

	_redDnEnable = False;
	_greenDnEnable = False;
	_blueDnEnable = False;
	_bwDnEnable = False;

	createDisplay();

	_glue = new ImageToCursorDnViewGlue(imageData, this);
}

///////////////////////////////////////////////////////////////
//	DESTRUCTOR
///////////////////////////////////////////////////////////////
CursorDnView::~CursorDnView(  )
{
	delete _glue;
}

///////////////////////////////////////////////////////////////
//	getValueString()
//		local function for getting a dn value and
//		its string equivalent.  Called once for 
//		each DN.
///////////////////////////////////////////////////////////////
void CursorDnView::getValueString(ColorType color, int x, int y, 
				char *newValueString, Boolean enabled)
{
	StatusType  status;
	unsigned char pixelBuffer[32];	// waay long enough for any pixel

	strcpy(newValueString, " ");
	if (enabled) {
		status = _imageData->readPixel( color,  x,  y,  pixelBuffer );
		if ( status == imSUCCESS ) {
			_imageData->getPixelType().printPixel(
					(void *)pixelBuffer, newValueString);
		}
		else {
		    if (!_imageData->errorMsgIssued()) {
			theErrorDialogManager->post(_imageData->getErrorMsg());
		    }
		    strcpy(newValueString, "***");
		}
	}
}

///////////////////////////////////////////////////////////////
//	cursorMoved() .. implements pure virtual in abstract
//		updates each DN display with new DN
//		values under cursor.
///////////////////////////////////////////////////////////////
void CursorDnView::cursorMoved(XvicImageCallbackStruct * cb )
{
	cursorMoved(cb->x, cb->y);
}

void CursorDnView::cursorMoved(int x, int y)
{
	char buf[132];

	// UPDATE RED DN
        getValueString(RED, x, y, buf, (_redDnEnable));
        updateValue(_textfieldRedDn, buf, _redDnEnable);

	// UPDATE GREEN DN
        getValueString(GREEN, x, y, buf, (_greenDnEnable));
        updateValue(_textfieldGreenDn, buf, _greenDnEnable);

	// UPDATE BLUE DN
        getValueString(BLUE, x, y, buf, (_blueDnEnable));
        updateValue(_textfieldBlueDn, buf, _blueDnEnable);

	// UPDATE BW DN
        getValueString(BWcolor, x, y, buf, (_bwDnEnable));
        updateValue(_textfieldBwDn, buf, _bwDnEnable);
}

///////////////////////////////////////////////////////////////
//	createCursorDisplays()
//		creates 4 displays: Red, Green, Blue, Black&white
//		and inits other variables needed later.
///////////////////////////////////////////////////////////////
void CursorDnView::createCursorDisplays()
{
	// GET COLOR MODE INFO.  
	// When mode=bw, we disable the rgb displays, and vice versa.

	ModeType mode = _imageData->getMode();

	// DECIPHER BIT FLAGS
	_redDnEnable = False;
	_greenDnEnable = False;
	_blueDnEnable = False;
	_bwDnEnable = False;
	if ( _bitFlags && (unsigned char) 1  && mode == COLORmode )
		_redDnEnable = True;
	if ( _bitFlags && (unsigned char) 2  && mode == COLORmode )
		_greenDnEnable = True;
	if ( _bitFlags && (unsigned char) 4  && mode == COLORmode )
		_blueDnEnable =   True;
	if ( _bitFlags && (unsigned char) 8  && mode == BWmode )
		_bwDnEnable = True  ;

	// CREATE DN DISPLAY:      RED, GREEN, BLUE,  BLACK&WHITE
	addNewSubView( _w, "RedDn",
			&_formRedDn, &_labelRedDn, &_textfieldRedDn,
			_redDnEnable );
	setWidth(_textfieldRedDn, _imageData->getPixelType().neededWidth());
	addNewSubView( _w, "GreenDn",
			&_formGreenDn, &_labelGreenDn, &_textfieldGreenDn,
			_greenDnEnable );
	setWidth(_textfieldGreenDn, _imageData->getPixelType().neededWidth());
	addNewSubView( _w, "BlueDn",
			&_formBlueDn, &_labelBlueDn, &_textfieldBlueDn,
			_blueDnEnable );
	setWidth(_textfieldBlueDn, _imageData->getPixelType().neededWidth());
	addNewSubView( _w, "BwDn",
			&_formBwDn, &_labelBwDn, &_textfieldBwDn,
			_bwDnEnable );
	setWidth(_textfieldBwDn, _imageData->getPixelType().neededWidth());
}

///////////////////////////////////////////////////////////////
//	removeCursorDisplays()
//		Removes widgets created by createCursorDisplays.
///////////////////////////////////////////////////////////////
void CursorDnView::removeCursorDisplays()
{
       	removeSubView(&_formRedDn, &_labelRedDn, &_textfieldRedDn);
	removeSubView(&_formGreenDn, &_labelGreenDn, &_textfieldGreenDn);
  	removeSubView(&_formBlueDn, &_labelBlueDn, &_textfieldBlueDn);
	removeSubView(&_formBwDn, &_labelBwDn, &_textfieldBwDn);
}

///////////////////////////////////////////////////////////////
//	blankSubViews()
//		Blank out all views - cursor out of range.  
//		Called by abstract class.
///////////////////////////////////////////////////////////////
void CursorDnView::blankSubViews()
{
	// BLANK OUT RED DN
	updateValue(_textfieldRedDn, "   ", _redDnEnable);

	// BLANK OUT  GREEN DN
	updateValue(_textfieldGreenDn, "   ",  _greenDnEnable);

	// BLANK OUT  BLUE DN
	updateValue(_textfieldBlueDn, "   ", _blueDnEnable);

	// BLANK OUT B&W DN
	updateValue(_textfieldBwDn, "   ", _bwDnEnable);
}
