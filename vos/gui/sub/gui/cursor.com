$!****************************************************************************
$!
$! Build proc for MIPL module cursor
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:22
$!
$! Execute by entering:		$ @cursor
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module cursor ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to cursor.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("cursor.imake") .nes. ""
$   then
$      vimake cursor
$      purge cursor.bld
$   else
$      if F$SEARCH("cursor.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake cursor
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @cursor.bld "STD"
$   else
$      @cursor.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cursor.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cursor.com -mixed -
	-s CursorBasicView.cc CursorDnView.cc CursorModel.cc -
	   CursorPositionView.cc CursorDisplayer.cc -
	-i cursor.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create CursorBasicView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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



$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CursorDnView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CursorModel.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// CursorModel.cc
//
//	Model for subclasses of CursorBasicView.
//
////////////////////////////////////////////////////////////////
#include "CursorModel.h"
#include "ViewMacros.h"
#include "CursorBasicView.h"
#include "XvicImage.h"

////////////////////////////////////////////////////////////////
//	Constructor
////////////////////////////////////////////////////////////////
CursorModel::CursorModel(Boolean trackingEnabled, Widget iw) 
{
	// INIT ARGS
	_trackingEnabled = trackingEnabled;
	_iw = iw; 				// SAVE WIDGET

	// SET RESOURCE IN WIDGET TO TRACK 
	XtVaSetValues(_iw, XvicNtrackFloatingCursor, True, NULL);

	// ATTACHMENT INIT
	_numViews = 0;
	_views = NULL;
}

////////////////////////////////////////////////////////////////
//	Destructor
////////////////////////////////////////////////////////////////
CursorModel::~CursorModel()
{
	// SET RESOURCE IN WIDGET NOT TO TRACK 
	XtVaSetValues(_iw, XvicNtrackFloatingCursor, False, NULL);

	if (_views != NULL)
		delete [] _views;
}

////////////////////////////////////////////////////////////////
//	attachView
////////////////////////////////////////////////////////////////
void CursorModel::attachView(CursorBasicView * view)
{
	AttachViewMacro(CursorBasicView, _views, _numViews, view);
	view->update();
}

////////////////////////////////////////////////////////////////
//      detachView
////////////////////////////////////////////////////////////////
void CursorModel::detachView(CursorBasicView *view)
{
        DetachViewMacro(CursorBasicView, _views, _numViews, view);
}

////////////////////////////////////////////////////////////////
//	updateView
////////////////////////////////////////////////////////////////
void  CursorModel::updateViews()
{
      for (int i=0; i<_numViews; i++)
              _views[i]->update();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CursorPositionView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CursorDisplayer.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create cursor.imake
#define SUBROUTINE cursor
#define MODULE_LIST CursorBasicView.cc CursorDnView.cc CursorModel.cc \
   CursorPositionView.cc CursorDisplayer.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP


$ Return
$!#############################################################################
