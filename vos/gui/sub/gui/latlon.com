$!****************************************************************************
$!
$! Build proc for MIPL module latlon
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:27
$!
$! Execute by entering:		$ @latlon
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
$ write sys$output "*** module latlon ***"
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
$ write sys$output "Invalid argument given to latlon.com file -- ", primary
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
$   if F$SEARCH("latlon.imake") .nes. ""
$   then
$      vimake latlon
$      purge latlon.bld
$   else
$      if F$SEARCH("latlon.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake latlon
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @latlon.bld "STD"
$   else
$      @latlon.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create latlon.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack latlon.com -mixed -
	-s CursorLatLonView.cc LatLonBarCmd.cc LatLonBar.cc SetLatLonTypeCmd.cc -
	   ImageToImageWindowGlue.cc -
	-i latlon.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create CursorLatLonView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// CursorLatLonView.cc
//
//	View for displaying the latitude and longitude 
//      corresponding to the (x,y) coordinates of the cursor
//
//	Subclass of CursorBasicView.  
//
///////////////////////////////////////////////////////////////
#include "CursorLatLonView.h"
#include "ImageData.h"
#include "ErrorDialogManager.h"
#include "CmdList.h"
#include "OptionCmdMenu.h"
#include <Xm/RowColumn.h>	
#include <Xm/Form.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include "SetLatLonTypeCmd.h"
#include <stdio.h>
#include <iostream>
using namespace std;
#include "NoOpCmd.h"
#include "MenuCmdList.h"


//#define DIAG_LL_TYPE // uncomment this to see which projection type is used

///////////////////////////////////////////////////////////////
//	CONSTRUCTOR
///////////////////////////////////////////////////////////////
CursorLatLonView::CursorLatLonView(Widget parent, const char *name, 
			CursorModel *cursorModel, ImageData *imageData, 
			unsigned char bitFlags)
	: CursorBasicView(parent, name, cursorModel, imageData, bitFlags)
{

  _selectionWidget = NULL;  // kept so that later, we can turn it on/off
  
  _lonDir = UNDEFdirection;
  
  _formLat = NULL;
  _labelLat = NULL;
  _textfieldLat = NULL;
  
  _formLon = NULL;
  _labelLon = NULL;
  _textfieldLon = NULL;
  
  _latLonType = CENTRIC; 
  
  createDisplay(); // this will attach the view to the model
  stopTracking();
}

///////////////////////////////////////////////////////////////
//	DESTRUCTOR
///////////////////////////////////////////////////////////////
CursorLatLonView::~CursorLatLonView(  )
{

}

///////////////////////////////////////////////////////////////
//	cursorMoved() .. implements pure virtual in abstract
//		updates each LatLon display with new LatLon
//		values under cursor.
///////////////////////////////////////////////////////////////

void CursorLatLonView::cursorMoved(XvicImageCallbackStruct * cb )
{
  cursorMoved(cb->x, cb->y);
}

#ifdef DIAG_LL_TYPE
   static int num_cursor_moves = 0;
#endif

void CursorLatLonView::cursorMoved(int x, int y)
{
#ifdef DIAG_LL_TYPE
  if((num_cursor_moves % 100) == 0) {
    if (_latLonType == CENTRIC)
      cout << "using CENTRIC projection type" << endl;
    else
      cout << "using DETIC projection type" << endl;
  }
#endif

  
  char buf[132];
  double latVal, lonVal;
  int status, type;
  
  if( _latLonType == CENTRIC )
    type = 1;
  else 
    type = 2;
  
  status = _imageData->lineSampToLatLon( (double) y, (double) x,
					 &latVal, &lonVal, type );
  
  // UPDATE Lon
  if(status == 1)
    sprintf(buf, "%.10g", lonVal);
  else   // cursor was probably out of range
    sprintf(buf, "+++");
  updateValue(_textfieldLon, buf, _lonEnable);
  
  
  // UPDATE Lat
  if(status == 1)
    sprintf(buf, "%.10g", latVal); 
  else
    sprintf(buf, "+++");
  updateValue(_textfieldLat, buf, _latEnable);
}

///////////////////////////////////////////////////////////////
//	createCursorDisplays:
//		Satisfies pure virtual function.  Creates 
//		subviews for (1) cursor lat position 
//		(2) cursor lon position.   Called 
//		by CursorBasicView::createDisplay() which is
//		itself called within constructor of this subclass.
///////////////////////////////////////////////////////////////
void CursorLatLonView::createCursorDisplays()
{ 
  // DECIPHER BIT FLAGS
  _latEnable = False;
  _lonEnable = False;
  if ( _bitFlags && (unsigned char) 1  )
    _latEnable = True;
  if ( _bitFlags && (unsigned char) 2  )
    _lonEnable = True;
  
  if (_latEnable && _lonEnable) {
    
    _latLonForm = XtVaCreateManagedWidget("LatLonForm",
					  xmFormWidgetClass,
					  _w,
					  NULL);
    // CREATE LABEL
    _labelLat = XtVaCreateManagedWidget("LatLabel", 
					xmLabelWidgetClass, 
					_latLonForm,
					XmNleftAttachment, XmATTACH_FORM,
					XmNtopAttachment, XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_FORM,
					NULL ); 
    // LAT stuff:	  
    _textfieldLat = XtVaCreateManagedWidget("textFieldLat", 
					    xmTextFieldWidgetClass,
					    _latLonForm,
					    XmNleftAttachment, XmATTACH_WIDGET,
					    XmNleftWidget, _labelLat,
					    NULL);



    // LON stuff:
    _labelLon = XtVaCreateManagedWidget("LonLabel", 
					xmLabelWidgetClass, _latLonForm,
					XmNleftAttachment, XmATTACH_WIDGET,
					XmNleftWidget, _textfieldLat,
					XmNtopAttachment, XmATTACH_FORM,
					XmNbottomAttachment, XmATTACH_FORM,
					NULL );
    
    _textfieldLon = XtVaCreateManagedWidget("textFieldLon", 
					   xmTextFieldWidgetClass, _latLonForm,
					   XmNleftAttachment, XmATTACH_WIDGET,
					   XmNleftWidget, _labelLon,
					   NULL);

    // E/W (longitude direction) stuff:
    _lonDirectionLabel = XtVaCreateManagedWidget("lonDirectionLabel",
					    xmTextFieldWidgetClass,
					    _latLonForm,
					    XmNleftAttachment, XmATTACH_WIDGET,
				            XmNleftWidget, _textfieldLon,
					    XmNshadowThickness, 0,
					    XmNeditable, False,
					    XmNcolumns, 2,
					    XmNvalue, "UU",
					    XmNcursorPositionVisible, False,
					    NULL );


    // set up menu for lat/lon options
    
    MenuCmdList *latLonList = new MenuCmdList();
    
    // this Cmd doesn't do anything, it's just a "label"
    Cmd *dummyLabel = new NoOpCmd("Projection Type", FALSE);
    latLonList->addOption(dummyLabel); 

    latLonList->addSeparator();

    // the last arg, a CmdList, is NULL since we don't want to automatically
    // be added to the list (we do addOption() instead)
    Cmd *deticLatLonCmd = new SetLatLonTypeCmd("Planetodetic", TRUE,
					       this, DETIC, NULL);
    
    latLonList->addOption(deticLatLonCmd);
    

    Cmd *centricLatLonCmd = new SetLatLonTypeCmd("Planetocentric", TRUE,
						 this, CENTRIC, NULL);

    latLonList->addOption(centricLatLonCmd);

    // set up selection / labels
    OptionCmdMenu *ci = new OptionCmdMenu(_latLonForm, "!at/Lon Type",
					  latLonList); // no applyCmdList here
    _latLonTypeWidget = ci->baseWidget();


    XtVaSetValues(_latLonTypeWidget, 
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, _lonDirectionLabel,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);

    int IFactive;

    // check whether we have an MP object; if not, we can't do lat/lon, so
    // we "grey-out" the interface

    if (_imageData == NULL)
      IFactive = FALSE;
    else if (!_imageData->isMapProjInfoPresent())
      IFactive = FALSE;
    else
      IFactive = TRUE;
    
    enableLatLonSelect(IFactive);   // deactivate the interface

    ci->manage();
  }
}


///////////////////////////////////////////////////////////////
//	removeCursorDisplays()
//		Removes widgets created by createCursorDisplays.
///////////////////////////////////////////////////////////////
void CursorLatLonView::removeCursorDisplays()
{
        removeSubView(&_formLat, &_labelLat, &_textfieldLat);
	removeSubView(&_formLon, &_labelLon, &_textfieldLon);
}

///////////////////////////////////////////////////////////////
//	blankSubViews()
//		Blank out all views - cursor out of range.  
//		Called by abstract class.
///////////////////////////////////////////////////////////////
void CursorLatLonView::blankSubViews()
{
	// BLANK OUT Lat
	updateValue(_textfieldLat, "   ", _latEnable);

	// BLANK OUT Lon
	updateValue(_textfieldLon, "   ", _lonEnable);
}

// another way:
void CursorLatLonView::updateLonDirection()
{
  _lonDir = _imageData->getLonDirection();
  
  if (_lonDir == EAST)
    updateValue(_lonDirectionLabel, "E", _lonEnable);
  else if (_lonDir == WEST)
    updateValue(_lonDirectionLabel, "W", _lonEnable);
  else 
    updateValue(_lonDirectionLabel, "X", _lonEnable);
}

void CursorLatLonView::update() 
{
  updateLonDirection();
  CursorBasicView::update();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LatLonBarCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// LatLonBarCmd.cc: Creates the LatLonBar Command for managing
//                whether the LatLonBar is displayed or not
//                from the dialog check box in Preferences
///////////////////////////////////////////////////////////
#include "LatLonBarCmd.h"
#include "ImageDisplayer.h"
#include <iostream>
using namespace std;
#include <stdint.h>


LatLonBarCmd::LatLonBarCmd( const char *name, int active, ImageDisplayer *obj )
                                                       : Cmd ( name, active )
{
  // Save the image view and determine the current state to set
  //  the check box appropriately.
  
  _imageView = obj;
  int value = (int) _imageView->IsLatLonBarDisplayed();
  _value = (CmdValue) (uintptr_t) value;
  newValue();
}

void LatLonBarCmd::doit()
{
  // Save the old value for a rainy day and change to the new value;
  
  _oldValue = _imageView->IsLatLonBarDisplayed();
  _imageView->showLatLonBar((_value != 0));
}

void LatLonBarCmd::undoit()
{
  // just set the value to FALSE (don't keep track of the old value)
  
  _imageView->showLatLonBar(FALSE);
  _value = (CmdValue) FALSE;
  newValue();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LatLonBar.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetLatLonTypeCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SetLatLonTypeCmd.cc: 
/////////////////////////////////////////////////////////////
#include "SetLatLonTypeCmd.h"
//#include <iostream>
using namespace std;

#include "CursorLatLonView.h"

SetLatLonTypeCmd::SetLatLonTypeCmd ( const char *name, int active,
				     CursorLatLonView *cursorView,
				     LatLonType latLonType, 
				     CmdList *list=NULL )
	: RadioCmd ( name, active, list )
{
    _cursorView = cursorView;
    _latLonType = latLonType;

    LatLonType value = (LatLonType) _cursorView->getLatLonType();
    if (value == _latLonType)
      _value = (CmdValue) TRUE;
    else
      _value = (CmdValue) FALSE;
    newValue ( );
}

void SetLatLonTypeCmd::doit ( )
{

  if (!_value) return;

  LatLonType type = (LatLonType) _cursorView->getLatLonType();
  if (type != _latLonType) 
    _cursorView->setLatLonType(_latLonType);
}      
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageToImageWindowGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ImageToImageWindowGlue: 
////////////////////////////////////////////////////////////////////////
#include "ImageToImageWindowGlue.h"
#include "ImageData.h"
#include "ImageWindow.h"
#include "ImageDisplayer.h"

ImageToImageWindowGlue::ImageToImageWindowGlue (ImageData *model,
						ImageWindow *window)
		: BasicImageView("glue", model)
{
    _window = window;

   _model->attachView(this);
}

ImageToImageWindowGlue::~ImageToImageWindowGlue ( )
{
    // Detach itself from the model so that the are no more updates sent
    _model->detachView(this);
}

////////////////////////////////////////////////////////////////////////
// This is the meat of the class.  Whenever the image changes,
// reset the Lat/Lon interfaces.  Note that this function can also
// be used to perform other ImageWindow-related tasks that need to be
// done when the image changes or a new image is loaded.
////////////////////////////////////////////////////////////////////////
void ImageToImageWindowGlue::update()
{
  // Lat/Lon interfaces: check for an MP object and de/activate the IFs
  // accordingly
  
  if( _window->getImageData()->isMapProjInfoPresent() )
    _window->enableLatLonIFs(True); 
  else {
    _window->enableLatLonIFs(False);

    // If the image has no MP info but the LatLonBar is still showing, hide it
    // (i.e.; when the user loads a new image that can't take advantage of the
    //  lat/lon capability)
    
    ImageDisplayer *displayer = _window->getImageDisplayer();

    if ( displayer == NULL ) return;
    
    if( displayer->IsLatLonBarDisplayed())
      displayer->showLatLonBar( False );
  }
}

void ImageToImageWindowGlue::updatePart(int /* flags */) { }
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create latlon.imake
#define SUBROUTINE latlon
#define MODULE_LIST LatLonBarCmd.cc CursorLatLonView.cc LatLonBar.cc \
                    SetLatLonTypeCmd.cc ImageToImageWindowGlue.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP
$ Return
$!#############################################################################
