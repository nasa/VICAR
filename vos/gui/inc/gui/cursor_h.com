$!****************************************************************************
$!
$! Build proc for MIPL module cursor_h
$! VPACK Version 1.9, Monday, October 19, 1998, 08:32:11
$!
$! Execute by entering:		$ @cursor_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module cursor_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to cursor_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("cursor_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @cursor_h.bld "STD"
$   else
$      @cursor_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cursor_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cursor_h.com -mixed -
	-s CursorBasicView.h CursorDnView.h CursorModel.h CursorPositionView.h -
	   CursorDisplayer.h ImageToCursorDnViewGlue.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create CursorBasicView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CursorDnView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CursorModel.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// CursorModel.h: Calls updates on cursor views such as position 
// view, dn view etc., when something cursor-related happens.
////////////////////////////////////////////////////////////////
#ifndef CURSORMODEL_H
#define CURSORMODEL_H
#include <Xm/Xm.h>		// only for Boolean

class CursorBasicView;

class CursorModel {

    protected:

	int _numViews;
	CursorBasicView  **_views;

	Boolean	_trackingEnabled;
	Widget _iw;

	virtual void updateViews();

    public:
  
	// CONSTRUCTOR/DESTRUCTOR
	CursorModel(Boolean trackingEnabled, Widget iw);
	virtual ~CursorModel();

	// GET IMAGE WIDGET ID
	Widget getImageWidget() { return _iw ; }

      	virtual void attachView(CursorBasicView * view);
        virtual void detachView(CursorBasicView * view);

	// IS TRACKING ENABLED (ON/OFF USER CONTROL TBD?)
	Boolean	isTrackingEnabled() { return _trackingEnabled; }

	// START TRACKING (CALLED BY USER CONTROL)
	void stopTracking() { _trackingEnabled = False; updateViews(); }

	// STOP TRACKING (CALLED BY USER CONTROL )
	void startTracking() { _trackingEnabled = True; updateViews(); }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CursorPositionView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CursorDisplayer.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
//  CursorDisplayer.h:
//
//	Instantiates all cursor views, model and maybe 
//	someday a controller.   
//
///////////////////////////////////////////////////////////////////
#ifndef CURSORDISPLAYER_H
#define CURSORDISPLAYER_H
#include "UIComponent.h"

class CursorDnView;
class CursorPositionView;

class CursorLatLonView;

class CursorModel;
class ImageData;
class BasicImageView;

class CursorDisplayer : public UIComponent {


private:

	CursorDnView *		_cursorDnView;     // to display DN values
	CursorPositionView *	_cursorPositionView;  //  x-y cursor coords

        CursorLatLonView *      _cursorLatLonView;

	CursorModel *		_cursorModel;	      // model
	
protected:

        	static String _defaults[]; // resource defaults.

public:

	CursorDisplayer( Widget parent, char *name,  ImageData *dataModel,
			 BasicImageView *imageView  );
	virtual ~CursorDisplayer();

	virtual const char * const className() { return ("cursorDisplayer"); } 

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageToCursorDnViewGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ImageToCursorDnViewGlue: class that serves as a "glue" class between
// an ImageData object and a CursorDnView, so that the CursorDnView
// can adjust its # of DN boxes when the mode changes from color to bw or
// back, or their width when the data type changes.  The class is a View to
// ImageData, so whenever it receives an update() from ImageData, it
// notifies the CursorDnView.  This class, even though it's a UIComponent,
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef IMAGETOCURSORDNVIEWGLUE_H
#define IMAGETOCURSORDNVIEWGLUE_H
#include "BasicImageView.h"
#include "ImageData.h"
#include "CursorDnView.h"

class ImageToCursorDnViewGlue : public BasicImageView {

 protected:
   ModeType _oldMode;
   ImagePixelType _oldType;
   CursorDnView *_cursorDnView;

 public:

   ImageToCursorDnViewGlue (ImageData *model, CursorDnView *cursorDnView)
		: BasicImageView("glue", model)
	{  _cursorDnView = cursorDnView;
	   _oldMode = _model->getMode();
	   _model->attachView(this);
	}

   virtual void update()	// the whole reason for the class existing
	{  if (_oldMode != _model->getMode() ||
		_oldType != _model->getPixelType()) {
	      // We're really like to _cursorDnView->unmanage() here and
	      // then manage again after creating the displays in order to
	      // minimize flashing as each view is destroyed and re-created.
	      // Unfortunately, while this works on Solaris, it doesn't work
	      // on any other platform - the unmanaged RowColumn won't
	      // resize when it is managed again.  Sigh.
	      _oldMode = _model->getMode();
	      _oldType = _model->getPixelType();
	      _cursorDnView->removeCursorDisplays();
	      _cursorDnView->createCursorDisplays();
	   }
	}

   virtual Widget getWidget() { return NULL; }

   virtual const char *const className() { return  "ImageToCursorGlue"; }

};
#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
