$!****************************************************************************
$!
$! Build proc for MIPL module latlon_h
$! VPACK Version 1.9, Monday, January 11, 1999, 13:48:16
$!
$! Execute by entering:		$ @latlon_h
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
$ write sys$output "*** module latlon_h ***"
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
$ write sys$output "Invalid argument given to latlon_h.com file -- ", primary
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
$   if F$SEARCH("latlon_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @latlon_h.bld "STD"
$   else
$      @latlon_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create latlon_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack latlon_h.com -mixed -
	-s CursorLatLonView.h LatLonBarCmd.h LatLonBar.h SetLatLonTypeCmd.h -
	   ImageToImageWindowGlue.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create CursorLatLonView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// CursorLatLonView.h
//
//	View for displaying the latitude and longitude 
//      corresponding to the (x,y) coordinates of the cursor
//
//	Subclass of CursorBasicView.  
//
///////////////////////////////////////////////////////////////
#ifndef CURSORLATLONVIEW_H
#define CURSORLATLONVIEW_H

#include "CursorBasicView.h"
#include <Xm/Xm.h>
#include "CursorModel.h"

class ImageData;

//enum LongitudeDirection { UNDEFdirection, EAST, WEST };

enum LatLonType   { CENTRIC, DETIC };

class CursorLatLonView : public CursorBasicView {

    protected:

        Widget _latLonForm;
	
	Widget _lonDirectionLabel;

	Widget _selectionWidget;

	// VARIABLES:  Latitude (line) display
	Widget		_labelLat, _textfieldLat, _formLat;
	Boolean		_latEnable;	// on-off 	

	// VARIABLES:  Longitude (samp) display
	Widget		_labelLon, _textfieldLon, _formLon;
	Boolean		_lonEnable;	// on-off 

	virtual void addNewSubView(Widget   /* w */ ,
				   char *   /* displayName */ , 
				   Widget * /* form */ ,
				   Widget * /* label */ ,
				   Widget * /* textfield */ , 
				   Boolean  /* enabled */ ) { }
			
	LatLonType _latLonType;
	LongitudeDirection _lonDir;

	Widget _latLonTypeWidget;  // the "Projection Type" button widget
	
    public:

	CursorLatLonView(Widget, const char *, CursorModel *, ImageData *,
			 unsigned char);
	virtual ~CursorLatLonView() ;

	// CREATE DISPLAYS lat/lon values
	virtual void    createCursorDisplays();
	virtual void    removeCursorDisplays();

	// UPDATE DISPLAYS WHEN CURSOR MOVES
	virtual void cursorMoved(XvicImageCallbackStruct * cb );
	virtual void cursorMoved(int x, int y);

	// BLANK OUT ALL VIEWS WHEN CURSOR IS OUT OF RANGE
	virtual void blankSubViews();

	virtual LatLonType getLatLonType() { return _latLonType; }
	virtual void setLatLonType(LatLonType n) { _latLonType = n; }
	
	virtual const char *const className() { return "CursorLatLonView" ; } 

	virtual void start() { startTracking(); }

	virtual void stop() { stopTracking(); }

	virtual void update();
	
	// call this fcn with False to "grey out" (deactivate) the interface:
	virtual void enableLatLonSelect(Boolean val) 
	     { if (_latLonTypeWidget != NULL)
	       XtSetSensitive(_latLonTypeWidget, val); }
	     
	//virtual void updateLonDirection(LongitudeDirection); 
	virtual void updateLonDirection();
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LatLonBarCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// LatLonBarCmd.h: (un)display the Lat/Lon Bar
/////////////////////////////////////////////////////////////
#ifndef LATLONBARCMD_H
#define LATLONBARCMD_H
#include "Cmd.h"

class ImageDisplayer;

class LatLonBarCmd : public Cmd {
  private:
    int _oldValue;    // for nostalgic purposes, we keep the old value
    ImageDisplayer *_imageView;

  protected:

    virtual void doit();
    virtual void undoit();

  public:

    LatLonBarCmd( const char *, int, ImageDisplayer * );
    virtual const char *const className () { return "LatLonBarCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LatLonBar.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// LatLonBar.h:
/////////////////////////////////////////////////////////////
#ifndef LATLONBAR_H
#define LATLONBAR_H
#include "UIComponent.h"

//#include "CursorLatLonView.h"
class CursorLatLonView;

class Cmd;
class ImageData;
class BasicImageView;

class LatLonBar : public UIComponent {

  protected:

    CursorLatLonView *_cursorLatLonView;

  public:

    LatLonBar ( Widget, const char *, BasicImageView *, ImageData *);
    virtual CursorLatLonView *getCursorLatLonView(){return _cursorLatLonView; }

    virtual const char *const className() { return "LatLonBar"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetLatLonTypeCmd.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SetLatLonTypeCmd.h: sets the method of calculating 
//  latitude and longitude; can be either planetocentric or
//  planetodetic
/////////////////////////////////////////////////////////////
#ifndef SETLATLONTYPECMD_H
#define SETLATLONTYPECMD_H
//#include "NoUndoCmd.h"

#include "RadioCmd.h"

//#include "ImageDisplayer.h"
#include "LatLonBar.h"
#include "CursorLatLonView.h"

class SetLatLonTypeCmd : public RadioCmd {

  protected:
  
    // int _oldValue;
    
    CursorLatLonView *_cursorView;
    LatLonType _latLonType;
    
    virtual void doit();   

  public:
    
    SetLatLonTypeCmd ( const char *, int, CursorLatLonView *, LatLonType,
		       CmdList * );

    virtual const char *const className () { return "SetLatLonTypeCmd"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageToImageWindowGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ImageToImageWindowGlue: class that serves as a "glue" class between an
// ImageData object and an ImageWindow object. The class is a
// View to ImageData, so that the ImageWindow can take appropriate
// action whenever something changes w.r.t. the image.
//
// This class, though a UIComponent, creates no widget, and therefore 
// should never be managed.
////////////////////////////////////////////////////////////////////////

#ifndef IMAGETOIMAGEWINDOWGLUE_H
#define IMAGETOIMAGEWINDOWGLUE_H
#include "BasicImageView.h"

class ImageData;
class ImageWindow;

class ImageToImageWindowGlue : public BasicImageView {

 private: 

 protected:
  
  ImageWindow *_window;

//  void *_collectionActive;
 
 public:

   ImageToImageWindowGlue (ImageData *model, ImageWindow *window);
   virtual ~ImageToImageWindowGlue ();

   virtual void update();	// the whole reason for the class existing
   virtual void updatePart(int flags);

   virtual const char *const className() { return  "ImageToImageWindowGlue"; }

};
#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
