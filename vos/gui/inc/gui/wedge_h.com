$!****************************************************************************
$!
$! Build proc for MIPL module wedge_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:55:44
$!
$! Execute by entering:		$ @wedge_h
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
$ write sys$output "*** module wedge_h ***"
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
$ write sys$output "Invalid argument given to wedge_h.com file -- ", primary
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
$   if F$SEARCH("wedge_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @wedge_h.bld "STD"
$   else
$      @wedge_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create wedge_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack wedge_h.com -mixed -
	-s BasicWedgeView.h WedgeResourceTypes.h WedgeView.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create BasicWedgeView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// BasicWedgeView.h:
///////////////////////////////////////////////////////////////////
#ifndef BASICWEDGEVIEW_H
#define BASICWEDGEVIEW_H
#include "UIComponent.h"
#include "XvicImage.h"
#include <stdio.h>
#include <iostream>

class BasicWedgeView : public UIComponent {

  private:

  protected:

  public:

	BasicWedgeView ( const char * name ) : UIComponent (name)  {}

	virtual const char *const className() { return  "BasicWedgeView"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create WedgeResourceTypes.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ImageResourceTypes.h
////////////////////////////////////////////////////////////////

#define XvicNnsteps		"nsteps"
#define XvicCNsteps		"Nsteps"
#define XvicNminPixelDN		"minPixelDN"
#define XvicCMinPixelDN    		"MinPixelDN"
#define XvicNmaxPixelDN		"maxPixelDN"
#define XvicCMaxPixelDN    	"MaxPixelDN"
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create WedgeView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// WedgeView.h
////////////////////////////////////////////////////////////////
#ifndef WEDGEVIEW_H
#define WEDGEVIEW_H
#include "BasicWedgeView.h"	// the base class for wedge views
#include "GuiDefs.h"
#include "XvicImage.h"

#ifndef MIN
#define MIN(x,y)	(((x) < (y)) ? (x) : (y))
#endif

class WedgeView : public BasicWedgeView {

  private:

	static XtResource _resources[];

	static  void exposeCallback ( Widget , 
				      XtPointer client_data, 
				      XtPointer call_data );
	static  void resizeCallback ( Widget , 
				      XtPointer client_data, 
				      XtPointer call_data );

  protected:

	Widget		_iw; 			 // Image widget, child of Scrolled Window
	int		_buffer_startLine;		// startLine number from most recent read;
	int		_buffer_startSample;	// startSample number from most recent read;
	OrientType	_orientation;
	int		_minPixelDN;
	int		_maxPixelDN;
	int		_numberOfSteps;
	XvicImageData 	_img;
	

	virtual void createWidget(Widget parent, int _view_height, int _view_width );
	virtual void resize( XvicImageCallbackStruct *  cb);
	virtual void expose( XvicImageCallbackStruct * cb);
	virtual void createBuffer();
	virtual void setWidgetStruct(XvicImageCallbackStruct * cb);

  public:

	WedgeView(Widget parent, const char * name,
				int view_height = 0, int view_width = 0 );
	~WedgeView() {XtDestroyWidget(_iw);  if (_img.bw_pixels) delete _img.bw_pixels;};

	Widget getWidget() { return _iw; };
	XvicImageData getImageData() { return _img; };

	virtual void setOrientation( OrientType  o = HORIZONTAL ) { _orientation = o; }
	virtual OrientType  getOrientation() { return _orientation; }

	virtual void setMinPixelValue( int min ) { _minPixelDN = min; }
	virtual int  getMinPixelValue() { return _minPixelDN; }

	virtual void setMaxPixelValue( int max ) { _maxPixelDN = max;  }
	virtual int  getMaxPixelValue() { return _maxPixelDN; }

	virtual void setNumberOfSteps( int nsteps ) { _numberOfSteps = nsteps; }
	virtual int  getNumberOfSteps() { return _numberOfSteps; }

	virtual const char *const className() { return ("WedgeView"); }
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
