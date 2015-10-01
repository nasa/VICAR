$!****************************************************************************
$!
$! Build proc for MIPL module imageview_h
$! VPACK Version 1.9, Friday, December 19, 1997, 17:43:24
$!
$! Execute by entering:		$ @imageview_h
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
$ write sys$output "*** module imageview_h ***"
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
$ write sys$output "Invalid argument given to imageview_h.com file -- ", primary
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
$   if F$SEARCH("imageview_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @imageview_h.bld "STD"
$   else
$      @imageview_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create imageview_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack imageview_h.com -mixed -
	-s BasicImageView.h ImageDisplayView.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create BasicImageView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// BasicImageView.h:
//	An abstract class for views with images to be displayed.
////////////////////////////////////////////////////////////////
#ifndef BASICIMAGEVIEW_H
#define BASICIMAGEVIEW_H
#include "UIComponent.h"
#include "ZoomFactor.h"
#include <Xm/Xm.h>

class ImageData;

class BasicImageView : public UIComponent {

 protected:

   ImageData *_model;	

   ZoomFactor _userZoom;
   ZoomFactor _imageZoom;
   ZoomFactor _preZoom;
   Boolean _zoom2Fit;

   // Calculate zoom factor needed for image to fit window
   virtual void calcZoomToFit(Widget iw, int *in, int *out);

 public:

   BasicImageView(const char *name, ImageData *model) : UIComponent(name) 
	{  _model = model;  }

   // Views that care about updatePart() should override to not call update().
   virtual void update() = 0;
   virtual void updatePart(int /*flags*/) { update(); }

   // Get image widget ID - should be overridden by subclass
   virtual Widget getWidget() { return NULL; }

   // Get object to fill in user zoom
   virtual ZoomFactor &getUserZoom() { return _userZoom; }

   // Is image set at zoom to fit?
   virtual Boolean isZoom2Fit() { return _zoom2Fit; }

   // Set user zoom factor on image - should be overridden by subclass
   virtual void setUserZoom(ZoomFactor &) { }

   // Set user zoom-to-fit on image - should be overridden by subclass
   virtual void setUserZoom2Fit() { }

   // Get current zoom - these should be overridden by subclasses
   virtual ZoomFactor & getImageZoom() { return _imageZoom; }
   virtual ZoomFactor & getPreZoom() { return _preZoom; }

   // Class name
   virtual const char *const className() { return  "BasicImageView"; }

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageDisplayView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ImageDisplayView.h
//
//	This view works with ImageModel (model) for displaying an
//	image on screen.  This is a subclass of BasicImageView.
//
//	Notes:  XvicCreateImage sets default resources.
////////////////////////////////////////////////////////////////
#ifndef IMAGEDISPLAYVIEW_H
#define IMAGEDISPLAYVIEW_H
#include "BasicImageView.h"

class ImageTile;

class ImageDisplayView : public BasicImageView {

 private:

   static void exposeCallback(Widget, XtPointer client_data, XtPointer call_data);
   static void resizeCallback(Widget, XtPointer client_data, XtPointer call_data);

 protected:

   Widget _iw; 			// Image widget, child of Scrolled Window
   int _buffer_startLine;	// startLine number from most recent read
   int _buffer_startSample;	// startSample number from most recent read
   int _bufferHeight;		// height of buffer from most recent read
   int _bufferWidth;		// width of buffer from most recent read
   unsigned char _SaveScrollBarDisplayPolicyDuringZoom2Fit;

   ImageTile *_tilePtr;

   // Create an image widget.  Called only from constructor
   virtual void createImageWidget(Widget parent,
			Dimension view_height, Dimension view_width);

   // Init widget info
   virtual void initWidget();		

   // Resize image.  Called from callback and zoom-to-fit routine
   virtual void resizeImage();

   // Expose image.  Called from callback when widget is loading data
   virtual void exposeImage(XtPointer call_data);

 public:

   ImageDisplayView(Widget parent, const char *name, ImageData *model,
				Dimension view_height, Dimension  view_width);
   virtual ~ImageDisplayView();

   // Set window title to filename(s)
   virtual void setTitle(char *title);

   // Update called by model.
   virtual void update();
   virtual void updatePart(int flags);

   // Get ID of actual image widget.  baseWidget() returns the scrolled window
   // parent.
   virtual Widget getWidget() { return _iw; }

   // Set user zoom is called when user selects a new zoom on image
   virtual void setUserZoom(ZoomFactor &zoom);

   // Get image zoom returns the current zoom on the image
   virtual ZoomFactor &getImageZoom();
   virtual ZoomFactor &getPreZoom();

   // Set zoom to fit exactly within the window on screen.
   virtual void setUserZoom2Fit();

   // Get and set the size of the view
   virtual void setViewSize(Dimension w, Dimension h, Boolean scrollbars);
   virtual void getViewSize(Dimension &w, Dimension &h);

   // Standard classname
   virtual const char *const className() { return "ImageView"; }

};
#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
