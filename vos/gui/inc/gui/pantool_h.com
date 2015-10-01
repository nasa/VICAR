$!****************************************************************************
$!
$! Build proc for MIPL module pantool_h
$! VPACK Version 1.8, Monday, April 07, 1997, 19:00:37
$!
$! Execute by entering:		$ @pantool_h
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
$ write sys$output "*** module pantool_h ***"
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
$ write sys$output "Invalid argument given to pantool_h.com file -- ", primary
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
$   if F$SEARCH("pantool_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @pantool_h.bld "STD"
$   else
$      @pantool_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pantool_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pantool_h.com -mixed -
	-s PanTool.h PanToolWindow.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create PanTool.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// Compenent that implements a pan tool.  This tool is intended to control
// another image widget display.  It presents a small zoom-to-fit version
// of the same image, with a box in the overlay representing the area
// currently being displayed in the other window.  Various actions allow
// the user to move the box in this display, which changes the pan on the
// other widget.  Likewise, other methods of changing the pan on the main
// widget will cause the box to move.
////////////////////////////////////////////////////////////////////////
#ifndef PANTOOL_H
#define PANTOOL_H

#include "ImageDisplayView.h"
#include "XvicImage.h"

class Lut;
class LutToImageWidgetGlue;

#define XvicNpanBoxColor	"panBoxColor"
#define XvicCPanBoxColor	"PanBoxColor"
#define XvicNapplyStretchToPan	"applyStretchToPan"
#define XvicCApplyStretchToPan	"ApplyStretchToPan"

class PanTool : public ImageDisplayView {
 private:
   static void bigWidgetChangeCallback(Widget, XtPointer, XtPointer);
   static void inputCallback(Widget, XtPointer, XtPointer);

   static XtResource _resources[];

   Dimension computeSizeX(int, int, Boolean, Widget);
   Dimension computeSizeY(int, int, Boolean, Widget);

 protected:
   Widget _big_iw;
   XvicID _box_id;
   String _box_color_string;	// From the resource
   XvicColor _box_color;
   XvicGC _box_gc;
   int _box_x, _box_y;		// Image coords of upper left
   int _input_x, _input_y;	// Saved coords for mouse-panning
   Boolean _preserve_aspect;	// True if aspect ratio of shell should be set
   Boolean _apply_stretch_to_pan;	// From the resource

   Lut *_rlut, *_glut, *_blut;
   Lut *_rplut, *_gplut, *_bplut;
   LutToImageWidgetGlue *_lutGlue, *_pseudoLutGlue;

   void bigWidgetChange(XtPointer);
   void input(XtPointer);

   void drawNewBox();
   void moveBox();

   void setAspectRatio();
   void copyDisplayModeResources();
   void copyDataRangeResources();

   void newSize();

 public:

   PanTool(Widget parent, const char *name, ImageData *model, Widget _big_iw,
		Dimension view_height, Dimension view_width,
		Boolean preserve_aspect = True,
		Lut *rlut=NULL, Lut *glut=NULL, Lut *blut=NULL,
		Lut *rplut=NULL, Lut *gplut=NULL, Lut *bplut=NULL);
   ~PanTool();

};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PanToolWindow.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// Compenent that creates a popup window containing a pan tool.
////////////////////////////////////////////////////////////////////////
#ifndef PANTOOLWINDOW_H
#define PANTOOLWINDOW_H

#include "MainWindow.h"
class PanTool;
class ImageData;
class Lut;

#define XvicNpanDesiredSize	"panDesiredSize"
#define XvicCpanDesiredSize	"PanDesiredSize"

class PanToolWindow : public MainWindow {

 private:
   static XtResource _resources[];

 protected:

   Widget _form;
   PanTool *_panTool;
   ImageData *_model;
   Widget _big_iw;

   int _pan_desired_size;		// from the resource
   Lut *_rlut, *_glut, *_blut;
   Lut *_rplut, *_gplut, *_bplut;

   virtual Widget createWorkArea(Widget);

 public:

   PanToolWindow(const char *name, ImageData *model, Widget big_iw,
	Lut *rlut=NULL, Lut *glut=NULL, Lut *blut=NULL,
	Lut *rplut=NULL, Lut *gplut=NULL, Lut *bplut=NULL);

};

#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
