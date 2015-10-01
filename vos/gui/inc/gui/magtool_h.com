$!****************************************************************************
$!
$! Build proc for MIPL module magtool_h
$! VPACK Version 1.8, Thursday, January 16, 1997, 14:49:23
$!
$! Execute by entering:		$ @magtool_h
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
$ write sys$output "*** module magtool_h ***"
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
$ write sys$output "Invalid argument given to magtool_h.com file -- ", primary
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
$   if F$SEARCH("magtool_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @magtool_h.bld "STD"
$   else
$      @magtool_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create magtool_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack magtool_h.com -mixed -
	-s MagTool.h MagInfo.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create MagTool.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// Component that implements a magnifying glass tool.  It presents a
// small zoomed-out portion of the same image.  The window has to be
// positioned in the way that the cursor is always in the middle of the
// magnifying glass.  Moving the cursor within the main image will change
// what's displayed inside the magnifying glass.  Moving the cursor outside
// of the main image removes the mag glass window.
////////////////////////////////////////////////////////////////////////

#ifndef MAGTOOL_H
#define MAGTOOL_H

#include "ImageDisplayView.h"
#include "XvicImage.h"

class CursorPositionView;
class CursorDnView;
class MagInfo;
class Lut;
class LutToImageWidgetGlue;

class MagTool : public ImageDisplayView {

 private:

   static void bigWidgetChangeCallback(Widget, XtPointer, XtPointer);
   static void cursorCallback(Widget, XtPointer, XtPointer);
   static void inputCallback(Widget, XtPointer, XtPointer);
   static void motionHandler(Widget, XtPointer, XEvent *, Boolean *);

 protected:

   Widget _big_iw;
   int _width, _height;
   int _init_xin, _init_xout, _init_yin, _init_yout;
   CursorPositionView *_posView;
   CursorDnView *_dnView;
   MagInfo *_magInfo;

   LutToImageWidgetGlue *_lTIWG, *_pseudoLTIWG;

   void setInitZoom();
   void bigWidgetChange(XtPointer);
   void cursor(XtPointer);
   void motion(XMotionEvent *);
   void input(XtPointer);

   void copyDisplayModeResources();
   void copyDataRangeResources();
   Boolean isOnImage(XvicImageCallbackStruct * cb);
   Boolean isOnImage(int x, int y);

 public:

   MagTool(Widget parent, const char *name, ImageData *model, Widget _big_iw,
		Lut *rlut, Lut *glut, Lut *blut,
		Lut *rpseudo, Lut *gpseudo, Lut *bpseudo,
		MagInfo *magInfo=NULL,
		CursorPositionView * = NULL, CursorDnView * = NULL,
		Dimension view_height=100, Dimension view_width=100);
   virtual ~MagTool();

   virtual void manage();

   void setSize(Dimension width, Dimension height);

   virtual const char *const className () { return "MagTool"; }

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create MagInfo.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// MagInfo.h: Displays mag text information on magnification rate 
// and mag size.
///////////////////////////////////////////////////////////////
#ifndef MAGINFO_H
#define MAGINFO_H
#include "UIComponent.h"

class KeyinView;

class MagInfo : public UIComponent {

    protected:

	static String _defaults[];

	KeyinView *_magSize, *_magRatio;

    public:

	MagInfo(Widget parent, const char *name);
	virtual ~MagInfo();

	void printSize(int w, int h);
	void printRatio(float ratioX, float ratioY);

	virtual const char *const className() { return "MagInfo"; } 
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
