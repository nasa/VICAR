$!****************************************************************************
$!
$! Build proc for MIPL module xsnapwidget_h
$! VPACK Version 1.8, Thursday, January 16, 1997, 11:51:34
$!
$! Execute by entering:		$ @xsnapwidget_h
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
$ write sys$output "*** module xsnapwidget_h ***"
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
$ write sys$output "Invalid argument given to xsnapwidget_h.com file -- ", primary
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
$   if F$SEARCH("xsnapwidget_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @xsnapwidget_h.bld "STD"
$   else
$      @xsnapwidget_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xsnapwidget_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xsnapwidget_h.com -mixed -
	-s XImageHandler.h XImageToVicar.h WidgetSnapshot.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create XImageHandler.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// XImageHandler - class that handles access to an XImage structure.
// Currently only returns image data from the struct.  Future enhancement
// would be to set data into the struct as well.
// This class can also hold "children" XImage's.  Child XImage's cover a
// certain defined portion of the XImage, and when that portion is requested,
// the child's data is returned instead (this of course may be recursive).
// This is useful for simulating a widget hierarchy using off-screen
// pixmaps, for example.
////////////////////////////////////////////////////////////////////////

#ifndef XIMAGEHANDLER_H
#define XIMAGEHANDLER_H
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>

class XImageHandler {

 private:

 protected:

   XImage *_ximage;
   Colormap _cmap;
   Visual *_visual;

   unsigned long _red_mask;
   unsigned long _green_mask;
   unsigned long _blue_mask;
   int _red_shift;
   int _green_shift;
   int _blue_shift;
   int _visual_class;
   int _cmap_size;

   XColor *_colors;

   int _num_children;
   XImageHandler **_children;
   int *_child_x;
   int *_child_y;

 public:
   XImageHandler(XImage *);
   virtual ~XImageHandler();		// NOTE: DOES NOT free _ximage

   void setWidget(Widget);		// calls setColormap() and setVisual()
   void setVisual(Display *, Visual *);
   void setColormap(Display *, Colormap);

   int width() { return _ximage->width; }
   int height() { return _ximage->height; }

   void printHeader();

   void getLine(unsigned long *pixels, int y, int x, int width);

   void getColorLine(unsigned char *r, unsigned char *g, unsigned char *b,
		int y, int x, int width);

   void addChild(XImageHandler *child, int x, int y);
   void removeChild(XImageHandler *child);

   void freeChildTree();	// free children and all XImage's
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XImageToVicar.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// XImageToVicar - function (not class) which uses XImageHandler to write
// to a VICAR file.  This function is in a separate module to isolate
// VICAR dependencies.
////////////////////////////////////////////////////////////////////////

#ifndef _XIMAGETOVICAR_H
class XImageHandler;

void writeXImageToVicarFile(XImageHandler *img,
		char *red_file, char *green_file, char *blue_file,
		int x, int y, int w, int h);

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create WidgetSnapshot.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// WidgetSnapshot - function that will take a "snapshot" of the visual
// appearance of a widget, returning an XImageHandler object from  which
// the image can be extracted.
// This function is unique in that the widget need not be displayed on the
// screen (although it must be managed and realized, it doesn't have to
// be mapped).  It works by temporarily replacing the widget's window with
// an off-screen pixmap and then forcing an expose.
//
// *** This uses all sorts of illegal accesses to internal Xt data ***
// *** structures, and is NOT guaranteed to work with all widgets! ***
//
// A complication here is that off-screen pixmaps have no window hierarchy,
// so we must traverse the widget tree ourselves and build them up.
// NOTE:  Background pixmaps in the widget are NOT supported at this time.
// NOTE:  Overlapping widget children at the same hierarchy level are not
// handled well.
//
// A special function may be provided to render widgets that do not work
// with this scheme; see the doc file.
//
// The freeWidgetSnapshot() function will free all data structures alloceted
// by takeWidgetSnapshot.
////////////////////////////////////////////////////////////////////////

#ifndef _WIDGET_SNAPSHOT_H
#define _WIDGET_SNAPSHOT_H

#include <X11/Intrinsic.h>

class XImageHandler;

typedef XImageHandler *widgetSnapSpecialFunction(Widget w);
XImageHandler *takeWidgetSnapshot(Widget w,
				widgetSnapSpecialFunction *fn = NULL);
void freeWidgetSnapshot(XImageHandler *h);

#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
