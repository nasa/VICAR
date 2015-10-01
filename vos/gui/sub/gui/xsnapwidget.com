$!****************************************************************************
$!
$! Build proc for MIPL module xsnapwidget
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:43
$!
$! Execute by entering:		$ @xsnapwidget
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
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module xsnapwidget ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Create_Other .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to xsnapwidget.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$   if F$SEARCH("xsnapwidget.imake") .nes. ""
$   then
$      vimake xsnapwidget
$      purge xsnapwidget.bld
$   else
$      if F$SEARCH("xsnapwidget.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xsnapwidget
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xsnapwidget.bld "STD"
$   else
$      @xsnapwidget.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xsnapwidget.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xsnapwidget.com -mixed -
	-s XImageHandler.cc XImageToVicar.cc WidgetSnapshot.cc -
	-i xsnapwidget.imake -
	-o xsnapwidget.doc
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create XImageHandler.cc
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

#include "XImageHandler.h"
#include <stdio.h>
#include <X11/Xutil.h>
#include <X11/StringDefs.h>

#ifndef MAX
#define MAX(a,b) ((a)>(b)?(a):(b))
#endif
#ifndef MIN
#define MIN(a,b) ((a)<(b)?(a):(b))
#endif

////////////////////////////////////////////////////////////////////////

XImageHandler::XImageHandler(XImage *ximage)
{
   _ximage = ximage;
   _cmap = None;
   _visual = NULL;

   _red_mask = 0;
   _green_mask = 0;
   _blue_mask = 0;
   _visual_class = PseudoColor;
   _cmap_size = 256;

   _colors = NULL;

   _num_children = 0;
   _children = NULL;
   _child_x = NULL;
   _child_y = NULL;
}

////////////////////////////////////////////////////////////////////////

XImageHandler::~XImageHandler()
{
   if (_colors)
      delete _colors;

   if (_children)
      delete _children;
   if (_child_x)
      delete _child_x;
   if (_child_y)
      delete _child_y;
}

////////////////////////////////////////////////////////////////////////

void XImageHandler::addChild(XImageHandler *child, int x, int y)
{
   XImageHandler **new_list;
   int *new_x, *new_y;

   // Create new list
   new_list = new XImageHandler *[_num_children+1];
   new_x = new int [_num_children+1];
   new_y = new int [_num_children+1];

   // Copy old elements over
   for (int i=0; i<_num_children; i++) {
      new_list[i] = _children[i];
      new_x[i] = _child_x[i];
      new_y[i] = _child_y[i];
   }

   // Delete old, and assign to new
   delete []_children;
   delete []_child_x;
   delete []_child_y;
   _children = new_list;
   _child_x = new_x;
   _child_y = new_y;

   // Finally, add the new one
   _children[_num_children] = child;
   _child_x[_num_children] = x;
   _child_y[_num_children] = y;

   _num_children++;
}

////////////////////////////////////////////////////////////////////////

void XImageHandler::removeChild(XImageHandler *child)
{
   for (int i=0; i<_num_children; i++) {
      if (_children[i] == child) {		// found the one to delete
         for (int j=i; j<_num_children-1; j++) {
            _children[j] = _children[j+1];
            _child_x[j] = _child_x[j+1];
            _child_y[j] = _child_y[j+1];
         }
         _num_children--;	// Don't bother to realloc; this is rare anyway
         i--;			// Repeat the check for this child
      }
   }
}


////////////////////////////////////////////////////////////////////////
// Set the colormap and visual based on the given widget.
////////////////////////////////////////////////////////////////////////

void XImageHandler::setWidget(Widget w)
{
   Visual *v;
   if (!XtIsRealized(w))
      v = NULL;
   else {
      XWindowAttributes attr;
      XGetWindowAttributes(XtDisplay(w), XtWindow(w), &attr);
      v = attr.visual;
   }
   setVisual(XtDisplay(w), v);

   Colormap c;
   XtVaGetValues(w, XtNcolormap, &c, NULL);
   setColormap(XtDisplay(w), c);
}

////////////////////////////////////////////////////////////////////////
// Set the Visual * which matches the XImage.  The visual is used only
// by getColorLine() (and setColormap()) and need not be set if these
// routines are not used.  It is needed for two things:  to get the visual
// class (PseudoColor, TrueColor, etc.) so we know how to interpret the
// pixels, and to get the rgb masks (for TrueColor/DirectColor), which should
// be in the XImage structure but don't seem to be set.
////////////////////////////////////////////////////////////////////////

void XImageHandler::setVisual(Display *dpy, Visual *v)
{
   XVisualInfo vinfo_mask, *vinfo;
   int n;
   unsigned long mask;

   vinfo_mask.visualid = XVisualIDFromVisual(v);
   vinfo = XGetVisualInfo(dpy, VisualIDMask, &vinfo_mask, &n);

   // Assume n=1; don't know what to do if it's not!
   _red_mask = vinfo->red_mask;
   _green_mask = vinfo->green_mask;
   _blue_mask = vinfo->blue_mask;
   _visual_class = vinfo->c_class;		// C++ member
   _cmap_size = vinfo->colormap_size;

   _red_shift = 0;
   mask = _red_mask;
   if (mask) {
      while ((mask&1) == 0) {
         _red_shift++;
         mask >>= 1;
      }
   }
   _green_shift = 0;
   mask = _green_mask;
   if (mask) {
      while ((mask&1) == 0) {
         _green_shift++;
         mask >>= 1;
      }
   }
   _blue_shift = 0;
   mask = _blue_mask;
   if (mask) {
      while ((mask&1) == 0) {
         _blue_shift++;
         mask >>= 1;
      }
   }

   XFree(vinfo);
}

////////////////////////////////////////////////////////////////////////
// Set the colormap which matches the XImage.  The colormap is used only
// by getColorLine() and need not be set if this routine is not used.
////////////////////////////////////////////////////////////////////////

void XImageHandler::setColormap(Display *dpy, Colormap c)
{
   int i;

   if (_colors)
      delete _colors;

   _colors = new XColor[_cmap_size];

   _cmap = c;
   for (i=0; i<_cmap_size; i++) {
      if (_visual_class == DirectColor)
         _colors[i].pixel = (i<<_red_shift)|(i<<_green_shift)|(i<<_blue_shift);
      else
         _colors[i].pixel = i;
   }

   XQueryColors(dpy, _cmap, _colors, _cmap_size);

}

////////////////////////////////////////////////////////////////////////
// Return the given part of a line from the XImage struct into the
// unsigned long buffer provided (which must be big enough!).
// The data is returned in the format of XGetPixel (as a "normalized"
// long).
////////////////////////////////////////////////////////////////////////

void XImageHandler::getLine(unsigned long *pixels, int y, int x, int width)
{
   int i;
   // Put in primary data

   for (i=0; i<width; i++)
      pixels[i] = XGetPixel(_ximage, x+i, y);

   // Overlay any children ... maintain in parallel with getColorLine

   for (i=0; i<_num_children; i++) {
      if (_child_y[i] <= y && (_child_y[i] + _children[i]->height() > y)) {

         // In the child covers the area vertically, try horizontal

         if (_child_x[i]<(x+width) && (_child_x[i]+_children[i]->width())>=x) {

            // Yep, use it

            int x1 = MAX(x, _child_x[i]);
            int x2 = MIN(x+width, _child_x[i]+_children[i]->width());
            _children[i]->getLine(pixels + (x1-x), y-_child_y[i],
			x1-_child_x[i], x2-x1);
         }
      }
   }
}

////////////////////////////////////////////////////////////////////////
// Return the given part of a line from the XImage struct into the
// color buffers provided (which must be big enough!).  The data is converted
// to color by running it through the colormap LUT.
////////////////////////////////////////////////////////////////////////

void XImageHandler::getColorLine(
		unsigned char *r, unsigned char *g, unsigned char *b,
		int y, int x, int width)
{
   int i;
   unsigned long *buf = new unsigned long[width];

   getLine(buf, y, x, width);

   switch (_visual_class) {

      case StaticGray:
      case GrayScale:
      case StaticColor:
      case PseudoColor:
         if (_colors)
            for (i=0; i<width; i++) {
               r[i] = _colors[buf[i]].red;
               g[i] = _colors[buf[i]].green;
               b[i] = _colors[buf[i]].blue;
            }
         else
            for (i=0; i<width; i++)
               r[i] = g[i] = b[i] = 0;
         break;

      case TrueColor:
         for (i=0; i<width; i++) {
            r[i] = (unsigned char) (buf[i] & _red_mask) >> _red_shift;
            g[i] = (unsigned char) (buf[i] & _green_mask) >> _green_shift;
            b[i] = (unsigned char) (buf[i] & _blue_mask) >> _blue_shift;
         }
         break;

      case DirectColor:
         if (_colors)
            for (i=0; i<width; i++) {
               r[i] = _colors[(buf[i] & _red_mask) >> _red_shift].red;
               g[i] = _colors[(buf[i] & _green_mask) >> _green_shift].green;
               b[i] = _colors[(buf[i] & _blue_mask) >> _blue_shift].blue;
            }
         else
            for (i=0; i<width; i++)
               r[i] = g[i] = b[i] = 0;
         break;
   }
   delete buf;

   // Overlay any children ... maintain in parallel with getColorLine
   // This unfortunately cannot be done as part of the readLine() above
   // in all cases because the colormap or visual type might be different.

   for (i=0; i<_num_children; i++) {
      if (_child_y[i] <= y && (_child_y[i] + _children[i]->height() > y)) {

         // In the child covers the area vertically, try horizontal

         if (_child_x[i]<(x+width) && (_child_x[i]+_children[i]->width())>=x) {

            // Yep, use it

            int x1 = MAX(x, _child_x[i]);
            int x2 = MIN(x+width, _child_x[i]+_children[i]->width());
            _children[i]->getColorLine(r + (x1-x), g + (x1-x), b + (x1-x),
			y-_child_y[i], x1-_child_x[i], x2-x1);
         }
      }
   }
}

////////////////////////////////////////////////////////////////////////
// Recursively free all children, and their assocated XImage structures.
// Note: destructor does not free XImage's.
////////////////////////////////////////////////////////////////////////

void XImageHandler::freeChildTree()
{
   for (int i=0; i<_num_children; i++) {
      _children[i]->freeChildTree();
      delete _children[i];
   }
   if (_ximage) {
      XDestroyImage(_ximage);
      _ximage = NULL;
   }

   _num_children = 0;
}

////////////////////////////////////////////////////////////////////////

void XImageHandler::printHeader()
{
   printf("width = %d\n", _ximage->width);
   printf("height = %d\n", _ximage->height);
   printf("xoffset = %d\n", _ximage->xoffset);
   printf("format = %d (%s)\n", _ximage->format,
		(_ximage->format==XYBitmap ? "XYBitmap" :
		 (_ximage->format==XYPixmap ? "XYPixmap" :
		  (_ximage->format==ZPixmap ? "ZPixmap" : "Unknown"))));
   printf("data ptr = %p\n", _ximage->data);
   printf("byte_order = %d (%s)\n", _ximage->byte_order,
		(_ximage->byte_order==LSBFirst ? "LSBFirst" :
		 (_ximage->byte_order==MSBFirst ? "MSBFirst" : "Unknown")));
   printf("bitmap_unit = %d\n", _ximage->bitmap_unit);
   printf("bitmap_bit_order = %d (%s)\n", _ximage->bitmap_bit_order,
		(_ximage->bitmap_bit_order==LSBFirst ? "LSBFirst" :
		 (_ximage->bitmap_bit_order==MSBFirst ? "MSBFirst":"Unknown")));
   printf("bitmap_pad = %d\n", _ximage->bitmap_pad);
   printf("depth = %d\n", _ximage->depth);
   printf("bytes_per_line = %d\n", _ximage->bytes_per_line);
   printf("bits_per_pixel = %d\n", _ximage->bits_per_pixel);
   printf("red_mask = %lx\n", _ximage->red_mask);
   printf("green_mask = %lx\n", _ximage->green_mask);
   printf("blue_mask = %lx\n", _ximage->blue_mask);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create XImageToVicar.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// XImageToVicar - function (not class) which uses XImageHandler to write
// to a VICAR file.  This function is in a separate module to isolate
// VICAR dependencies.
////////////////////////////////////////////////////////////////////////

#include "XImageToVicar.h"
#include "XImageHandler.h"
#include "zvproto.h"

////////////////////////////////////////////////////////////////////////
// Write the given part of the XImage structure into a set of three
// VICAR files (in color).  Width==0 or height==0 means the entire
// width or height of the XImage.  X and Y are 0-based (image starts at 0,0).
////////////////////////////////////////////////////////////////////////

void writeXImageToVicarFile(XImageHandler *img,
		char *red_file, char *green_file, char *blue_file,
		int x, int y, int w, int h)
{
   unsigned char *buf1, *buf2, *buf3;
   int unit1, unit2, unit3;

   static int instance = 1;

   if (w == 0)
      w = img->width();
   if (h == 0)
      h = img->height();

   buf1 = new unsigned char[w];
   buf2 = new unsigned char[w];
   buf3 = new unsigned char[w];

   zvunit(&unit1, (char *)"XImageHandler", instance++, "u_name", red_file,NULL);
   zvunit(&unit2, (char *)"XImageHandler", instance++,"u_name",green_file,NULL);
   zvunit(&unit3, (char *)"XImageHandler", instance++, "u_name",blue_file,NULL);

   zvopen(unit1, "op", "write", "u_nl", h, "u_ns", w, NULL);
   zvopen(unit2, "op", "write", "u_nl", h, "u_ns", w, NULL);
   zvopen(unit3, "op", "write", "u_nl", h, "u_ns", w, NULL);

   for (int i=0; i<h; i++) {
      img->getColorLine(buf1, buf2, buf3, y+i, x, w);
      zvwrit(unit1, buf1, NULL);
      zvwrit(unit2, buf2, NULL);
      zvwrit(unit3, buf3, NULL);
   }
   zvclose(unit1, "clos_act", "free", NULL);
   zvclose(unit2, "clos_act", "free", NULL);
   zvclose(unit3, "clos_act", "free", NULL);

   delete buf1;
   delete buf2;
   delete buf3;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create WidgetSnapshot.cc
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

#include "WidgetSnapshot.h"
#include "XImageHandler.h"

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>

#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>

#include <X11/Xproto.h>

#include <stdio.h>	//!!!!!!!!
////////////////////////////////////////////////////////////////////////
// Error handling function for XClearArea (see below).  Ignores ClearArea
// calls and reports anything else normally.  ChangeWindowAttributes is
// also ignored, since an expose of interest calls it.
// old_handler can be static because the error function is never set in
// a recursive context.
////////////////////////////////////////////////////////////////////////
static XErrorHandler old_handler;

static int myhandler(Display *dpy, XErrorEvent *myerr)
{
   if (myerr->error_code == BadWindow)
      if (myerr->request_code == X_ClearArea ||
	  myerr->request_code == X_ChangeWindowAttributes)
         return 0;
   return (*old_handler)(dpy, myerr);
}

////////////////////////////////////////////////////////////////////////

XImageHandler *takeWidgetSnapshot(Widget w, widgetSnapSpecialFunction *fn)
{
   XImageHandler *xh = NULL;

   // Call special function if present

   if (fn) {
      xh = (*fn)(w);
      if (xh)
         return xh;
   }

   // Make sure it's a widget and has a window.  If not, bail.

   if (!XtIsWidget(w))	// Gadgets are rendered by parent
      return NULL;
   if (!XtIsRealized(w))
      return NULL;
   if (!XtIsManaged(w))
      return NULL;

   // Since we're using illegal access anyway, might as well access
   // all of the Core resources faster...

   _WidgetRec *wr = (_WidgetRec *)w;

   // Create the off-screen pixmap to render into

   Pixmap pixmap = XCreatePixmap(XtDisplay(w), XtWindow(w),
		wr->core.width, wr->core.height, wr->core.depth);

   if (pixmap == None)
      return NULL;

   // Clear the pixmap.  Why couldn't they allow XClearWindow or
   // XClearArea to apply to pixmaps???
   // NOTE: Background pixmaps could be supported here fairly easily.

   XGCValues gcval;
   gcval.foreground = wr->core.background_pixel;
   GC gc = XCreateGC(XtDisplay(w), pixmap, GCForeground, &gcval);
   XFillRectangle(XtDisplay(w), pixmap, gc, 0,0,wr->core.width,wr->core.height);
   XFreeGC(XtDisplay(w), gc);

   // Here's the really tricky part.  Temporarily replace the widget's
   // window (which would be clipped if obscured or unmapped), and replace
   // it with our off-screen pixmap.  Then, force the widget to be exposed
   // by calling the Redisplay() method directly.  Then, replace the window
   // pointer and hope nobody noticed.  ;-)

   Window save_window = wr->core.window;
   Boolean save_visible = wr->core.visible;
   wr->core.window = (Window)pixmap;		// We're committed...
   wr->core.visible = TRUE;			// force visibility

   // Now generate a synthetic Expose event, and an expose region, to send
   // to the Redisplay function

   XExposeEvent event;
   Region region;

   event.type = Expose;
   event.serial = 0;			// nothing better to put...
   event.send_event = 1;
   event.display = XtDisplay(w);
   event.window = pixmap;
   event.x = 0;
   event.y = 0;
   event.width = wr->core.width;
   event.height = wr->core.height;
   event.count = 0;

   XPoint points[4];
   points[0].x = 0; points[0].y = 0;
   points[1].x = 0; points[1].y = wr->core.height;
   points[2].x = wr->core.width; points[2].y = wr->core.height;
   points[3].x = wr->core.width; points[3].y = 0;
   region = XPolygonRegion(points, 4, WindingRule);

   // Because XClearArea and XClearWindow apply *ONLY* to Windows, not to
   // Pixmaps (grr), and these routines are used often in the various widgets'
   // Redisplay methods, we have to trap errors and simply ignore ClearArea
   // calls.  We could actually clear the pixmap again, but we just did above,
   // and I don't see a reasonable way to get at the parameters for just what
   // part of the window to clear.  Any other error we report normally.
   // This may not work for all widgets, but this routine isn't guaranteed to.

   XSync(XtDisplay(w), False);		// wait for all errors to be processed
   old_handler = XSetErrorHandler(myhandler);

   // Setup is complete, now force the expose

   (*wr->core.widget_class->core_class.expose)(w, (XEvent *)&event, region);

   // Clean up by replacing error handler and window, and freeing stuff

   XSync(XtDisplay(w), False);		// wait for all errors to be processed
   XSetErrorHandler(old_handler);

   XDestroyRegion(region);

   wr->core.window = save_window;
   wr->core.visible = save_visible;

   // Now we can get the Pixmap into a nice client-side XImage structure
   // and get rid of the Pixmap.  Create the XImageHandler to manage the
   // XImage struct as well.

   XImage *ximage = XGetImage(XtDisplay(w), pixmap, 0, 0,
	wr->core.width, wr->core.height, 0xffffffff, ZPixmap);

   xh = new XImageHandler(ximage);
   xh->setWidget(w);

   XFreePixmap(XtDisplay(w), pixmap);

   // Finally, we must look to see if this widget has children, and if so,
   // do it all again with each of them.  XImageHandler manages the hierarchy.
   // Overlapping children at the same hierarchy level are not handled well.

   if (XtIsComposite(w)) {
      CompositeRec *wc = (CompositeRec *)w;

      for (int i=0; i<(int)wc->composite.num_children; i++) {
         Widget child = wc->composite.children[i];
         _WidgetRec *child_rec = (_WidgetRec *)child;
         XImageHandler *xh_child = takeWidgetSnapshot(child, fn);
         if (xh_child != NULL)		// unmanaged will return NULL
            xh->addChild(xh_child, child_rec->core.x, child_rec->core.y);
      }
   }

   // All done!

   return xh;
}

////////////////////////////////////////////////////////////////////////

void freeWidgetSnapshot(XImageHandler *h)
{
   h->freeChildTree();
   delete h;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xsnapwidget.imake
#define SUBROUTINE xsnapwidget
#define MODULE_LIST XImageHandler.cc XImageToVicar.cc WidgetSnapshot.cc

#define GUI_SUBLIB

#define MAIN_LANG_C_PLUS_PLUS
#define USES_C_PLUS_PLUS

#define LIB_MOTIF
#define LIB_RTL

$ Return
$!#############################################################################
$Other_File:
$ create xsnapwidget.doc
Documentation file for xsnapwidget.com/xsnapwidget_h.com

XImage to VICAR File Converter
------------------------------

The two modules, XImageHandler.h/.cc and XImageToVicar.h/.cc, allow conversion
from an arbitrary XImage structure (such as that returned by XGetImage() or
XRT's getImage()) into a VICAR file or other such medium.

The data can be returned either "raw" (the values are unsigned longs that are
actually in the pixel buffer), or as "color" (the values are unsigned bytes
representing the RGB color of the pixel.

In order to use these functions, you first create an instance of XImageHandler,
passing the XImage structure in the constructor.  NOTE:  The destructor does
*not* delete the XImage structure, that's up to you.

If you want to get the XImage data in color, you must supply the colormap
and visual type (setVisual(), setColormap()) that the XImage used to be
displayed.  This can be simplified by providing a widget instead (setWidget());
the colormap and visual type are derived from this widget (assming it's
realized!).

Once you've set up the XImageHandler, separate calls allow you to get at a
line of data either as raw pixels (getLine()), or converted into color using
the colormap (getColorLine()).

XImageHandler also provides the capability to manage overlaying child XImage
structures, which is used by the Widget Snapshot capability.  It is not further
discussed here.

The writeXImageToVicarFile() function (NOTE:  NOT a class member function!)
uses the public interface of XImageHandler to write a color VICAR file triplet.
This subroutine is in a separate module in order to reduce and isolate the
dependencies on VICAR.  Zeros may be provided for height and width to represent
the entire image, and the first pixel is at (0,0).

Example of call:

ximage = _graph->getImage();	// _graph is XRT
XImageHandler h(ximage);
h.setWidget(_graph->getWidget());
writeXImageToVicarFile(&h, "tmp.red","tmp.grn","tmp.blu", 0, 0, 0, 0);

Widget Snapshot
---------------

The Widget Snapshot provides the capability to get a "screen dump" of a
widget tree without necessarily rendering it on screen.  In order to use
this, the widgets in the tree must be managed and realized, but they do
not have to be mapped (set mappedWhenManaged to False, or simply create a
dialog or main window (e.g. MotifApp MainWindow) but don't pop it up.
For a MotifApp MainWindow, this means overriding manage() so as to not
call XtPopup() or XMapRaised().

The snapshot function (not class!) takeWidgetSnapshot() creates a tree
of XImageHandler objects with the rendering of each item in the widget
tree.  In a nutshell, it does this by temporarily replacing each widget's
window field with an off-screen pixmap, and forcing a redisplay (by directly
calling the widget's redisplay method).  This involves lots of illegal
accesses to X Toolkit internals, and is not guaranteed to work with all
widgets.

If you have widgets which cannot be rendered in this way, an optional
second argument to takeWidgetSnapshot() allows you to supply a pointer to
a function to render special widgets.  This function is called at the
beginning of takeWidgetSnapshot().  If it returns an XImageHandler object,
that is simply returned.  If it returns NULL (or doesn't exist), the
standard processing described above is performed.  For example, the XRT
graph widget cannot be rendered using the above method (for unknown reasons).
However, XRT does provide a XrtGetImage() function to return the XImage
for their widget, which can be called instead.  See below for an example.
takeWidgetSnapshot() does no more processing on the returned XImageHandler
object, so it must be completely set up.  Also, the special function is
called before the widget state (IsWidget, IsRealized, IsManaged) is checked,
so those should be checked (if relevant) in the special function.  This
functionality is provided via a function pointer rather than being built
in so there is no dependency on the XRT widget set (a commercial product)
in this package.

takeWidgetSnapshot() should be called after all widgets are managed and
realized.  If this function is invoked via a menu, this is not a problem.
However, an automated, no-display application needs to get control back after
the event loop is started to ensure everything is ready.  A WorkProc should
be sufficient for this; however, it does not seem to work well in all
cases.  A TimeOut can be used instead, or another method could be found.

The returned XImageHandler object can then be used to extract the image
data as described above, or can be used with writeXImageToVicarFile().
The XImageHandler children are transparent; the getLine() and getColorLine()
functions take care of overlaying the children.  It is not necessary to
(actually, it's better not to) set the colormap, visual, or widget in
the XImageHandler class; takeWidgetSnapshot does this automatically.

The returned XImageHandler object can be freed (with all its children
and XImage structures) via freeWidgetSnapshot().

Example of call:

void timer_proc(XtPointer client_data, XtIntervalId *)
{
   Widget w = (Widget)client_data;

   XImageHandler *h = takeWidgetSnapshot(w);
   writeXImageToVicarFile(h, "tmp.red", "tmp.grn", "tmp.blu", 0,0,0,0);
   freeWidgetSnapshot(h);
}

Example of call handling special widgets:

#include <Xm/XrtGraph.h>
#include <Xm/Xrt3d.h>
XImageHandler *getXrtWidgets(Widget w)
{
   XImageHandler *xh = NULL;
   XImage *xi;
   if (!XtIsXrtGraph(w) && !XtIsXrt3d(w)) return NULL;
   if (!XtIsRealized(w)) return NULL;	// don't need IsWidget()
   if (!XtIsManaged(w)) return NULL;
   if (XtIsXrt3d(w))
      xi = Xrt3dGetImage(w);
   else
      xi = XrtGetImage(w);
   if (xi != NULL) {
      xh = new XImageHandler(xi);
      xh->setWidget(w);
   }
   return xh;
}

...
   XImageHandler *h = takeWidgetSnapshot(w, &getXrtWidgets);
...

$ Return
$!#############################################################################
