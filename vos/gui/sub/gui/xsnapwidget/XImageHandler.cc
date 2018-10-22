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

