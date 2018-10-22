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
