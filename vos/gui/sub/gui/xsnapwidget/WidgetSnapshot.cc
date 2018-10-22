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

