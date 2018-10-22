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

