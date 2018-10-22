////////////////////////////////////////////////////////
// HelpOnContextCmd.cc: Command used to implement context-sensitive help
////////////////////////////////////////////////////////
#include "HelpOnContextCmd.h"
#include "HelpBrowser.h"
#include <X11/cursorfont.h>
#include <iostream>

HelpOnContextCmd::HelpOnContextCmd(const char *name, int active, Widget baseW)
		: NoUndoCmd(name, active)
{
    _baseW = baseW;
}

void HelpOnContextCmd::doit()
{
    Cursor cursor;
    Display *dpy = XtDisplay(_baseW);
    XEvent event;

    // Find out what widget the user wants help on

    cursor = XCreateFontCursor(dpy, XC_question_arrow);

    Widget w = XmTrackingEvent(_baseW, cursor, False,
			&event);
    XFreeCursor(dpy, cursor);
    XFlush(dpy);
    if (w == NULL)
        return;

    // Now run the browser on this wiget.

    theHelpBrowser->run(w);
}

