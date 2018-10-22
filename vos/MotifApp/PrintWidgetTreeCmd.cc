////////////////////////////////////////////////////////
// PrintWidgetTreeCmd.cc: Debugging command used to print out the
// names and classes of a widget and all its ancestors.
////////////////////////////////////////////////////////
#include "PrintWidgetTreeCmd.h"
#include "InfoDialogManager.h"
#include <X11/cursorfont.h>
#include <X11/IntrinsicP.h>
#include <X11/ObjectP.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

PrintWidgetTreeCmd::PrintWidgetTreeCmd(const char *name,int active,Widget baseW)
		: NoUndoCmd(name, active)
{
    _baseW = baseW;
    _post_to_dialog = 0;
}

PrintWidgetTreeCmd::PrintWidgetTreeCmd(const char *name, int active, 
					int post, Widget baseW)
		: NoUndoCmd(name, active)
{
    _baseW = baseW;
    _post_to_dialog = post;
}

void PrintWidgetTreeCmd::doit()
{
    Cursor cursor;
    Display *dpy = XtDisplay(_baseW);
    XEvent event;
    int bufflen = 512;
    char *buffer, *temp, tempBuff[512];
    buffer = (char *) NULL;

    cursor = XCreateFontCursor(dpy, XC_hand2);

    Widget w = XmTrackingEvent(_baseW, cursor, False,
			&event);
    XFreeCursor(dpy, cursor);
    if (w == NULL)
        return;

    // preallocate some space
    if (!(buffer = (char *) malloc(bufflen))) {
	theInfoDialogManager->post(
	    (char *)"Insufficient memory to complete widget list.");
	return;
    }

    sprintf(tempBuff, "\nWidget: %s  (%s)\n", XtName(w),
			((ObjectClass)XtClass(w))->object_class.class_name);
    strcpy (buffer, tempBuff);
    w = XtParent(w);

    while (w != NULL) {
        sprintf(tempBuff,"Parent: %s  (%s)\n", XtName(w),
	      ((ObjectClass)XtClass(w))->object_class.class_name);
        w = XtParent(w);
        if (strlen(tempBuff) > bufflen-strlen(buffer)-1) {
	    temp = buffer;
	    bufflen += 512;
	    if (!(buffer = (char *) malloc(bufflen))) {
	        theInfoDialogManager->post(
		   (char *)"Insufficient memory to complete widget list.");
	        if (temp) free (temp);
	        return;
	    }
	    strcpy (buffer, temp);
	    free (temp);
	}
        strcat(buffer, tempBuff);
    }
    if (_post_to_dialog) theInfoDialogManager->post(buffer);
    else printf ("%s", buffer);
    free (buffer);
}

