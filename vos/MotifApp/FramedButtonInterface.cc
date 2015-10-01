////////////////////////////////////////////////////////////////        
// FramedButtonInterface.h: A push button interface with a frame
// around it.  The frame does not "push in"; it is used to get
// different visual effects.
////////////////////////////////////////////////////////////////

#include "FramedButtonInterface.h"
#include <Xm/Frame.h>
#include <Xm/PushB.h>

FramedButtonInterface::FramedButtonInterface(Widget parent, Cmd *cmd,
			unsigned char frameType)
	: CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent,
		XmNshadowType, frameType, NULL);
    installDestroyHandler();

    _pushB = XtVaCreateManagedWidget(_name, xmPushButtonWidgetClass, _w,
		XmNshadowThickness, 0, NULL);

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
    
    if ( _active )
	activate();     
    else
	deactivate();   

// No idea what the right ifdef is for automatically recognizing g++
#ifdef GNU_CC
    
    // G++ reportedly doesn't like the form expected by cfront. I'm
    // told this will work, but I haven't tested it myself.
    
    XtAddCallback ( _pushB,
		   XmNactivateCallback, 
		   executeCmdCallback,
		   (XtPointer) this );  
#else

    XtAddCallback ( _pushB,
		   XmNactivateCallback, 
		   &CmdInterface::executeCmdCallback,
		   (XtPointer) this );  
#endif

}

////////////////////////////////////////////////////////////////

void FramedButtonInterface::setButtonLabel(char *label)
{
    XmString string = XmStringCreateLocalized(label);

    XtVaSetValues(_pushB, XmNlabelString, string, NULL);

    XmStringFree(string);

    // If the frame is not managed, yet the parent is realized, under certain
    // circumstances setting the string will not update the width of the
    // button, which ends up with a 0 width.  Changing the shadow thickness,
    // then setting it back, seems to fix the problem.  Go figure.  Apparently
    // this is a Motif bug (at least on Solaris 2.3).

    if (!XtIsManaged(_w)) {
        Dimension shadow;
        XtVaGetValues(_w, XmNshadowThickness, &shadow, NULL);
        XtVaSetValues(_w, XmNshadowThickness, shadow+1, NULL);
        XtVaSetValues(_w, XmNshadowThickness, shadow, NULL);
    }
}

