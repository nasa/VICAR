//////////////////////////////////////////////////////////////
// SgDrawAreaInterface.cc: A "push button" interface to a Cmd object.
// The surface of a push-button is a drawing area widget.
// This class is intended to have subclasses.  The subclass
// must provide the drawing area widget.
///////////////////////////////////////////////////////////////
#include "SgDrawAreaInterface.h"
#include <Xm/DrawingA.h>
#include <Xm/Frame.h>
#include <assert.h>

SgDrawAreaInterface::SgDrawAreaInterface ( Widget parent, Cmd *cmd )
	: CmdInterface ( cmd )
{
   _w = XtVaCreateWidget ( _name, 
			   xmFrameWidgetClass, parent,
			   XmNshadowType, XmSHADOW_OUT,
			   NULL);
   installDestroyHandler();

   _drawingAreaWidget = NULL;

   // The _active member is set when each instance is registered
   // with an associated Cmd object. Now that a widget exists,
   // set the widget's sensitivity according to its active state.

    if ( _active )
        activate();
    else
        deactivate();
}

SgDrawAreaInterface::~SgDrawAreaInterface()
{
    if (_drawingAreaWidget)
	XtDestroyWidget(_drawingAreaWidget);
}

//////////////////////////////////////////////////////////////
// Set the drawing area widget and declare input callback on it.
/////////////////////////////////////////////////////////////
void SgDrawAreaInterface::setDrawingAreaWidget ( Widget w )
{
//    assert ( !XmIsDrawingArea ( w ) );

    _drawingAreaWidget = w;

    XtAddCallback ( _drawingAreaWidget, 
		XmNinputCallback,
		&CmdInterface::executeCmdCallback,
		(XtPointer) this );
}

//////////////////////////////////////////////////////////////
// Handle an event.  If it's a button press, we just invert the
// frame.  If it's a button release, we revert it, and if we're
// still in the window, we call the command.
//////////////////////////////////////////////////////////////
void SgDrawAreaInterface::executeCmd ( XtPointer callData )
{
   if ( _drawingAreaWidget == NULL ) 
	return;

   XmDrawingAreaCallbackStruct *cb = (XmDrawingAreaCallbackStruct *)callData;
 
   if ( cb->reason != XmCR_INPUT )
      return;
 
   if ( cb->event->type == ButtonPress ) {
      XButtonPressedEvent *xbp = (XButtonPressedEvent *)cb->event;
      if ( xbp->button != 1 )
         return;                        // Ignore all but button 1
      XtVaSetValues ( _w, XmNshadowType, XmSHADOW_IN, NULL );
   }
 
   if ( cb->event->type == ButtonRelease ) {
      XButtonReleasedEvent *xbr = (XButtonReleasedEvent *)cb->event;
      if ( xbr->button != 1 )
         return;                        // Ignore all but button 1
      XtVaSetValues ( _w, XmNshadowType, XmSHADOW_OUT, NULL );
 
      // If we're still over the button, run the command
 
      Dimension width, height;
      XtVaGetValues ( _drawingAreaWidget, 
		XmNwidth, &width, 
		XmNheight, &height,
		NULL );
      if ( (xbr->x >= 0) && (xbr->x < (int)width) &&
           (xbr->y >= 0) && (xbr->y < (int)height) ) {
         runCmd();
      }
   }
}

int SgDrawAreaInterface::operator== ( const SgDrawAreaInterface &dai )
{
   return this == &dai;
}
