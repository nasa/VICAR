//////////////////////////////////////////////////////////////
//ZoomCmdInterface.cc: Integer text field interface to a Cmd object
//////////////////////////////////////////////////////////////
#include "ZoomCmdInterface.h"
#include "Cmd.h"
#include "ZoomFactor.h"
#include "KeyinView.h"
#include "BasicImageView.h"
#include "XvicImage.h"
#include <Xm/Form.h>
#include "stdlib.h"		// for atoi()
#include "stdio.h"		// for sprintf()

ZoomCmdInterface::ZoomCmdInterface(Widget parent, Cmd *cmd,
			BasicImageView *imageView) // = NULL
		: CmdInterface(cmd)
{
   createAllSubViews(parent, imageView);
}

ZoomCmdInterface::ZoomCmdInterface(Widget parent, const char *name,
			BasicImageView *imageView) // = NULL
		: CmdInterface(name)
{
    createAllSubViews(parent, imageView);
}

ZoomCmdInterface::~ZoomCmdInterface()
{
   delete _zoomXIn;
   delete _zoomXOut;
   delete _zoomYIn;
   delete _zoomYOut;
}

//////////////////////////////////////////////////////////////
// Add the KeyinView for one text field
//////////////////////////////////////////////////////////////

KeyinView *ZoomCmdInterface::addOneSubView(Widget parent, const char *name)
{
   KeyinView *view = new KeyinView(parent, name);
   view->manage();
   view->installCallback(&CmdInterface::executeCmdCallback, (XtPointer)this);
   return view;
}

//////////////////////////////////////////////////////////////
// Add keyins for all four components of the zoom.  Also, set up
// the zoom-changed callback on the image widget, and set up the
// initial value of the text widgets.
//////////////////////////////////////////////////////////////

void ZoomCmdInterface::createAllSubViews(Widget parent,
				BasicImageView *imageView)
{
   _w = XtCreateManagedWidget(_name, xmFormWidgetClass, parent,
			 NULL, 0 );
   installDestroyHandler();

   _zoomXIn  = addOneSubView( _w, "Zoom X In" );
   XtVaSetValues(_zoomXIn->baseWidget(),
	XmNleftAttachment, XmATTACH_FORM, XmNrightAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_FORM,
	NULL);
   _zoomXOut = addOneSubView( _w, "Zoom X Out" );
   XtVaSetValues(_zoomXOut->baseWidget(),
	XmNleftAttachment, XmATTACH_FORM, XmNrightAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_WIDGET, XmNtopWidget, _zoomXIn->baseWidget(),
	NULL);
   _zoomYIn  = addOneSubView( _w, "Zoom Y In" );
   XtVaSetValues(_zoomYIn->baseWidget(),
	XmNleftAttachment, XmATTACH_FORM, XmNrightAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_WIDGET, XmNtopWidget,_zoomXOut->baseWidget(),
	NULL);
   _zoomYOut = addOneSubView( _w, "Zoom Y Out" );
   XtVaSetValues(_zoomYOut->baseWidget(),
	XmNleftAttachment, XmATTACH_FORM, XmNrightAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_WIDGET, XmNtopWidget, _zoomYIn->baseWidget(),
	NULL);

   // Set the initial value from the widget if present, or the cmd if not

   if (imageView && imageView->getWidget()) {
      XtAddCallback(imageView->getWidget(), XvicNvisibleAreaCallback,
		&ZoomCmdInterface::newZoomCallback, (XtPointer)this);
      newZoom(imageView->getWidget());
   }
   else if (_cmd)
      setValue(_cmd->getValue());
}

//////////////////////////////////////////////////////////////
// If any of the zoom values changes, create a ZoomFactor object
// and execute the command.
//////////////////////////////////////////////////////////////

void ZoomCmdInterface::executeCmd(XtPointer)
{
   char *in_text, *out_text;
   ZoomFactor *z = new ZoomFactor;

   in_text = _zoomXIn->getFieldValue();
   out_text = _zoomXOut->getFieldValue();
   z->setX(atoi(in_text), atoi(out_text));
   XtFree(in_text);
   XtFree(out_text);

   in_text = _zoomYIn->getFieldValue();
   out_text = _zoomYOut->getFieldValue();
   z->setY(atoi(in_text), atoi(out_text));
   XtFree(in_text);
   XtFree(out_text);

   runCmd((CmdValue)z);
}

//////////////////////////////////////////////////////////////
// If the zoom changes, update what's displayed in the text fields
//////////////////////////////////////////////////////////////

void ZoomCmdInterface::setValue(CmdValue value)
{
   char buf[50];
   ZoomFactor *z = (ZoomFactor *) value;

   CmdInterface::setValue(value);	// Removes cmd from deferred list

   if (z == NULL)			// No value has been set
      return;

   sprintf(buf, "%d", z->getXIn());
   _zoomXIn->setFieldValue(buf);
 
   sprintf(buf, "%d", z->getXOut());
   _zoomXOut->setFieldValue(buf);
    
   sprintf(buf, "%d", z->getYIn());
   _zoomYIn->setFieldValue(buf);

   sprintf(buf, "%d", z->getYOut());
   _zoomYOut->setFieldValue(buf);
}

//////////////////////////////////////////////////////////////
// Callback called when the zoom factor changes
//////////////////////////////////////////////////////////////

void ZoomCmdInterface::newZoomCallback(Widget w, XtPointer clientData,
						 XtPointer callData)
{
   ZoomCmdInterface *obj = (ZoomCmdInterface *)clientData;
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)callData;

   if (cb->reason != XvicCR_VISIBLE_AREA)
      return;
   if (! (cb->flags & XvicZOOM_CHANGED))
      return;

   obj->newZoom(w);
}

//////////////////////////////////////////////////////////////
// New zoom factor in widget; update display
//////////////////////////////////////////////////////////////

void ZoomCmdInterface::newZoom(Widget w)
{
   int xZoomIn, xZoomOut, yZoomIn, yZoomOut;

   XtVaGetValues(w, XvicNxZoomIn, &xZoomIn, XvicNxZoomOut, &xZoomOut,
		    XvicNyZoomIn, &yZoomIn, XvicNyZoomOut, &yZoomOut, NULL);
   ZoomFactor z(xZoomIn, yZoomIn, xZoomOut, yZoomOut);
   setValue((CmdValue)&z);
}

