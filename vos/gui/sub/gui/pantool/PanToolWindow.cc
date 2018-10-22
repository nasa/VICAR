////////////////////////////////////////////////////////////////////////
// Compenent that creates a popup window containing a pan tool.
////////////////////////////////////////////////////////////////////////
#include "PanToolWindow.h"
#include "PanTool.h"
#include <Xm/Form.h>

// Resources for this class

XtResource PanToolWindow::_resources[] = {
 {	// Square this to get desired area for pan tool to cover, by default.
   (char *)XvicNpanDesiredSize,
   (char *)XvicCpanDesiredSize,
   XmRInt,
   sizeof(int),
   XtOffset(PanToolWindow *, _pan_desired_size),
   XmRImmediate,
   (XtPointer) 200,
 },
};

// "model" is used to display the image; "big_iw" is the Widget that
// we're controlling pan for.

PanToolWindow::PanToolWindow(const char *name, ImageData *model, Widget big_iw,
	Lut *rlut, Lut *glut, Lut *blut, Lut *rplut, Lut *gplut, Lut *bplut)
			: MainWindow(name)
{
   _model = model;
   _big_iw = big_iw;
   _rlut = rlut;
   _glut = glut;
   _blut = blut;
   _rplut = rplut;
   _gplut = gplut;
   _bplut = bplut;
}

Widget PanToolWindow::createWorkArea(Widget parent)
{
   getResources(_resources, XtNumber(_resources));

   // Tell the Shell not to destroy us
   XtVaSetValues(_w, XmNdeleteResponse, XmUNMAP, NULL);

   _form = XtVaCreateWidget(_name, xmFormWidgetClass, parent,
		NULL);

   // Create the tool

   _panTool = new PanTool(_form, "panTool", _model, _big_iw,
		_pan_desired_size, _pan_desired_size, True,
		_rlut, _glut, _blut, _rplut, _gplut, _bplut);

   XtVaSetValues(_panTool->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
   _panTool->manage();

   return _form;
}

