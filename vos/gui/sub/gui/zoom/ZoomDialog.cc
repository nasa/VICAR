////////////////////////////////////////////////////////////////
// ZoomDialog.cc - Dialog box for the zoom factor
////////////////////////////////////////////////////////////////
#include "ZoomDialog.h"
#include "ZoomCmdInterface.h"
#include "ZoomRadioCmdBox.h"
#include "ZoomCmdSet.h"
#include "ZoomCmd.h"
#include "ZoomSpecialCmd.h"
#include "ZoomSpecialCmdList.h"
#include <Xm/Form.h>

ZoomDialog::ZoomDialog(const char *name, ZoomCmdSet *zoomCmdSet)
		: CustomDialog(name)
{
   _zoomCmdSet = zoomCmdSet; 
}

ZoomDialog::~ZoomDialog()
{
   delete _zoomRadioCmdBox;
   delete _zoomCmdInterface;
}

////////////////////////////////////////////////////////////////
// Create the actual dialog box here
////////////////////////////////////////////////////////////////

Widget ZoomDialog::createWorkArea(Widget parent)
{
   // Create work area widget

   Widget form = XtVaCreateWidget("ZoomDialog", xmFormWidgetClass, parent,
		NULL);

   // Create a radiobox of zoom commands
   _zoomRadioCmdBox = new ZoomRadioCmdBox(form, "ZoomRadioCmdBox",
				_zoomCmdSet->getRadioList(), _applyCmdList);
   XtVaSetValues(_zoomRadioCmdBox->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		NULL);
   _zoomRadioCmdBox->manage();

   // Create ZoomCmdInterface and defer its execution to the ZoomSpecialCmdList

   _zoomCmdInterface = new ZoomCmdInterface(form, _zoomCmdSet->getZoomCmd(),
				_zoomCmdSet->getImageView());
   XtVaSetValues(_zoomCmdInterface->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, _zoomRadioCmdBox->baseWidget(),
		NULL);
   _zoomCmdInterface->manage();
   _zoomCmdInterface->setDeferredExec(_zoomCmdSet->getZoomSpecialCmdList());

   // Pass the ZoomSpecialCmd interface widget to ZoomSpecialCmdList

   Widget w =
	_zoomRadioCmdBox->getInterfaceWidget(_zoomCmdSet->getZoomSpecialCmd());
   _zoomCmdSet->getZoomSpecialCmdList()->setInterfaceWidget(w);

   return form;
}

