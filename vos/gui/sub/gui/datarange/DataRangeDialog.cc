////////////////////////////////////////////////////////////////////////
// DataRangeDialog.cc:  Create the Data Range dialog.
////////////////////////////////////////////////////////////////////////
#include "DataRangeDialog.h"
#include "CheckBoxInterface.h"
#include "Cmd.h"
#include "StringKeyinInterface.h"
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <stdio.h>

DataRangeDialog::DataRangeDialog(const char *name, Cmd *minAutoCmd,
		Cmd *maxAutoCmd, Cmd *minValueCmd, Cmd *maxValueCmd)
	: CustomDialog(name, Default, Invisible, Invisible, Invisible, Visible)
{
   _minAutoCmd = minAutoCmd;
   _maxAutoCmd = maxAutoCmd;
   _minValueCmd = minValueCmd;
   _maxValueCmd = maxValueCmd;
   _minInterface = NULL;
   _maxInterface = NULL;
   _saveMin = 0.0;
   _saveMax = 0.0;
}

Widget DataRangeDialog::createWorkArea(Widget parent)
{
   Widget form = XtVaCreateWidget("DataRangeDialog", xmFormWidgetClass,
		parent, NULL);

   Widget label = XtVaCreateManagedWidget("Set Data Range", xmLabelWidgetClass,
		form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

   Widget label2= XtVaCreateManagedWidget("Auto Range", xmLabelWidgetClass,
		form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, label,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

   Widget minForm = XtVaCreateManagedWidget("minForm", xmFormWidgetClass,
		form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, label2,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

   Widget maxForm = XtVaCreateManagedWidget("maxForm", xmFormWidgetClass,
		form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, minForm,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

   CmdInterface *ci1 = new CheckBoxInterface(minForm, _minAutoCmd);
   XtVaSetValues(ci1->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		NULL);
   _minInterface = new StringKeyinInterface(minForm, _minValueCmd);
   XtVaSetValues(_minInterface->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, ci1->baseWidget(),
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

   CmdInterface *ci2 = new CheckBoxInterface(maxForm, _maxAutoCmd);
   XtVaSetValues(ci2->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		NULL);
   _maxInterface = new StringKeyinInterface(maxForm, _maxValueCmd);
   XtVaSetValues(_maxInterface->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, ci2->baseWidget(),
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

   // Manage Interfaces

   ci1->manage();
   ci2->manage();
   _minInterface->manage();
   _maxInterface->manage();

   // Set commands to defer command execution until the apply button is pressed
   // Policy decision: don't defer this dialog box.  It feels better that way.

//   ci1->setDeferredExec(_applyCmdList);
//   ci2->setDeferredExec(_applyCmdList);
//   _minInterface->setDeferredExec(_applyCmdList);
//   _maxInterface->setDeferredExec(_applyCmdList);

   setDataRange(_saveMin, _saveMax);

   return form;
}

void DataRangeDialog::setDataRange(double min, double max)
{
   char buf[30];

   sprintf(buf, "%g", min);
   if (_minInterface)
      _minInterface->setValue((CmdValue)strdup(buf));
   else
      _saveMin = min;
   sprintf(buf, "%g", max);
   if (_maxInterface)
      _maxInterface->setValue((CmdValue)strdup(buf));
   else
      _saveMax = max;
}

