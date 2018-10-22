//////////////////////////////////////////////////////////////////////////////
// TpQualFormatCmdInterface.cc:  
//////////////////////////////////////////////////////////////////////////////
#include "TpQualFormatCmdInterface.h"
#include "TpQualFormatView.h"
#include "TpQualFormatValue.h"
#include "TpDefs.h"
#include "KeyinView.h"
#include "Cmd.h"
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <Xm/TextF.h>
#include <stdlib.h>
#include <stdio.h>

TpQualFormatCmdInterface::TpQualFormatCmdInterface(Widget parent, Cmd *cmd) 
	: CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
    installDestroyHandler();

    XtVaCreateManagedWidget("qualFormatFrameLabel",
                            xmLabelGadgetClass, _w,
                            XmNchildType, XmFRAME_TITLE_CHILD,
                            XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
                            NULL);

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
 
    if (_active)
        activate();
    else
        deactivate();

    Widget form = XtVaCreateManagedWidget("formQualFormat", 
					  xmFormWidgetClass, _w,
					  NULL);

    _numQualsView = new KeyinView(form, "numQualsView");
    _numQualsView->installCallback(
	&TpQualFormatCmdInterface::setNumQualsCallback, (XtPointer)this);
    XtVaSetValues(_numQualsView->baseWidget(), 
		  XmNtopAttachment, XmATTACH_FORM, 
		  XmNleftAttachment, XmATTACH_FORM, 
		  XmNrightAttachment, XmATTACH_NONE,
		  XmNbottomAttachment, XmATTACH_NONE,
		  NULL);
    _numQualsView->manage();

    _qualView = new TpQualFormatView(form, "qualFormatView", this);
    XtVaSetValues(_qualView->baseWidget(),
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, _numQualsView->baseWidget(),
		  XmNleftAttachment, XmATTACH_FORM, 
		  XmNrightAttachment, XmATTACH_FORM, 
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
    _qualView->manage();

    setValue(_cmd->getValue());
}

void TpQualFormatCmdInterface::addQualCallback(Widget, 
		XtPointer clientData, XtPointer)
{
    TpQualFormatCmdInterface *obj;
    obj = (TpQualFormatCmdInterface *)clientData;
    if (obj != NULL)
        obj->addQual();
}

void TpQualFormatCmdInterface::deleteQualCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpQualFormatCmdInterface *obj;
    obj = (TpQualFormatCmdInterface *)clientData;
    if (obj != NULL)
        obj->deleteQual();
}

void TpQualFormatCmdInterface::setNumQualsCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpQualFormatCmdInterface *obj;
    obj = (TpQualFormatCmdInterface *)clientData;
    if (obj != NULL)
        obj->setNumQuals();
}

void TpQualFormatCmdInterface::addQual()
{
    
}

void TpQualFormatCmdInterface::setNumQuals()
{
    int n = atoi(_numQualsView->getFieldValue());
    //_value = new TpQualFormatValue(*((TpQualFormatValue *)(_cmd->getValue())));
    _value->setNumQuals(n);
    _qualView->setQuals(_value);
    runCmd((CmdValue)_value);
}

void TpQualFormatCmdInterface::deleteQual()
{
    
}

void TpQualFormatCmdInterface::setName(char *name, int n)
{
    _value->_info[n]._qualName = sdup(name);
    runCmd((CmdValue)_value);
}

void TpQualFormatCmdInterface::setUnit(char *unit, int n)
{
    _value->_info[n]._qualUnit = sdup(unit);
    runCmd((CmdValue)_value);
}

void TpQualFormatCmdInterface::setType(TpQualType t, int n)
{
    _value->_info[n]._qualType = t;
    runCmd((CmdValue)_value);
}

void TpQualFormatCmdInterface::setValue(CmdValue v)
{
    _value = new TpQualFormatValue(*((TpQualFormatValue *)v));

    char buf[16];
    sprintf(buf, "%d", _value->getNumQuals());
    _numQualsView->setFieldValue(buf);

    _qualView->setQuals(_value);
}
