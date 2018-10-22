//////////////////////////////////////////////////////////////////////////////
// TpContrastControl.cc: Contrast control.
//////////////////////////////////////////////////////////////////////////////
#include "TpContrastControl.h"
#include "TpWedgeOverlayView.h"
#include "TpContrastValue.h"
#include "Cmd.h"
#include "PrefManager.h"
#include "TpContrastControlPrefView.h"
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/LabelG.h>
#include <Xm/TextF.h>
#include <stdlib.h>
#include <stdio.h>

XtResource TpContrastControl::_resources[] = {
    {
	(char *)"minContrast", 
	(char *)"MinContrast", 
	XmRInt,
	sizeof(int),
	XtOffset(TpContrastControl *, _min),
	XmRImmediate,
	(XtPointer)0,
    },
    {
        (char *)"maxContrast", 
        (char *)"MaxContrast", 
        XmRInt,
        sizeof(int),
        XtOffset(TpContrastControl *, _max),
        XmRImmediate,
        (XtPointer)255,
    },
};

void TpContrastControl::reload(TpContrastControl *copy)
{
    if (_min != copy->_min)
	setLowContrast(copy->_min);
    if (_max != copy->_max)
        setHighContrast(copy->_max);
}

TpContrastControl::TpContrastControl(Widget parent, Cmd *cmd) 
	: CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
    installDestroyHandler();

    getResources(_resources, XtNumber(_resources));

    char path[100];
    sprintf(path, "*%s*", _name);
    thePrefManager->registerResources(this,
				      _resources, XtNumber(_resources),
				      path, new TpContrastControlPrefView());

    XtVaCreateManagedWidget ("contrastFrameLabel",
                xmLabelGadgetClass, _w,
                XmNchildType, XmFRAME_TITLE_CHILD,
                XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
                NULL );

    Widget form = XtVaCreateManagedWidget("form", xmFormWidgetClass, _w, NULL);

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
 
    if (_active)
        activate();
    else
        deactivate();

    // Create text field to keyin low contrast value

    _keyinLowContrast = XtVaCreateManagedWidget("keyinLowContrast", 
				xmTextFieldWidgetClass, form,
				NULL);
    XtAddCallback(_keyinLowContrast, XmNlosingFocusCallback, 
				&TpContrastControl::setLowContrastCallback, 
				(XtPointer)this);
    XtAddCallback(_keyinLowContrast, XmNactivateCallback,
				&TpContrastControl::setLowContrastCallback,
				(XtPointer)this);

    // Create text field to keyin high contrast value

    _keyinHighContrast = XtVaCreateManagedWidget("keyinHighContrast",
                                xmTextFieldWidgetClass, form,
                                NULL);
    XtAddCallback(_keyinHighContrast, XmNlosingFocusCallback,
                                &TpContrastControl::setHighContrastCallback,
                                (XtPointer)this);
    XtAddCallback(_keyinHighContrast, XmNactivateCallback,
                                &TpContrastControl::setHighContrastCallback,
                                (XtPointer)this);

    // Create gray wedge

    _wedge = new TpWedgeOverlayView(form, "wedge", _cmd);

    // Set form attachments

    XtVaSetValues(_keyinLowContrast, 
		  XmNtopAttachment,       XmATTACH_FORM,
		  XmNleftAttachment,      XmATTACH_FORM,
		  XmNrightAttachment,     XmATTACH_NONE,
		  XmNbottomAttachment,    XmATTACH_FORM,
		  NULL);
    XtVaSetValues(_wedge->baseWidget(),
		  XmNtopAttachment,       XmATTACH_FORM,
		  XmNleftAttachment,      XmATTACH_WIDGET,
		  XmNleftWidget,          _keyinLowContrast,
		  XmNrightAttachment,     XmATTACH_NONE,
		  //XmNrightWidget,         _keyinHighContrast,
		  XmNbottomAttachment,    XmATTACH_FORM,
		  NULL);
    XtVaSetValues(_keyinHighContrast,
		  XmNtopAttachment,       XmATTACH_FORM,
		  XmNleftAttachment,      XmATTACH_WIDGET,
		  XmNleftWidget,          _wedge->baseWidget(),
		  XmNrightAttachment,     XmATTACH_NONE,
		  XmNbottomAttachment,    XmATTACH_FORM,
		  NULL);

    _wedge->manage();

    TpContrastValue *value = new TpContrastValue(_min, _max);
    _cmd->execute((CmdValue)value);
}

void TpContrastControl::setLowContrastCallback(Widget w, 
		XtPointer clientData, XtPointer)
{
    int i = atoi(XmTextFieldGetString(w));

    TpContrastControl *obj;
    obj = (TpContrastControl *)clientData;
    if (obj != NULL)
	obj->setLowContrast(i);
}

void TpContrastControl::setHighContrastCallback(Widget w, 
		XtPointer clientData, XtPointer)
{
    int i = atoi(XmTextFieldGetString(w));

    TpContrastControl *obj;
    obj = (TpContrastControl *)clientData;
    if (obj != NULL)
        obj->setHighContrast(i);
}

void TpContrastControl::setLowContrast(int i)
{
    TpContrastValue *value = (TpContrastValue *)(_cmd->getValue());
    value->setMin(i);

    _cmd->execute((CmdValue *)value);
}

void TpContrastControl::setHighContrast(int i)
{
    TpContrastValue *value = (TpContrastValue *)(_cmd->getValue());
    value->setMax(i);
 
    _cmd->execute((CmdValue *)value);
}

void TpContrastControl::setValue(CmdValue v)
{
    TpContrastValue *value = (TpContrastValue *)v;

    _min = value->getMin();
    _max = value->getMax();

    char buf[16];

    sprintf(buf, "%d", _min);
    XmTextFieldSetString(_keyinLowContrast, buf);

    sprintf(buf, "%d", _max);
    XmTextFieldSetString(_keyinHighContrast, buf);

    _wedge->update(value);
}
