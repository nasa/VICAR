//////////////////////////////////////////////////////////////////////////////
// TpSetMatchModeValuesCmdIf.cc:  
//////////////////////////////////////////////////////////////////////////////
#include "TpMatchModeValuesCmdIf.h"
#include "TpMatchModeValues.h"
#include "KeyinView.h"
#include "Cmd.h"
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <stdlib.h>
#include <stdio.h>

TpSetMatchModeValuesCmdIf::TpSetMatchModeValuesCmdIf(Widget parent, Cmd *cmd) 
    : CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
    installDestroyHandler();

    XtVaCreateManagedWidget("matchModeValuesFrameLabel",
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

    Widget rc = XtVaCreateManagedWidget("matchModeValuesRC", 
					xmRowColumnWidgetClass, _w, 
					NULL);

    _pmk = new KeyinView(rc, "pmk");
    _lsm = new KeyinView(rc, "lsm");
    _sw = new KeyinView(rc, "sw");
    _accuracy = new KeyinView(rc, "accuracy");
    _threshold = new KeyinView(rc, "threshold");

    _pmk->installCallback(&TpSetMatchModeValuesCmdIf::setPMKCallback, 
			  (XtPointer)this);
    _pmk->manage();

    _lsm->installCallback(&TpSetMatchModeValuesCmdIf::setLSMCallback,
                          (XtPointer)this);
    _lsm->manage();

    _sw->installCallback(&TpSetMatchModeValuesCmdIf::setSWCallback,
			 (XtPointer)this);
    _sw->manage();

    _accuracy->installCallback(&TpSetMatchModeValuesCmdIf::setAccuracyCallback,
			       (XtPointer)this);
    _accuracy->manage();

    _threshold->installCallback(
	&TpSetMatchModeValuesCmdIf::setThresholdCallback,
	(XtPointer)this);
    _threshold->manage();

    setValue(_cmd->getValue());
}

void TpSetMatchModeValuesCmdIf::setPMKCallback(Widget, 
		XtPointer clientData, XtPointer)
{
    TpSetMatchModeValuesCmdIf *obj;
    obj = (TpSetMatchModeValuesCmdIf *)clientData;
    if (obj != NULL)
        obj->setPMK();
}

void TpSetMatchModeValuesCmdIf::setLSMCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpSetMatchModeValuesCmdIf *obj;
    obj = (TpSetMatchModeValuesCmdIf *)clientData;
    if (obj != NULL)
        obj->setLSM();
}

void TpSetMatchModeValuesCmdIf::setSWCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpSetMatchModeValuesCmdIf *obj;
    obj = (TpSetMatchModeValuesCmdIf *)clientData;
    if (obj != NULL)
        obj->setSW();
}

void TpSetMatchModeValuesCmdIf::setAccuracyCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpSetMatchModeValuesCmdIf *obj;
    obj = (TpSetMatchModeValuesCmdIf *)clientData;
    if (obj != NULL)
        obj->setAccuracy();
}

void TpSetMatchModeValuesCmdIf::setThresholdCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpSetMatchModeValuesCmdIf *obj;
    obj = (TpSetMatchModeValuesCmdIf *)clientData;
    if (obj != NULL)
        obj->setThreshold();
}

void TpSetMatchModeValuesCmdIf::setPMK()
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_cmd->getValue();
    value->_pmk = atoi(_pmk->getFieldValue());

    runCmd(value);
}

void TpSetMatchModeValuesCmdIf::setLSM()
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_cmd->getValue();
    value->_lsm = atoi(_lsm->getFieldValue());

    runCmd(value);
}

void TpSetMatchModeValuesCmdIf::setSW()
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_cmd->getValue();
    value->_searchWindow = atoi(_sw->getFieldValue());

    runCmd(value);
}

void TpSetMatchModeValuesCmdIf::setAccuracy()
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_cmd->getValue();
    value->_accuracy = atof(_accuracy->getFieldValue());

    runCmd(value);
}

void TpSetMatchModeValuesCmdIf::setThreshold()
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_cmd->getValue();
    value->_threshold = atof(_threshold->getFieldValue());

    runCmd(value);
}

void TpSetMatchModeValuesCmdIf::setValue(CmdValue value)
{
    TpMatchModeValues *v;
    v = (TpMatchModeValues *)value;

    char buf[16];
    
    sprintf(buf, "%d", v->_pmk);
    _pmk->setFieldValue(buf);

    sprintf(buf, "%d", v->_lsm);
    _lsm->setFieldValue(buf);

    sprintf(buf, "%d", v->_searchWindow);
    _sw->setFieldValue(buf);

    sprintf(buf, "%.2f", v->_accuracy);
    _accuracy->setFieldValue(buf);

    sprintf(buf, "%.2f", v->_threshold);
    _threshold->setFieldValue(buf);
}
