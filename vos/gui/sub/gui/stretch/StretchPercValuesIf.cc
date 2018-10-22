//////////////////////////////////////////////////////////////
// StretchPercValuesIf.cc: Shows current percent stretch limits.
//////////////////////////////////////////////////////////////
#include "StretchPercValuesIf.h"
#include "StretchValue.h"
#include "KeyinView.h"
#include "Cmd.h"
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <iostream>
using namespace std;
#include <stdio.h>
#include "xvmaininc.h"		// for UNUSED

StretchPercValuesIf::StretchPercValuesIf(Widget parent, Cmd *cmd)
    : CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name,
			  xmFormWidgetClass, parent,
			  NULL );
    installDestroyHandler(); 

    Widget mainLabel = XtVaCreateManagedWidget("Percent Stretch Values",
			  xmLabelWidgetClass, _w, 
			  XmNtopAttachment, XmATTACH_FORM,
			  XmNleftAttachment, XmATTACH_FORM,
			  XmNrightAttachment, XmATTACH_FORM,
			  NULL);

    Widget subForm = XtVaCreateManagedWidget("Sub Form",
			 xmFormWidgetClass, _w,
			 XmNtopAttachment, XmATTACH_WIDGET,
			 XmNtopWidget, mainLabel,
			 XmNbottomAttachment, XmATTACH_FORM,
			 NULL);

    Widget lowRowCol = XtVaCreateManagedWidget("Low Row Col",
				        xmRowColumnWidgetClass, subForm ,
					XmNtopAttachment, XmATTACH_FORM,
				 	XmNpacking, XmPACK_COLUMN, 
					XmNnumColumns, 4,
					XmNorientation, XmHORIZONTAL,
					XmNisAligned, True,
					XmNentryAlignment, XmALIGNMENT_END,
					NULL);

    Widget highRowCol = XtVaCreateManagedWidget("High Row Col",
					 xmRowColumnWidgetClass, subForm ,
					 XmNleftAttachment, XmATTACH_WIDGET,
					 XmNleftWidget, lowRowCol,
					 XmNtopAttachment, XmATTACH_FORM  ,
					 XmNpacking, XmPACK_COLUMN, 
					 XmNnumColumns, 4,
					 XmNorientation, XmHORIZONTAL,
					 XmNisAligned, True,
					 XmNentryAlignment, XmALIGNMENT_END,
					 NULL);

    Widget UNUSED(lowLabel) = XtVaCreateManagedWidget("Low Label",
					      xmLabelWidgetClass, lowRowCol,
					      NULL);

    Widget UNUSED(highLabel) = XtVaCreateManagedWidget("High Label",
					       xmLabelWidgetClass, highRowCol,
					       NULL);
					
    _lPercValueRed = new KeyinView(lowRowCol, "lPercValueRed");
    _lPercValueGrn = new KeyinView(lowRowCol, "lPercValueGrn");
    _lPercValueBlu = new KeyinView(lowRowCol, "lPercValueBlu");

    _hPercValueRed = new KeyinView(highRowCol, "hPercValueRed");
    _hPercValueGrn = new KeyinView(highRowCol, "hPercValueGrn");
    _hPercValueBlu = new KeyinView(highRowCol, "hPercValueBlu");

    _lPercValueRed->manage();
    _lPercValueGrn->manage();
    _lPercValueBlu->manage();
    _hPercValueRed->manage();
    _hPercValueGrn->manage();
    _hPercValueBlu->manage();

    setValue(_cmd->getValue());

}

////////////////////////////////////////////////////////
// Update the interface to match a given value
////////////////////////////////////////////////////////
void StretchPercValuesIf::setValue(CmdValue value)
{
    StretchValue *stretchValue = (StretchValue *)(_cmd->getValue());

    char buf[16];

    sprintf(buf, "%f", stretchValue->lPercValueRed);
    _lPercValueRed->setFieldValue(buf);

    sprintf(buf, "%f", stretchValue->lPercValueGrn);
    _lPercValueGrn->setFieldValue(buf);

    sprintf(buf, "%f", stretchValue->lPercValueBlu);
    _lPercValueBlu->setFieldValue(buf);

    sprintf(buf, "%f", stretchValue->hPercValueRed);
    _hPercValueRed->setFieldValue(buf);
 
    sprintf(buf, "%f", stretchValue->hPercValueGrn);
    _hPercValueGrn->setFieldValue(buf);
 
    sprintf(buf, "%f", stretchValue->hPercValueBlu);
    _hPercValueBlu->setFieldValue(buf);
}

