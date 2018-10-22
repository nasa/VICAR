//////////////////////////////////////////////////////////////
// StretchListCmdInterface.cc: Cmd interface to StretchCmd.  Fills in 
// StretchValue and executes the command.  Intended to be used for 
// table stretches.
//////////////////////////////////////////////////////////////
#include "StretchListInterface.h"
#include "StretchParmList.h"
#include "StretchValue.h"
#include "CurListValue.h"
#include "ListControl.h"
#include "TableValue.h"
#include "Cmd.h"
#include <Xm/Form.h>
#include <iostream>
using namespace std;
#include <stdio.h>
			
StretchListInterface::StretchListInterface(Widget parent, Cmd *cmd, 
				       StretchType stretchType, int defValue) 
                                                                // = 0
    : CmdInterface(cmd)
{
    _stretchType = stretchType;
    _defValue = defValue;

    _w = XtVaCreateWidget(_name,
			  xmFormWidgetClass, parent,
			  NULL );
    installDestroyHandler();
    
    StretchParmList *list = NULL;
    CurListValue *curListInValue = NULL;
    CurListValue *curListOutValue = NULL;
    
    curListInValue = new CurListValue(_w, "curListInValue");
    curListOutValue = new CurListValue (_w, "curListOutValue", defValue);
    list = new StretchParmList(_w, "list", curListInValue, curListOutValue );
    _control = new ListControl(_w, "listControl", curListInValue, 
			       curListOutValue, list, this);
    
    XtVaSetValues(XtParent(list->baseWidget()),
		  XmNbottomAttachment, XmATTACH_WIDGET,
		  XmNbottomWidget,     curListOutValue->baseWidget(),
		  XmNleftAttachment,   XmATTACH_FORM,
		  XmNrightAttachment,  XmATTACH_FORM,
		  XmNtopAttachment,    XmATTACH_FORM,
		  NULL );
    
    XtVaSetValues(curListInValue->baseWidget(),
		  XmNbottomAttachment, XmATTACH_WIDGET,
		  XmNbottomWidget,     _control->baseWidget(),
		  XmNleftAttachment,   XmATTACH_FORM,
		  NULL );
    
    XtVaSetValues(curListOutValue->baseWidget(),
		  XmNbottomAttachment, XmATTACH_WIDGET,
		  XmNbottomWidget,     _control->baseWidget(),
		  // right attachment is set in the resource file
		  // to hide OUT value if necessary
		  NULL);
    
    XtVaSetValues(_control->baseWidget(),
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNleftAttachment,   XmATTACH_FORM,
		  XmNrightAttachment,  XmATTACH_FORM,
		  NULL );
    
    list->manage();
    curListInValue->manage();
    curListOutValue->manage();
    _control->manage();

    setValue(_cmd->getValue());
}

void StretchListInterface::runIt()
{
    TableValue *t = new TableValue;
    
    t->count = _control->getCount();
    t->inTable = new int[t->count];
    t->outTable = new int[t->count];
    for (int i = 0; i < t->count; i++) {
        t->inTable[i] = _control->getInAsArray()[i];
        t->outTable[i] = _control->getOutAsArray()[i];
    }
    
    StretchValue *stretchValue;
    stretchValue = new StretchValue(*((StretchValue *)_cmd->getValue()));
    switch(_stretchType) {
    case (ITABLE):
	stretchValue->inITable = t->inTable;
	stretchValue->outITable = t->outTable;
	stretchValue->itableNoVals = t->count;
	break;
    case (TABLE):
        stretchValue->inTable = t->inTable;
        stretchValue->outTable = t->outTable;
        stretchValue->tableNoVals = t->count;
	break;
    case (ALARM):
        stretchValue->alarmValues = t->inTable;
        stretchValue->alarmNoVals = t->count;
	break;
    default:
	cerr << "runIt(): Wrong type!\n";
    }

    runCmd(stretchValue);
}

////////////////////////////////////////////////////////
// Update the interface to match a given value
////////////////////////////////////////////////////////
void StretchListInterface::setValue(CmdValue value)
{
    StretchValue *stretchValue = (StretchValue *)value;
    int i;
    TableValue *tableValue = new TableValue;
    switch(_stretchType) {
    case (ITABLE):
	tableValue->count = stretchValue->itableNoVals;
	tableValue->inTable = new int [tableValue->count];
	tableValue->outTable = new int [tableValue->count];
	for (i = 0; i < tableValue->count; i++) {
	    tableValue->inTable[i] = stretchValue->inITable[i];
	    tableValue->outTable[i] = stretchValue->outITable[i];
	}
	break;
    case (TABLE):
	tableValue->count = stretchValue->tableNoVals;
        tableValue->inTable = new int [tableValue->count];
        tableValue->outTable = new int [tableValue->count];
        for (i = 0; i < tableValue->count; i++) {
            tableValue->inTable[i] = stretchValue->inTable[i];
            tableValue->outTable[i] = stretchValue->outTable[i];
        }
	break;
    case (ALARM):
	tableValue->count = stretchValue->alarmNoVals;
	tableValue->inTable = new int [tableValue->count];
	tableValue->outTable = new int [tableValue->count];
	for (i = 0; i < tableValue->count; i++) {
	    tableValue->inTable[i] = stretchValue->alarmValues[i];
	    tableValue->outTable[i] = _defValue;
	}
	break;
    default:
	cerr << "setValue(): Wrong type!\n";
    }

    // Do not delete tableValue after set call!

    _control->setTableValue(tableValue);
}

