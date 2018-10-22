///////////////////////////////////////////////////////////////////
// TpMatchBrowseControl.cc: This component displays general qualifier 
// information and allows user to change it.  It also allows user to 
// select tiepoints by typing the point sequence number.
///////////////////////////////////////////////////////////////////
#include "TpMatchBrowseControl.h"
#include "TpSelectionMgr.h"
#include "TpQualGroup.h"
#include "TpQualGroupMgr.h"
#include "TpMatchManager.h"
#include "QuestionDialogManager.h"
#include "Application.h"
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/ArrowB.h>
#include <Xm/Label.h>
#include <assert.h>
#include <stdio.h>

XtResource TpMatchBrowseControl::_resources[] = {
  {
    (char *)"enableGotoQual",
    (char *)"EnableGotoQual",
    XmRBoolean,
    sizeof(Boolean),
    XtOffsetOf(TpMatchBrowseControl, _enableGotoQual),
    XmRImmediate,
    (XtPointer) False,
  },
};

String TpMatchBrowseControl::_defaults[] = {
    (char *)"*number.columns: 3",
    (char *)"*value.columns: 21", 
    (char *)"*value.maxLength: 255",
    (char *)"*prev.arrowDirection: ARROW_DOWN",
    (char *)"*next.arrowDirection: ARROW_UP",
    NULL,
};

int _displayed = 0; //!!!

TpMatchBrowseControl::TpMatchBrowseControl(Widget parent, const char *name, 
			TpSelectionMgr *sm, TpMatchManager *mm)
    : UIComponent(name)
{
    _selMgr = sm;
    _matchManager = mm;
    _numberInt = 0;

    setDefaultResources(parent, _defaults);

    XtGetApplicationResources(theApplication->baseWidget(), (XtPointer)this,
			_resources, XtNumber(_resources), NULL, 0);

    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent,
                          XmNshadowThickness, 3,
                          NULL);
    installDestroyHandler();

    Widget rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, _w,
					XmNorientation, XmHORIZONTAL,
					NULL);

    _numberField = XtVaCreateManagedWidget("number", 
					   xmTextFieldWidgetClass, rc, 
					   NULL);
    _valueField = XtVaCreateManagedWidget("value",
					  xmTextFieldWidgetClass, rc, 
					  NULL);

    // This field shows the qualifier like _valueField does, but when typed
    // in, instead of changing the value it selects ("goes to") the first
    // tiepoint found with that qualifier.  Or does nothing if not found.

    if (_enableGotoQual) {
	_gotoLabel = XtVaCreateManagedWidget("gotoLabel",
					xmLabelWidgetClass, rc, NULL);
	_gotoField = XtVaCreateManagedWidget("gotoField",
					xmTextFieldWidgetClass, rc, NULL);
    }

    Widget prev = XtVaCreateManagedWidget("prev", 
					  xmArrowButtonWidgetClass, rc,
					  NULL);
    Widget next = XtVaCreateManagedWidget("next", 
                                          xmArrowButtonWidgetClass, rc,
                                          NULL);

    // Declare all necessary callbacks

    XtAddCallback(_numberField, XmNactivateCallback,
                  &TpMatchBrowseControl::setNumberCallback, 
		  (XtPointer)this);

    XtAddCallback(_valueField, XmNactivateCallback, 
                  &TpMatchBrowseControl::setValueCallback, 
		  (XtPointer)this);

    if (_enableGotoQual) {
	XtAddCallback (_gotoField, XmNactivateCallback,
		  &TpMatchBrowseControl::gotoQualCallback,
		  (XtPointer)this);
    }

    XtAddCallback(prev, XmNactivateCallback, 
		  &TpMatchBrowseControl::decNumberCallback, 
		  (XtPointer)this);
    XtAddCallback(next, XmNactivateCallback, 
                  &TpMatchBrowseControl::incNumberCallback, 
		  (XtPointer)this);
}

void TpMatchBrowseControl::setNumberCallback(Widget, XtPointer clientData,
					     XtPointer)
{
    TpMatchBrowseControl *obj = (TpMatchBrowseControl *)clientData;
    obj->setNumber();
}

void TpMatchBrowseControl::setValueCallback(Widget, XtPointer clientData,
					    XtPointer)
{
    TpMatchBrowseControl *obj = (TpMatchBrowseControl *)clientData;
    obj->setValue(False);
}

void TpMatchBrowseControl::gotoQualCallback(Widget, XtPointer clientData,
					    XtPointer)
{
    TpMatchBrowseControl *obj = (TpMatchBrowseControl *)clientData;
    obj->gotoQual();
}

void TpMatchBrowseControl::confirm(void *clientData)
{
    TpMatchBrowseControl *obj = (TpMatchBrowseControl *)clientData;
    obj->setValue(True);
}

void TpMatchBrowseControl::incNumberCallback(Widget, XtPointer clientData,
					     XtPointer)
{
    TpMatchBrowseControl *obj = (TpMatchBrowseControl *)clientData;
    obj->incNumber();
}

void TpMatchBrowseControl::decNumberCallback(Widget, XtPointer clientData,
					     XtPointer)
{
    TpMatchBrowseControl *obj = (TpMatchBrowseControl *)clientData;
    obj->decNumber();
}

/////////////////////////////////////////////////////////////////////////////
// Execute this on user typing a new running id
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::setNumber()
{
    // Get the desired match number provided by the user

    char *numberString = XmTextFieldGetString(_numberField);
    _numberInt = atoi(numberString);

    setNumber(_numberInt);
}

/////////////////////////////////////////////////////////////////////////////
// Given the new running id, check it first, then select new tiepoint and
// display correspoding general qualifier value.
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::setNumber(int num)
{
    _numberInt = num;
    if (_matchManager->numMatches() == 0)
	_numberInt = 0;

    setNumberField((char *)"");

    if (_numberInt > _matchManager->numMatches())
        _numberInt = _matchManager->numMatches();
    else if ((_numberInt <= 0) && (_matchManager->numMatches() > 0))
        _numberInt = 1;
    
    TpMatch *match = _matchManager->getNthMatch(_numberInt);
    if (match)
	_selMgr->selectMatchAndPoints(match);
    
    char buf[16];
    sprintf(buf, "%d", _numberInt);
    setNumberField(buf);

    displayValue();
}

/////////////////////////////////////////////////////////////////////////////
// Execute this on user typing into the Goto Qual box
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::gotoQual()
{
    // Get the desired qual value provided by the user

    char *gotoString = XmTextFieldGetString(_gotoField);

    gotoQual(gotoString);
}

/////////////////////////////////////////////////////////////////////////////
// Given a general qualifier value, find the first tiepoint that has that
// qual value and select the corresponding tiepoint.
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::gotoQual(char *gotoString)
{

    // We could do this in a slicker way by using a matching function on
    // the SL_List to find the appropriate match.  But in the interests of
    // time, this simple brute-force method should work...  (rgd 6/2010)

    int nmatch = _matchManager->numMatches();

    for (int i=1; i <= nmatch; i++) {
	TpMatch *match = _matchManager->getNthMatch(i);
	if (match == NULL) continue;

        TpQualGroup *genQual = match->getGenQual();
	if (genQual == NULL) continue;

        if (genQual->getNumQuals() > _displayed) {
	    char *buf = genQual->valueToString(_displayed);
	    if (strcmp(buf, gotoString) == 0) {		// found it!
		setNumber(i);
		if (buf) delete [] buf;
		return;
	    }
	    if (buf) delete [] buf;
	}
    }

    // It wasn't found, so issue an error... such as it is

    if (_enableGotoQual)
	XmTextFieldSetString(_gotoField, (char *)"UNK");
}

/////////////////////////////////////////////////////////////////////////////
// Find out general qualifier value and show it.
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::displayValue()
{
    // Get currently-selected match

    TpMatch *match = _selMgr->getMatch();
    if (match == NULL) {
	setValueField(NULL);
	return;
    }

    TpQualGroup *genQual = match->getGenQual();

    char *buf = NULL;
    if (genQual->getNumQuals() > _displayed) {
	buf = genQual->valueToString(_displayed);
    }
    else {
	buf = sdup("No qualifier defined");
    }
    setValueField(buf);
    if (buf) delete [] buf;
}

/////////////////////////////////////////////////////////////////////////////
// Execute this on user typing a new qualifier value.  Note that only one 
// general qualifier is supported right now, although TpQualGroup class
// has support for multiple qualifiers.  That is why setGenQual() function 
// always gets 1 as a second argument right now.  This should be changed 
// to the qualifier number when it will be possible to set more than one 
// qualifier. 
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::setValue(Boolean confirmed)
{
    char *valueString = XmTextFieldGetString(_valueField);

    TpMatch *match = _selMgr->getMatch();
    if (match == NULL) {
	setValueField(NULL);
	return;
    }

    TpQualGroup *genQual = match->getGenQual();

    if (genQual->getNumQuals() <= _displayed) {
	setValueField((char *)"No qualifier defined");
	return;
    }

    if (_matchManager->isCheckingGenQualUnique() && !confirmed) {
	if (!_matchManager->getGenQualMgr()->isUnique(genQual, _displayed, 
						      valueString)) {
	    char msg[] ="You are entering non-unique qualifier. Are you sure?";
	    theQuestionDialogManager->post(msg, (void *)this,
					   &TpMatchBrowseControl::confirm);
	    return;
	}
    }
    genQual->setValue(_displayed, valueString);
}

/////////////////////////////////////////////////////////////////////////////
// Execute this on user pressing increment by one button
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::incNumber()
{
    setNumber(_numberInt+1);
}

/////////////////////////////////////////////////////////////////////////////
// Execute this on user pressing decrement by one button
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::decNumber()
{
    setNumber(_numberInt-1);
}

/////////////////////////////////////////////////////////////////////////////
// Just a convenience function to set the running id text field.
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::setNumberField(char *text)
{
    if (text && strlen(text))
	XmTextFieldSetString(_numberField, text);
    else
	XmTextFieldSetString(_numberField, (char *)"");
}

/////////////////////////////////////////////////////////////////////////////
// Just a convenience function to set the qualifier value text field.
// Also sets the same text in the Goto field, if enabled.
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::setValueField(char *text)
{
    if (text) {
	XmTextFieldSetString(_valueField, text);
	if (_enableGotoQual)
	    XmTextFieldSetString(_gotoField, text);
    }
    else {
	XmTextFieldSetString(_valueField, (char *)"No Tiepoints Selected");
	if (_enableGotoQual)
	    XmTextFieldSetString(_gotoField, (char *)"");
    }
}

////////////////////////////////////////////////////////////////////////////
// This function gets called every time the user made a selection of a 
// different match.
////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::newMatchSelected(TpMatch *match)
{
    if (match == NULL) {
	setNumberField(NULL);
	setValueField(NULL);
	return;
    }

    _numberInt = _matchManager->getMatchNumber(match);
    char buf[16];
    sprintf(buf, "%d", _numberInt);
    setNumberField(buf);

    displayValue();
}

