//////////////////////////////////////////////////////////////////////////////
// TpQualFormatSingleView.cc
//////////////////////////////////////////////////////////////////////////////
#include "TpQualFormatSingleView.h"
#include "TpQualFormatView.h"
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include <stdio.h>

TpQualFormatSingleView::TpQualFormatSingleView(Widget parent, const char *name, 
				   TpQualFormatView *view, int n)
    : UIComponent(name)
{
    _view = view;
    _n = n;
    
    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent, 
			  XmNorientation, XmHORIZONTAL,
			  XmNnumColumns, 1,
			  NULL);
    installDestroyHandler();

    _wname = XtVaCreateManagedWidget("name", xmTextFieldWidgetClass, _w, NULL);
    XtAddCallback(_wname, XmNlosingFocusCallback, 
		  &TpQualFormatSingleView::setNameCallback,
		  (XtPointer)this);
    XtAddCallback(_wname, XmNactivateCallback, 
		  &TpQualFormatSingleView::setNameCallback,
                  (XtPointer)this);

    Widget radioBox = XtVaCreateManagedWidget("qualFormatRadioBox", 
		xmRowColumnWidgetClass, _w,
                XmNorientation, XmHORIZONTAL,
                XmNradioBehavior, True,
                XmNradioAlwaysOne, True,
                NULL );
    _wreal = XtVaCreateManagedWidget("real", 
                xmToggleButtonWidgetClass, radioBox,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, True,
                NULL);
    _wfull = XtVaCreateManagedWidget("full",
                xmToggleButtonWidgetClass, radioBox,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);
    _wtext = XtVaCreateManagedWidget("text",
                xmToggleButtonWidgetClass, radioBox,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);

    XtAddCallback(_wreal, XmNvalueChangedCallback, 
		  &TpQualFormatSingleView::setTypeCallback, 
		  (XtPointer)this);
    XtAddCallback(_wfull, XmNvalueChangedCallback, 
                  &TpQualFormatSingleView::setTypeCallback,
                  (XtPointer)this);
    XtAddCallback(_wtext, XmNvalueChangedCallback, 
                  &TpQualFormatSingleView::setTypeCallback,
                  (XtPointer)this);

    _wunit = XtVaCreateManagedWidget("unit", xmTextFieldWidgetClass, _w, NULL);
    XtAddCallback(_wunit, XmNlosingFocusCallback,
                  &TpQualFormatSingleView::setUnitCallback,
                  (XtPointer)this);
    XtAddCallback(_wunit, XmNactivateCallback,
                  &TpQualFormatSingleView::setUnitCallback,
                  (XtPointer)this);
}

void TpQualFormatSingleView::setNameCallback(Widget w, 
					     XtPointer clientData, XtPointer)
{
    TpQualFormatSingleView *obj;
    obj = (TpQualFormatSingleView *)clientData;
    if (obj != NULL)
	obj->setName(XmTextFieldGetString(w));
}

void TpQualFormatSingleView::setUnitCallback(Widget w, 
                                             XtPointer clientData, XtPointer)
{
    TpQualFormatSingleView *obj;
    obj = (TpQualFormatSingleView *)clientData;
    if (obj != NULL)
        obj->setUnit(XmTextFieldGetString(w));
}

void TpQualFormatSingleView::setTypeCallback(Widget,
					     XtPointer clientData, XtPointer)
{
    TpQualFormatSingleView *obj;
    obj = (TpQualFormatSingleView *)clientData;
    if (obj != NULL)
	obj->setType();
}

void TpQualFormatSingleView::setName(char *name, Boolean doUpdate)
{
    XmTextFieldSetString(_wname, name);
    if (doUpdate)
	_view->setName(name, _n);
}

void TpQualFormatSingleView::setUnit(char *unit, Boolean doUpdate)
{
    XmTextFieldSetString(_wunit, unit);
    if (doUpdate)
	_view->setUnit(unit, _n);
}

// Process value that came from outside (no view update necessary)

void TpQualFormatSingleView::setType(TpQualType type)
{
    if (type == TpReal)
	XmToggleButtonSetState(_wreal, True, False);
    else
	XmToggleButtonSetState(_wreal, False, False);

    if (type == TpFull)
        XmToggleButtonSetState(_wfull, True, False);
    else
        XmToggleButtonSetState(_wfull, False, False);

    if (type == TpText)
        XmToggleButtonSetState(_wtext, True, False);
    else
        XmToggleButtonSetState(_wtext, False, False);
}

// Process user input

void TpQualFormatSingleView::setType()
{
    if (XmToggleButtonGetState(_wreal)) {
	_view->setType(TpReal, _n);
    }
    if (XmToggleButtonGetState(_wfull)) {
        _view->setType(TpFull, _n);
    }
    if (XmToggleButtonGetState(_wtext)) {
        _view->setType(TpText, _n);
    }
}
