///////////////////////////////////////////////////////
// StretchBandChooser.cc: A component class to choose which band will be
// stretched.  The choices are Red, Green, Blue, All.  Only one option
// can be chosen.  The other should be stored.  If the user decides to 
// stretch another band, then the stored verstion becomes active.
///////////////////////////////////////////////////////
#include "StretchBandChooser.h"
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <iostream>
using namespace std;

StretchBandChooser::StretchBandChooser(Widget parent, Cmd *cmd )
    : CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent,
		XmNorientation, XmHORIZONTAL,
                XmNradioBehavior, True,
                XmNradioAlwaysOne, True,
		NULL );
    installDestroyHandler();
    
    _all = XtVaCreateManagedWidget("all", xmToggleButtonWidgetClass, _w,
		XmNindicatorType, XmONE_OF_MANY,
		XmNset, True,
		NULL);
    _red = XtVaCreateManagedWidget("red", xmToggleButtonWidgetClass, _w,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);
    _grn = XtVaCreateManagedWidget("green", xmToggleButtonWidgetClass, _w,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);
    _blu = XtVaCreateManagedWidget("blue", xmToggleButtonWidgetClass, _w,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);

    XtAddCallback(_all, XmNvalueChangedCallback,
		  &StretchBandChooser::valueChangedCallback, (XtPointer)this);
    XtAddCallback(_red, XmNvalueChangedCallback,
		  &StretchBandChooser::valueChangedCallback, (XtPointer)this);
    XtAddCallback(_grn, XmNvalueChangedCallback,
		  &StretchBandChooser::valueChangedCallback, (XtPointer)this);
    XtAddCallback(_blu, XmNvalueChangedCallback,
		 &StretchBandChooser::valueChangedCallback, (XtPointer)this);
}

////////////////////////////////////////////////////////////////////////
// Callback function
////////////////////////////////////////////////////////////////////////
void StretchBandChooser::valueChangedCallback(Widget,
				XtPointer clientData, XtPointer)
{
    StretchBandChooser *obj = (StretchBandChooser *)clientData;
    
    obj->valueChanged();
}

////////////////////////////////////////////////////////////////////////
// Fill the stretch value structure, then execute the stretch
////////////////////////////////////////////////////////////////////////
void StretchBandChooser::valueChanged()
{
    StretchValue *stretchValue = new StretchValue;
    stretchValue->changeOnlyBand = True;

    if (XmToggleButtonGetState(_all)) {
	stretchValue->band = STR_ALL;
    }
    
    if (XmToggleButtonGetState(_red)) {
        stretchValue->band = STR_RED;
    }
    
    if (XmToggleButtonGetState(_grn)) {
        stretchValue->band = STR_GREEN;
    }
    
    if (XmToggleButtonGetState(_blu)) {
        stretchValue->band = STR_BLUE;
    }

    runCmd(stretchValue);
}

////////////////////////////////////////////////////////////////////////
// Update the radio bank to reflect an externally-set value
// We don't set the sensitivity because the caller does
////////////////////////////////////////////////////////////////////////
void StretchBandChooser::setValue(CmdValue value)
{
    StretchValue *stretchValue = (StretchValue *)value;
    StretchBand band = stretchValue->band;

    if (band == STR_ALL)			// We're it
	XmToggleButtonSetState(_all, True, False);
    else					// We're not it
	XmToggleButtonSetState(_all, False, False);

    if (band == STR_RED)                     // We're it
	XmToggleButtonSetState(_red, True, False);
    else                                 // We're not it
	XmToggleButtonSetState(_red, False, False);
    
    if (band == STR_GREEN)                     // We're it
	XmToggleButtonSetState(_grn, True, False);
    else                                 // We're not it
	XmToggleButtonSetState(_grn, False, False);
    
    if (band == STR_BLUE)                     // We're it
	XmToggleButtonSetState(_blu, True, False);
    else                                 // We're not it
	XmToggleButtonSetState(_blu, False, False);
}
