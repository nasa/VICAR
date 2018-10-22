///////////////////////////////////////////////////////
// StretchRadioBtn.cc: A component class to show a Stretch type radio button
///////////////////////////////////////////////////////
#include "StretchRadioBtn.h"
#include "StretchCmdInterface.h"
#include <Xm/ToggleB.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////
StretchRadioBtn::StretchRadioBtn(Widget parent, const char *name,
		StretchType type,
		StretchCmdInterface * stretchCmdInterface)
	: UIComponent(name)
{ 
    _type = type;
    _stretchCmdInterface = stretchCmdInterface;

    _w = XtVaCreateWidget(_name, xmToggleButtonWidgetClass, parent,
		XmNindicatorType, XmONE_OF_MANY,
		XmNset, False,
		NULL);
    installDestroyHandler();

    XtAddCallback(_w, XmNvalueChangedCallback,
		  &StretchRadioBtn::valueChangedCallback, (XtPointer)this);
}

////////////////////////////////////////////////////////////////////////
// Callback function
////////////////////////////////////////////////////////////////////////
void StretchRadioBtn::valueChangedCallback(Widget,
				XtPointer clientData, XtPointer)
{
    StretchRadioBtn *obj = (StretchRadioBtn *)clientData;
    
    obj->valueChanged();
}

////////////////////////////////////////////////////////////////////////
// Fill the stretch value structure, then execute the stretch
////////////////////////////////////////////////////////////////////////
void StretchRadioBtn::valueChanged()
{
    int value;

    value = XmToggleButtonGetState(_w);

    if (value) {
	_stretchCmdInterface->setType(_type);
	_stretchCmdInterface->setSensitivity(_type);
	_stretchCmdInterface->stretchIt();
    }
}

////////////////////////////////////////////////////////////////////////
// Update the radio bank to reflect an externally-set value
// We don't set the sensitivity because the caller does
////////////////////////////////////////////////////////////////////////
void StretchRadioBtn::setValue(StretchType type)
{
    if (type == _type)			// We're it
	XmToggleButtonSetState(_w, True, False);
    else				// We're not it
	XmToggleButtonSetState(_w, False, False);
}

