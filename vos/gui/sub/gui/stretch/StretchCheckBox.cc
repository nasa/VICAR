///////////////////////////////////////////////////////
// StretchCheckBox.cc: On stretch interface, checkboxes are 
// used for turning post-stretch functions on/off.
///////////////////////////////////////////////////////
#include "StretchCheckBox.h"
#include "StretchCmdInterface.h"
#include <Xm/ToggleB.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////
StretchCheckBox::StretchCheckBox(Widget parent, const char *name,
				 StretchType type, 
				 StretchCmdInterface *stretchCmdInterface)
    : UIComponent(name)
{ 
    _type = type;
    _stretchCmdInterface = stretchCmdInterface;
    
    _w = XtVaCreateWidget(_name, xmToggleButtonWidgetClass, parent,
			  XmNset, False,
			  NULL);
    installDestroyHandler();
    
    XtAddCallback(_w, XmNvalueChangedCallback,
		  &StretchCheckBox::valueChangedCallback, (XtPointer)this);
}

////////////////////////////////////////////////////////////////////////
// Callback function
////////////////////////////////////////////////////////////////////////
void StretchCheckBox::valueChangedCallback(Widget,
		    XtPointer clientData, XtPointer)
{
    StretchCheckBox *obj = (StretchCheckBox *)clientData;
    
    obj->valueChanged();
}

////////////////////////////////////////////////////////////////////////
// Fill the stretch value structure, then execute the stretch
////////////////////////////////////////////////////////////////////////
void StretchCheckBox::valueChanged()
{
    int value = XmToggleButtonGetState(_w);

    if (value) {
	if (_type == ALARM) _stretchCmdInterface->setAlarmOn(True);
	if (_type == COMP) _stretchCmdInterface->setComplOn(True);
	if (_type == TABLE) _stretchCmdInterface->setTableOn(True);
	if (_type == OFF) _stretchCmdInterface->setOffOn(True);
   }
   else {
	if (_type == ALARM) _stretchCmdInterface->setAlarmOn(False);
	if (_type == COMP) _stretchCmdInterface->setComplOn(False);
	if (_type == TABLE) _stretchCmdInterface->setTableOn(False);
	if (_type == OFF) _stretchCmdInterface->setOffOn(False);
   }
   _stretchCmdInterface->setSensitivity(_type);
   _stretchCmdInterface->stretchIt();
}

////////////////////////////////////////////////////////////////////////
// Update the radio bank to reflect an externally-set value
// We don't set the sensitivity because the caller does
////////////////////////////////////////////////////////////////////////
void StretchCheckBox::setValue(Boolean on)
{
   if (on)
      XmToggleButtonSetState(_w, True, False);
   else
      XmToggleButtonSetState(_w, False, False);
}
