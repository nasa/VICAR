///////////////////////////////////////////////////////
// InterpolationChooser.cc: A component class to choose the interpolation
// on the interval.  The choices are No Interpolation, 
// Flat, Linear, Cubic Spline.  Only one option can be chosen
///////////////////////////////////////////////////////
#include "InterpolationChooser.h"
#include "PseudoValue.h"
#include "PseudoMarks.h"
#include "PseudoCmdInterface.h"
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <stdio.h>

InterpolationChooser::InterpolationChooser(Widget parent, const char *name,
	InterpolationType type, PseudoValue *pseudoValue, PseudoMarks *marks,
	PseudoCmdInterface *pseudoCmdInterface)
	: UIComponent(name)
{
   _pseudoValue = pseudoValue;
   _pseudoMarks = marks;
   _type = type;
   _pseudoCmdInterface = pseudoCmdInterface;

   _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent,
		XmNorientation, XmHORIZONTAL,
                XmNradioBehavior, True,
                XmNradioAlwaysOne, True,
		NULL );
   installDestroyHandler();

   _none = XtVaCreateManagedWidget("none", xmToggleButtonWidgetClass, _w,
		XmNindicatorType, XmONE_OF_MANY,
		XmNset, True,
		NULL);
   _flat = XtVaCreateManagedWidget("flat", xmToggleButtonWidgetClass, _w,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);
   _linear = XtVaCreateManagedWidget("linear", xmToggleButtonWidgetClass, _w,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);
   _cubs = XtVaCreateManagedWidget("cubs", xmToggleButtonWidgetClass, _w,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);
   XtSetSensitive(_cubs, False);	//!!!! temporary!

   valueChanged();

   XtAddCallback(_none, XmNvalueChangedCallback,
		&InterpolationChooser::valueChangedCallback, (XtPointer)this);
   XtAddCallback(_flat, XmNvalueChangedCallback,
                &InterpolationChooser::valueChangedCallback, (XtPointer)this);
   XtAddCallback(_linear, XmNvalueChangedCallback,
                &InterpolationChooser::valueChangedCallback, (XtPointer)this);
   XtAddCallback(_cubs, XmNvalueChangedCallback,
                &InterpolationChooser::valueChangedCallback, (XtPointer)this);
}

////////////////////////////////////////////////////////////////////////
// Callback function
////////////////////////////////////////////////////////////////////////
void InterpolationChooser::valueChangedCallback(Widget,
				XtPointer clientData, XtPointer)
{
   InterpolationChooser *obj = (InterpolationChooser *)clientData;

   obj->valueChanged();
}

////////////////////////////////////////////////////////////////////////
// Fill the pseudocolor value structure, then execute the stretch
////////////////////////////////////////////////////////////////////////
void InterpolationChooser::valueChanged()
{
	// Set selected type for each of the mark on the interval
	if (XmToggleButtonGetState(_linear)) {
		_pseudoMarks->setInterpolation (LINEAR);
	}

	if (XmToggleButtonGetState(_flat)) {
		_pseudoMarks->setInterpolation (FLAT);
	}

	if (XmToggleButtonGetState(_none)) {
		_pseudoMarks->setInterpolation (NONE);
	}

	if (XmToggleButtonGetState(_cubs)) {
	   // Empty
	}

	_pseudoValue->regenerate(_pseudoMarks);

	_pseudoCmdInterface->loadTable(_pseudoValue);
}

////////////////////////////////////////////////////////////////////////
// Update the radio bank to reflect an externally-set value
// We don't set the sensitivity because the caller does
////////////////////////////////////////////////////////////////////////
void InterpolationChooser::setValue(InterpolationType type)
{
   if (type == NONE)			// We're it
      XmToggleButtonSetState(_none, True, False);
   else					// We're not it
      XmToggleButtonSetState(_none, False, False);

   if (type == FLAT)                     // We're it
      XmToggleButtonSetState(_flat, True, False);
   else                                 // We're not it
      XmToggleButtonSetState(_flat, False, False);

   if (type == LINEAR)                     // We're it
      XmToggleButtonSetState(_linear, True, False);
   else                                 // We're not it
      XmToggleButtonSetState(_linear, False, False);

   if (type == CUBS)                     // We're it
      XmToggleButtonSetState(_cubs, True, False);
   else                                 // We're not it
      XmToggleButtonSetState(_cubs, False, False);
}

void InterpolationChooser::update(PseudoMarks *marks)
{
   setValue(marks->getInterpolation());
}
