//////////////////////////////////////////////////////////////
// RadioButtonInterface.cc: A radio button (1 of many) interface to a
// Cmd object.  The value is either TRUE or FALSE.  Two differences
// from CheckBox: the type of indicator used, and the user can only turn
// this on (it is assumed that setValue will turn it off).
///////////////////////////////////////////////////////////////
#include "RadioButtonInterface.h"
#include "CmdList.h"
#include <Xm/ToggleB.h>

RadioButtonInterface::RadioButtonInterface ( Widget parent, Cmd *cmd )
		: CheckBoxInterface ( parent, cmd )
{
    XtVaSetValues(_w, XmNindicatorType, XmONE_OF_MANY, NULL);
}

RadioButtonInterface::RadioButtonInterface ( Widget parent, const char *name )
		: CheckBoxInterface ( parent, name )
{
    XtVaSetValues(_w, XmNindicatorType, XmONE_OF_MANY, NULL);
}

void RadioButtonInterface::executeCmd ( XtPointer )
{
    Boolean value;

    value = XmToggleButtonGetState(_w);

    if (value)
        runCmd((CmdValue) (int)value);	// User turned it on
    else {
        if (_deferredList) {
            // Let normal Motif radio behavior operate
            if (_cmd)
                _deferredList->remove(_cmd);
        }
        else {
            // Don't let the user turn it back off
            XmToggleButtonSetState(_w, True, False);
        }
    }
}

