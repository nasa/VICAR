//////////////////////////////////////////////////////////////////////////////
// TpAddIdAsGenQualInterface.cc:  
//////////////////////////////////////////////////////////////////////////////
#include "TpAddIdAsGenQualInterface.h"
#include "TpQualFormatView.h"
#include "TpQualFormatValue.h"
#include "TpDefs.h"
#include "Cmd.h"
#include <Xm/PushB.h>
#include <stdlib.h>
#include <stdio.h>

TpAddIdAsGenQualInterface::TpAddIdAsGenQualInterface(Widget parent, Cmd *cmd) 
	: CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name, xmPushButtonWidgetClass, parent, NULL);
    installDestroyHandler();

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
 
    if (_active)
        activate();
    else
        deactivate();

    XtAddCallback(_w, XmNactivateCallback,
	&TpAddIdAsGenQualInterface::addIdAsGenQualCallback, (XtPointer)this);

   _value = new TpQualFormatValue(*((TpQualFormatValue *)_cmd->getValue())); 
}

void TpAddIdAsGenQualInterface::addIdAsGenQualCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpAddIdAsGenQualInterface *obj;
    obj = (TpAddIdAsGenQualInterface *)clientData;
    if (obj != NULL)
        obj->addIdAsGenQual();
}

void TpAddIdAsGenQualInterface::addIdAsGenQual()
{
    if (_value->getNumQuals() == 0) {
    	_value->addQualInfo("ID", TpFull, "None");
        runCmd((CmdValue)_value);
    }
}

void TpAddIdAsGenQualInterface::setValue(CmdValue v)
{
    _value = new TpQualFormatValue(*((TpQualFormatValue *)v));
}
