//////////////////////////////////////////////////////////////////////////////
// TpAddCorrParmAsPntQualInterface.cc:  
//////////////////////////////////////////////////////////////////////////////
#include "TpAddCorrParmAsPntQualInterface.h"
#include "TpQualFormatView.h"
#include "TpQualFormatValue.h"
#include "TpDefs.h"
#include "Cmd.h"
#include <Xm/PushB.h>
#include <stdlib.h>
#include <stdio.h>

TpAddCorrParmAsPntQualInterface::TpAddCorrParmAsPntQualInterface(Widget parent, 
					Cmd *cmd) 
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
	&TpAddCorrParmAsPntQualInterface::addCorrParmAsPntQualCallback, 
	(XtPointer)this);

   _value = new TpQualFormatValue(*((TpQualFormatValue *)_cmd->getValue())); 
}

void TpAddCorrParmAsPntQualInterface::addCorrParmAsPntQualCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpAddCorrParmAsPntQualInterface *obj;
    obj = (TpAddCorrParmAsPntQualInterface *)clientData;
    if (obj != NULL)
        obj->addCorrParmAsPntQual();
}

void TpAddCorrParmAsPntQualInterface::addCorrParmAsPntQual()
{
    if (_value->getNumQuals() == 0) {
    	_value->addQualInfo("Corr_Parm", TpReal, "None");
        runCmd((CmdValue)_value);
    }
}

void TpAddCorrParmAsPntQualInterface::setValue(CmdValue v)
{
    _value = new TpQualFormatValue(*((TpQualFormatValue *)v));
}
