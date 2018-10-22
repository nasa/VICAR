////////////////////////////////////////////////////////////
// TpAutofindDialog.h: Dialog containing display mode values.
////////////////////////////////////////////////////////////
#include "TpAutofindDialog.h"
#include "TpMatchManager.h"
#include "RadioCmdBox.h"
#include "CmdList.h"
#include "TpDefs.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <stdio.h>

TpAutofindDialog::TpAutofindDialog(const char *name, TpMatchManager *mm, 
				   CmdList *findRadioList)
    : CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{
    _matchManager = mm;
    _findRadioList = findRadioList;
}

Widget TpAutofindDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("workArea", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 XmNnumColumns, 1,
				 NULL);

    XtVaCreateManagedWidget("autofindLabel", 
			    xmLabelWidgetClass, rc, 
			    NULL);

    ////////////////////////////////////////////////////////////////
    // Radio box for specifying autofind mode
    ///////////////////////////////////////////////////////////////

    UIComponent *optMenu = new RadioCmdBox(rc, "autofindMode", 
					   _findRadioList, _applyCmdList);
    optMenu->manage();

    return rc;
}
