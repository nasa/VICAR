////////////////////////////////////////////////////////////
// TpMatchModeDialog.h: Dialog containing display mode values.
////////////////////////////////////////////////////////////
#include "TpMatchModeDialog.h"
#include "TpMatchManager.h"
#include "TpSetMatchModeValuesCmd.h"
#include "TpMatchModeValuesCmdIf.h"
#include "RadioCmdBox.h"
#include "CmdList.h"
#include "TpDefs.h"
#include "CmdList.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <stdio.h>

TpMatchModeDialog::TpMatchModeDialog(const char *name, TpMatchManager *mm, 
				     CmdList *modeRadioList)
	: CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{
    _matchManager = mm;
    _modeRadioList = modeRadioList;
}

Widget TpMatchModeDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("workArea", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 XmNnumColumns, 1,
				 NULL);

    XtVaCreateManagedWidget("matchModeLabel", 
			    xmLabelWidgetClass, rc, 
			    NULL);

    UIComponent *matchModeOptionMenu = new RadioCmdBox(rc, "match", 
						       _modeRadioList, 
						       _applyCmdList);
    matchModeOptionMenu->manage();

    CmdValue mmValue = (CmdValue)_matchManager->getMatchModeValues();
    _setMatchModeValuesCmd = new TpSetMatchModeValuesCmd(
	"SetMatchModeValuesCmd", True, mmValue, _matchManager);
    CmdInterface *setMatchModeValuesCmdIf = new TpSetMatchModeValuesCmdIf(
	rc, _setMatchModeValuesCmd);
    setMatchModeValuesCmdIf->setDeferredExec(_applyCmdList);
    setMatchModeValuesCmdIf->manage();

    return rc;
}

