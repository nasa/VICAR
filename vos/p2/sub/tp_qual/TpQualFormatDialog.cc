/////////////////////////////////////////////////////////////////////////////
// TpQualFormatDialog.h: Dialog containing qualifier format values.
////////////////////////////////////////////////////////////////////////////
#include "TpQualFormatDialog.h"
#include "TpQualFormatCmd.h"
#include "TpSetMatchIdOffsetCmd.h"
#include "TpSetMatchIdNextCmd.h"
#include "TpQualFormatCmdInterface.h"
#include "TpAddIdAsGenQualInterface.h"
#include "TpAddCorrParmAsPntQualInterface.h"
#include "TpMatchManager.h"
#include "StringKeyinInterface.h"
#include "TpDefs.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <stdio.h>

TpQualFormatDialog::TpQualFormatDialog(const char *name, TpMatchManager *mm)
	: CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{
    _matchManager = mm;
    _cmdGen = NULL;
    _cmdPnt = NULL;
}

Widget TpQualFormatDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("workArea", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 XmNnumColumns, 1,
				 XmNpacking, XmPACK_TIGHT,
				 NULL);

    XtVaCreateManagedWidget("qualFormatLabel", 
			    xmLabelWidgetClass, rc, 
			    NULL);

    /////////////////////////////////////////////////////////////////////////
    // General Qualifier Part
    /////////////////////////////////////////////////////////////////////////

    _cmdGen = new TpQualFormatCmd("genQualFormatCmd", True, 
				  _matchManager->getGenQualMgr());
    Widget rc1 = XtVaCreateManagedWidget("rcGenQualExtra", 
			xmRowColumnWidgetClass, rc,
			XmNorientation, XmHORIZONTAL, 
			XmNnumColumns, 1, 
			XmNpacking, XmPACK_TIGHT,
			NULL); 
    CmdInterface *idAsGen = new TpAddIdAsGenQualInterface(rc1, _cmdGen);
    idAsGen->manage();

    XtVaCreateManagedWidget("offsetLabel", xmLabelWidgetClass, rc1, NULL);
    char buf[16];
    sprintf(buf, "%d", _matchManager->getStartId());
    Cmd *offsetCmd = new TpSetMatchIdOffsetCmd("setMatchIdOffset",
			True, (CmdValue)buf, _matchManager);
    CmdInterface *setMatchIdOffsetCmdIf = new StringKeyinInterface(rc1, offsetCmd);
    offsetCmd->newValue();
    setMatchIdOffsetCmdIf->manage();

    XtVaCreateManagedWidget("nextLabel", xmLabelWidgetClass, rc1, NULL);
    sprintf(buf, "%d", _matchManager->getStartId());
    Cmd *nextCmd = new TpSetMatchIdNextCmd("setMatchIdNext",
                        True, (CmdValue)buf, _matchManager);
    CmdInterface *setMatchIdNextCmdIf = new StringKeyinInterface(rc1, nextCmd);
    nextCmd->newValue();
    setMatchIdNextCmdIf->manage();

    CmdInterface *ciGen = new TpQualFormatCmdInterface(rc, _cmdGen);
    ciGen->setDeferredExec(_applyCmdList);
    ciGen->manage();

    /////////////////////////////////////////////////////////////////////////
    // Point Qualifier Part
    /////////////////////////////////////////////////////////////////////////

    _cmdPnt = new TpQualFormatCmd("pntQualFormatCmd", True,
                                      _matchManager->getPointQualMgr());
    Widget rc2 = XtVaCreateManagedWidget("rcPntQualExtra",
			xmRowColumnWidgetClass, rc,
			XmNorientation, XmHORIZONTAL,
			XmNnumColumns, 1,
			XmNpacking, XmPACK_TIGHT,
			NULL);
    CmdInterface *corParAsPnt = new TpAddCorrParmAsPntQualInterface(rc2, _cmdPnt);
    corParAsPnt->manage();

    CmdInterface *ciPnt = new TpQualFormatCmdInterface(rc, _cmdPnt);
    ciPnt->setDeferredExec(_applyCmdList);
    ciPnt->manage();
     
    return rc;
}

void TpQualFormatDialog::post()
{
    if (_cmdGen) _cmdGen->collectValue();
    if (_cmdPnt) _cmdPnt->collectValue();

    CustomDialog::post();
}
