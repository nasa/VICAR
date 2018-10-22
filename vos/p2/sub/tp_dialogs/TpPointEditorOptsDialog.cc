////////////////////////////////////////////////////////////
// TpPointEditorOptsDialog.h: Dialog containing display mode values.
////////////////////////////////////////////////////////////
#include "TpPointEditorOptsDialog.h"
#include "TpCheckGenQualUniqueCmd.h"
#include "TpShowPointLabelsCmd.h"
#include "TpMatchManager.h"
#include "TpDefs.h"
#include "CheckBoxInterface.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <stdio.h>

TpPointEditorOptsDialog::TpPointEditorOptsDialog(const char *name, 
						 TpMatchManager *mm)
	: CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{
    _matchManager = mm;
}

Widget TpPointEditorOptsDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("workArea", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 XmNnumColumns, 1,
				 NULL);

    XtVaCreateManagedWidget("TpPointEditorOptsLabel", 
			    xmLabelWidgetClass, rc, 
			    NULL);

    Cmd *genQualUniqueCmd = new TpCheckGenQualUniqueCmd("GenQualUniqueCmd", 
							True, _matchManager);
    CmdInterface *genQualUniqueIf = new CheckBoxInterface(rc, 
							  genQualUniqueCmd);
    genQualUniqueIf->setDeferredExec(_applyCmdList);
    genQualUniqueIf->manage();

    Cmd *showLabelsCmd = new TpShowPointLabelsCmd("ShowPointLabelsCmd", 
							True, _matchManager);
    CmdInterface *showLabelsIf = new CheckBoxInterface(rc, showLabelsCmd);
    showLabelsIf->setDeferredExec(_applyCmdList);
    showLabelsIf->manage();

    return rc;
}
