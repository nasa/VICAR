////////////////////////////////////////////////////////////
// TpPointTagsDialog.h: Dialog containing point tag options.
////////////////////////////////////////////////////////////
#include "TpPointTagsDialog.h"
#include "TpSetTagPositionCmd.h"
#include "TpMatchManager.h"
#include "TpDefs.h"
#include "RadioCmd.h"
#include "RadioCmdBox.h"
#include "CmdList.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <stdio.h>

TpPointTagsDialog::TpPointTagsDialog(const char *name, TpMatchManager *mm)
	: CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{
    _matchManager = mm;
}

Widget TpPointTagsDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("workArea", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 XmNnumColumns, 1,
				 NULL);

    XtVaCreateManagedWidget("TpPointTagLabel", 
			    xmLabelWidgetClass, rc, 
			    NULL);

    CmdList *tagPosRadioList = new CmdList;
    int i = 0;
    CmdValue neValue, nwValue, seValue, swValue, ctrValue;
    neValue = nwValue = seValue = swValue = ctrValue = (CmdValue)False;
    if (_matchManager->getTagPosition() == NorthEast)
	neValue = (CmdValue)True;
    else if (_matchManager->getTagPosition() == NorthWest)
        nwValue = (CmdValue)True;
    else if (_matchManager->getTagPosition() == SouthEast)
        seValue = (CmdValue)True;
    else if (_matchManager->getTagPosition() == SouthWest)
        swValue = (CmdValue)True;
    else if (_matchManager->getTagPosition() == Center)
        ctrValue = (CmdValue)True;

    Cmd *tagPosCmd[5];
    tagPosCmd[i++] = new TpSetTagPositionCmd("NorthEast", True, 
					     neValue, tagPosRadioList, 
					     NorthEast, _matchManager);
    tagPosCmd[i++] = new TpSetTagPositionCmd("NorthWest", True, 
					     nwValue, tagPosRadioList, 
					     NorthWest, _matchManager);
    tagPosCmd[i++] = new TpSetTagPositionCmd("SouthEast", True, 
					     seValue, tagPosRadioList, 
					     SouthEast, _matchManager);
    tagPosCmd[i++] = new TpSetTagPositionCmd("SouthWest", True, 
					     swValue, tagPosRadioList, 
					     SouthWest, _matchManager);
    tagPosCmd[i++] = new TpSetTagPositionCmd("Center", True, 
					     ctrValue, tagPosRadioList, 
					     Center, _matchManager);

    UIComponent *tagPosOptionMenu = new RadioCmdBox(rc,"tag",tagPosRadioList, 
						    _applyCmdList);

    tagPosOptionMenu->manage();

    return rc;
}
