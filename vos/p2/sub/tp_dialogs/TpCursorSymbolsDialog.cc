////////////////////////////////////////////////////////////
// TpCursorSymbolsDialog.h: Dialog containing cursor symbol options.
////////////////////////////////////////////////////////////
#include "TpCursorSymbolsDialog.h"
#include "TpDisplayer.h"
#include "TpSetCursorColorCmd.h"
#include "TpSetCursorSymbolCmd.h"
#include "SgColorChooserInterface.h"
#include "RadioCmd.h"
#include "RadioCmdBox.h"
#include "CmdList.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <stdio.h>

TpCursorSymbolsDialog::TpCursorSymbolsDialog(const char *name, TpDisplayer *disp)
    : CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{
    _displayer = disp;
}

Widget TpCursorSymbolsDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("workArea", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 XmNnumColumns, 1,
				 NULL);

    XtVaCreateManagedWidget("TpCursorEditorOptsLabel", 
			    xmLabelWidgetClass, rc, 
			    NULL);

    RadioCmd *symbolCmd[6];
    CmdList *symbolRadioList = new CmdList;
    int i = 0;
    symbolCmd[i++] = new TpSetCursorSymbolCmd("crosshair", True,
                                             symbolRadioList, _displayer);
    symbolCmd[i++] = new TpSetCursorSymbolCmd("cross", True,
                                             symbolRadioList, _displayer);
    symbolCmd[i++] = new TpSetCursorSymbolCmd("tcross", True,
                                             symbolRadioList, _displayer);
    symbolCmd[i++] = new TpSetCursorSymbolCmd("target", True,
                                             symbolRadioList, _displayer);
    symbolCmd[i++] = new TpSetCursorSymbolCmd("draped_box", True,
                                             symbolRadioList, _displayer);
    symbolCmd[i++] = new TpSetCursorSymbolCmd("X_cursor", True,
                                             symbolRadioList, _displayer);

    UIComponent *symbolOptionMenu = new RadioCmdBox(rc, "symbol",
						    symbolRadioList,
						    _applyCmdList);
    symbolOptionMenu->manage();

    // Set cursor's color
    Widget rcColor = XtVaCreateManagedWidget("rcColor",
                                 xmRowColumnWidgetClass, rc,
                                 XmNorientation, XmHORIZONTAL,
                                 XmNnumColumns, 1,
                                 NULL);
    //Widget xxx = XtVaCreateManagedWidget("xxxlabelColor", xmLabelWidgetClass, rcColor);
    char *colorValueStr = sdup(_displayer->getCursorColor());
    CmdValue colorValue = (CmdValue)colorValueStr;
    Cmd *colorCmd = new TpSetCursorColorCmd("cursorColor", True, 
					    colorValue, _displayer);
    CmdInterface *keyinColor = new SgColorChooserInterface(rcColor, colorCmd);
    keyinColor->setDeferredExec(_applyCmdList);
    keyinColor->manage();

    return rc;
}
