//////////////////////////////////////////////////////////////////////////////
// TpPointSymbolSDialog.h: Dialog containing point symbol options.
//////////////////////////////////////////////////////////////////////////////
#include "TpPointSymbolsDialog.h"
#include "TpSetPointSymbolCmd.h"
#include "TpSetPointSizeCmd.h"
#include "TpMatchManager.h"
#include "TpPoint.h"
#include "TpSetPointColorCmd.h"
#include "TpSetPointColorSelCmd.h"
#include "TpColorCodeCmd.h"
#include "SgColorChooserInterface.h"
#include "ButtonInterface.h"
#include "TpDefs.h"
#include "RadioCmd.h"
#include "OptionCmdMenu.h"
#include "CmdList.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <stdio.h>

TpPointSymbolsDialog::TpPointSymbolsDialog(const char *name, TpMatchManager *mm)
    : CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{
    _matchManager = mm;
}

Widget TpPointSymbolsDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("workArea", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 XmNnumColumns, 1,
				 NULL);

    XtVaCreateManagedWidget("TpPointEditorOptsLabel", 
			    xmLabelWidgetClass, rc, 
			    NULL);

    CmdList *symbolRadioList = new CmdList;
    int i = 0;
    _symbolCmd[i++] = new TpSetPointSymbolCmd("CrossWithDot", True, 
					     symbolRadioList, 
					     CrossWithDot, _matchManager);
    _symbolCmd[i++] = new TpSetPointSymbolCmd("Rectangle", True, 
					     symbolRadioList, 
					     Rectangle, _matchManager);
    _symbolCmd[i++] = new TpSetPointSymbolCmd("Dot", True,
                                             symbolRadioList,
                                             Dot, _matchManager);
    _symbolCmd[i++] = new TpSetPointSymbolCmd("Cross45", True,
                                             symbolRadioList,
                                             Cross45, _matchManager);
    _symbolCmd[i++] = new TpSetPointSymbolCmd("CrossWithHole45", True, 
					     symbolRadioList, 
					     CrossWithHole45, _matchManager);
    _symbolCmd[i++] = new TpSetPointSymbolCmd("RectWithCrossesWithDot", True, 
					     symbolRadioList, 
					     RectangleWithCrossesWithDot, 
					     _matchManager);

    UIComponent *symbolOptionMenu = new OptionCmdMenu(rc, "symbol", 
						      symbolRadioList, 
						      _applyCmdList);
    symbolOptionMenu->manage();

    CmdList *sizeRadioList = new CmdList;
    i = 0;
    _sizeCmd[i++] = new TpSetPointSizeCmd("Size5", True,
					 sizeRadioList,
					 5, _matchManager);
    _sizeCmd[i++] = new TpSetPointSizeCmd("Size10", True,
					 sizeRadioList,
					 10, _matchManager);
    _sizeCmd[i++] = new TpSetPointSizeCmd("Size15", True,
					 sizeRadioList,
					 15, _matchManager);
    _sizeCmd[i++] = new TpSetPointSizeCmd("Size20", True,
					 sizeRadioList,
					 20, _matchManager);
    _sizeCmd[i++] = new TpSetPointSizeCmd("Size25", True,
					 sizeRadioList,
					 25, _matchManager);
    _sizeCmd[i++] = new TpSetPointSizeCmd("Size30", True,
					 sizeRadioList,
					 30, _matchManager);

    UIComponent *sizeOptionMenu = new OptionCmdMenu(rc, "size", sizeRadioList,
						    _applyCmdList);
    sizeOptionMenu->manage();

    // Set point's color

    Widget rcColor = XtVaCreateManagedWidget("rcColor",
                                 xmRowColumnWidgetClass, rc,
                                 XmNorientation, XmHORIZONTAL,
                                 XmNnumColumns, 1,
                                 NULL);
    //!!!XtVaCreateManagedWidget("labelColor", xmLabelWidgetClass, rcColor);
    char *colorValueStr = sdup(_matchManager->getPointColor());
    CmdValue colorValue = (CmdValue)colorValueStr;
    _colorCmd = new TpSetPointColorCmd("Color", True, colorValue, 
					   _matchManager);
    CmdInterface *keyinColor = new SgColorChooserInterface(rcColor, _colorCmd);
    keyinColor->setDeferredExec(_applyCmdList);
    keyinColor->manage();

    // Set selected point's color

    Widget rcColorSel = XtVaCreateManagedWidget("rcColorSel",
                                 xmRowColumnWidgetClass, rc,
                                 XmNorientation, XmHORIZONTAL,
                                 XmNnumColumns, 1,
                                 NULL);
    //!!!XtVaCreateManagedWidget("labelColorSel", xmLabelWidgetClass, rcColorSel);
    char *colorSelValueStr = sdup(_matchManager->getPointColorSel());
    CmdValue colorSelValue = (CmdValue)colorSelValueStr;
    _colorSelCmd = new TpSetPointColorSelCmd("ColorSel", True, 
						 colorSelValue, _matchManager);
    CmdInterface *keyinColorSel;
    keyinColorSel = new SgColorChooserInterface(rcColorSel, _colorSelCmd);
    keyinColorSel->setDeferredExec(_applyCmdList);
    keyinColorSel->manage();

    Cmd *colorCodeCmd = new TpColorCodeCmd("Color Code Point", True,
					_matchManager);
    CmdInterface *colorCodeIf = new ButtonInterface(rc, colorCodeCmd); 
    colorCodeIf->setDeferredExec(_applyCmdList);
    colorCodeIf->manage();

    return rc;
}

void TpPointSymbolsDialog::post()
{
    CustomDialog::post();

    int i;
    for (i = 0; i < 6; i++) {
	TpPointSymbolShapeType sh = _matchManager->getPointSymbolShape();
	if (sh == _symbolCmd[i]->getShape())
	    _symbolCmd[i]->execute((CmdValue)True);
	else 
	    _symbolCmd[i]->execute((CmdValue)False);
    }

    for (i = 0; i < 6; i++) {
        int w = _matchManager->getPointWidth();
        if (w == _sizeCmd[i]->getSize())
            _sizeCmd[i]->execute((CmdValue)True);
        else
            _sizeCmd[i]->execute((CmdValue)False);
    }

    char *colorValueStr = sdup(_matchManager->getPointColor());
    CmdValue colorValue = (CmdValue)colorValueStr;
    _colorCmd->execute(colorValue);

    char *colorSelValueStr = sdup(_matchManager->getPointColorSel());
    CmdValue colorSelValue = (CmdValue)colorSelValueStr;
    _colorSelCmd->execute(colorSelValue);
}
