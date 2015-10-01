$!****************************************************************************
$!
$! Build proc for MIPL module tp_dialogs
$! VPACK Version 1.9, Friday, June 04, 2010, 14:01:57
$!
$! Execute by entering:		$ @tp_dialogs
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module tp_dialogs ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to tp_dialogs.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("tp_dialogs.imake") .nes. ""
$   then
$      vimake tp_dialogs
$      purge tp_dialogs.bld
$   else
$      if F$SEARCH("tp_dialogs.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tp_dialogs
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tp_dialogs.bld "STD"
$   else
$      @tp_dialogs.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tp_dialogs.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tp_dialogs.com -mixed -
	-s TpDisplayModeDialog.cc TpPointEditorOptsDialog.cc -
	   TpPointSymbolsDialog.cc TpPointTagsDialog.cc -
	   TpCursorSymbolsDialog.cc TpAutofindDialog.cc TpMatchModeDialog.cc -
	   TpAutofindResultsDialog.cc TpMatchModeResultsDialog.cc -
	-i tp_dialogs.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create TpDisplayModeDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpDisplayModeDialog.h: Dialog containing display mode values.
////////////////////////////////////////////////////////////
#include "TpDisplayModeDialog.h"
#include "TpNumDisplaysCmd.h"
#include "TpRotateImageCmd.h"
#include "TpSwapLockCmd.h"
#include "TpDisplayer.h"
#include "RadioCmdBox.h"
#include "CmdList.h"
#include "RadioCmd.h"
#include "OptionCmdMenu.h"
#include "CheckBoxInterface.h"
#include "TpDefs.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>
#include <stdio.h>

TpDisplayModeDialog::TpDisplayModeDialog(const char *name, 
		TpDisplayer *displayer)
	: CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{
    _displayer = displayer;
}

Widget TpDisplayModeDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("workArea", 
			xmRowColumnWidgetClass, parent, 
			XmNorientation, XmVERTICAL,
			XmNnumColumns, 1,
			NULL);

    XtVaCreateManagedWidget("displayModeLabel", xmLabelWidgetClass, rc, NULL);

    ////////////////////////////////////////////////////////////////
    // Radio box for specifying number of images displayed.
    ///////////////////////////////////////////////////////////////
    Widget displayTypeFrame = XtVaCreateManagedWidget("displayFrame",
			xmFrameWidgetClass, rc,
			NULL);
    XtVaCreateManagedWidget ("displayFrameLabel", 
			     xmLabelGadgetClass, displayTypeFrame, 
			     XmNchildType, XmFRAME_TITLE_CHILD,
			     XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
			     NULL);
    int initNumImages = _displayer->getNumWin();
    int i = 0;
    CmdList *dispTypeRadList = new CmdList;
    RadioCmd *dispTypeCmd[TP_MAX_DISPLAYS];
    for (int w=0; w < TP_MAX_DISPLAYS; w++) {
	char num[10];
	sprintf(num, "num%d", w+1);
        dispTypeCmd[i++] = new TpNumDisplaysCmd(strdup(num), True, 
				(CmdValue) (!(i+1 - initNumImages)),
				dispTypeRadList, _displayer, w+1);
    }
    UIComponent *radioBox = new RadioCmdBox(displayTypeFrame, "displayTypes", 
					dispTypeRadList, _applyCmdList);
    radioBox->manage();

    ////////////////////////////////////////////////////////////////
    // Option menus for specifying the rotation of images displayed.
    ///////////////////////////////////////////////////////////////
    Widget rotationFrame = XtVaCreateManagedWidget("rotationFrame",
			xmFrameWidgetClass, rc,
			NULL);
    XtVaCreateManagedWidget ("rotationFrameLabel", 
                             xmLabelGadgetClass, rotationFrame,
                             XmNchildType, XmFRAME_TITLE_CHILD,
                             XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
			     NULL);
    Widget rotationRC = XtVaCreateManagedWidget("rotationRC", 
			xmRowColumnWidgetClass, rotationFrame,
			NULL);
    RadioCmd *rotationCmd[4];
    for (int j = 0; j < TP_MAX_IMAGES; j++) {
	i = 0;
	CmdList *rotRadList = new CmdList;
	rotationCmd[i++] = new TpRotateImageCmd("0", True,
			rotRadList, _displayer->getSubDisplayer(j), ROTATE_NO);
	rotationCmd[i++] = new TpRotateImageCmd("90", True,
			rotRadList, _displayer->getSubDisplayer(j), ROTATE_CW);
	rotationCmd[i++] = new TpRotateImageCmd("180", True,
			rotRadList, _displayer->getSubDisplayer(j), ROTATE_FULL);
	rotationCmd[i++] = new TpRotateImageCmd("270", True,
			rotRadList, _displayer->getSubDisplayer(j), ROTATE_CCW);
	char name[16];
	sprintf(name, "rotOptMenu%d", j);
	UIComponent *optionMenu = new OptionCmdMenu(rotationRC, 
			name, rotRadList, _applyCmdList);
	optionMenu->manage();
    }

    ////////////////////////////////////////////////////////////////
    // Image swap locking options
    ////////////////////////////////////////////////////////////////
    Widget swapFrame = XtVaCreateManagedWidget("swapFrame",
			xmFrameWidgetClass, rc,
			NULL);
    XtVaCreateManagedWidget ("swapFrameLabel",
                             xmLabelGadgetClass, swapFrame,
                             XmNchildType, XmFRAME_TITLE_CHILD,
                             XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
                             NULL);
    Widget swapRC = XtVaCreateManagedWidget("swapRC",
			xmRowColumnWidgetClass, swapFrame,
			NULL);
    Cmd *swapCmd[TP_MAX_DISPLAYS];
    CmdInterface *swapCmdInterface[TP_MAX_DISPLAYS];
    for (i = 0; i < TP_MAX_DISPLAYS; i++) {
	swapCmd[i] = new TpSwapLockCmd("swapLock", True, _displayer, i);
	swapCmdInterface[i] = new CheckBoxInterface(swapRC, swapCmd[i]);
	swapCmdInterface[i]->manage();
	swapCmdInterface[i]->setDeferredExec(_applyCmdList);
    }

    return rc;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPointEditorOptsDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPointSymbolsDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPointTagsDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpCursorSymbolsDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpAutofindDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchModeDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpAutofindResultsDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpAutofindResultsDialog.h: Dialog containing autofind results values.
//////////////////////////////////////////////////////////////////////////////
#include "TpAutofindResultsDialog.h"
#include "TpMatchManager.h"
#include "TpAutofindRes.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <stdio.h>

TpAutofindResultsDialog::TpAutofindResultsDialog(const char *name, 
						 TpMatchManager *mm)
    : CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{
    _matchManager = mm;
    _autofindResults = NULL;
}

Widget TpAutofindResultsDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("workArea", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 XmNnumColumns, 1,
				 NULL);

    XtVaCreateManagedWidget("autofindResultsLabel", 
			    xmLabelWidgetClass, rc, 
			    NULL);

    _autofindResults = new TpAutofindResults(rc, "autofindResults",
					     _matchManager);
    _autofindResults->manage();

    double a[6];
    for (int i = 0; i < 6; i++)
	a[i] = _matchManager->getAutofindResult(i);

    _autofindResults->setValues(a);

    return rc;
}

void TpAutofindResultsDialog::setValues(double a[6])
{
    if (_autofindResults)
	_autofindResults->setValues(a);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchModeResultsDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpMatchModeResultsDialog.h: Dialog containing matchMode results values.
//////////////////////////////////////////////////////////////////////////////
#include "TpMatchModeResultsDialog.h"
#include "TpMatchManager.h"
#include "TpMatchModeRes.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <stdio.h>

TpMatchModeResultsDialog::TpMatchModeResultsDialog(const char *name, 
						 TpMatchManager *mm)
    : CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{
    _matchManager = mm;
    _matchModeResults = NULL;
}

Widget TpMatchModeResultsDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("workArea", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 XmNnumColumns, 1,
				 NULL);

    XtVaCreateManagedWidget("matchModeResultsLabel", 
			    xmLabelWidgetClass, rc, 
			    NULL);

    _matchModeResults = new TpMatchModeResults(rc, "matchModeResults",
					     _matchManager);
    _matchModeResults->manage();

    float a[5];
    for (int i = 0; i < 5; i++)
	a[i] = _matchManager->getMatchModeResult(i);

    _matchModeResults->setValues(a);

    return rc;
}

void TpMatchModeResultsDialog::setValues(float a[5])
{
    if (_matchModeResults)
	_matchModeResults->setValues(a);
}

Boolean TpMatchModeResultsDialog::isDumpToStdout()
{
    if (_matchModeResults)
	return _matchModeResults->isDumpToStdout();
    return False;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tp_dialogs.imake
#define SUBROUTINE tp_dialogs
#define MODULE_LIST \
  TpDisplayModeDialog.cc TpPointEditorOptsDialog.cc \
  TpPointSymbolsDialog.cc TpPointTagsDialog.cc TpCursorSymbolsDialog.cc
#define MODULE_LIST2 \
  TpAutofindDialog.cc TpMatchModeDialog.cc TpAutofindResultsDialog.cc \
  TpMatchModeResultsDialog.cc

#define P2_SUBLIB

#define MAIN_LANG_C_PLUS_PLUS
#define USES_C_PLUS_PLUS
#define CCC_TEMPLATES

#define LIB_MOTIF
#define LIB_MOTIFAPP
#define LIB_GUISUB
#define LIB_P2SUB

$ Return
$!#############################################################################
