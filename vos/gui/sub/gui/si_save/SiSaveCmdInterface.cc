//////////////////////////////////////////////////////////////
// SiSaveCmdInterface.cc: Command interface subclass that fills
// a SiSaveCmdValue structure and executes the command.
//////////////////////////////////////////////////////////////

#include "SiSaveCmdInterface.h"
#include "SiSaveComponentCmd.h"
#include "SiSaveBrowserCmd.h"
#include "PostSingleFileDialogCmd.h"
#include "CmdList.h"
#include "OptionCmdMenu.h"
#include "KeyinView.h"
#include "ButtonInterface.h"
#include "xvmaininc.h"		// for UNUSED

#include <Xm/Form.h>
#include <Xm/Label.h>
#include <iostream>
using namespace std;

//////////////////////////////////////////////////////////////
// Constructor
//////////////////////////////////////////////////////////////
SiSaveCmdInterface::SiSaveCmdInterface(Widget parent, Cmd *cmd)
    : CmdInterface ( cmd )
{

    _suspend_updates = False;

    _w = XtVaCreateWidget(_name, xmFormWidgetClass, parent, NULL);
    installDestroyHandler();

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.

    if ( _active )
	activate();
    else
	deactivate();      

    // Create the contents of the interface

    _browser_red_cmd = new SiSaveBrowserCmd("browse red", TRUE, this, 0);
    _post_red_cmd = new PostSingleFileDialogCmd("post red", TRUE,
							 _browser_red_cmd);
    _browser_red = new ButtonInterface(_w, _post_red_cmd);
    XtVaSetValues(_browser_red->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_FORM,
			NULL);

    _filename_red = new KeyinView(_w, "filename red");
    XtVaSetValues(_filename_red->baseWidget(),
			XmNrightAttachment, XmATTACH_WIDGET,
			XmNrightWidget, _browser_red->baseWidget(),
			XmNtopAttachment, XmATTACH_FORM,
			NULL);
    _filename_red->installCallback(&SiSaveCmdInterface::runCommandCallback,
				(XtPointer)this);

    _browser_grn_cmd = new SiSaveBrowserCmd("browse grn", TRUE, this, 0);
    _post_grn_cmd = new PostSingleFileDialogCmd("post grn", TRUE,
							 _browser_grn_cmd);
    _browser_grn = new ButtonInterface(_w, _post_grn_cmd);
    XtVaSetValues(_browser_grn->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _filename_red->baseWidget(),
			NULL);

    _filename_grn = new KeyinView(_w, "filename grn");
    XtVaSetValues(_filename_grn->baseWidget(),
			XmNrightAttachment, XmATTACH_WIDGET,
			XmNrightWidget, _browser_grn->baseWidget(),
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _filename_red->baseWidget(),
			NULL);
    _filename_grn->installCallback(&SiSaveCmdInterface::runCommandCallback,
				(XtPointer)this);

    _browser_blu_cmd = new SiSaveBrowserCmd("browse blu", TRUE, this, 0);
    _post_blu_cmd = new PostSingleFileDialogCmd("post blu", TRUE,
							 _browser_blu_cmd);
    _browser_blu = new ButtonInterface(_w, _post_blu_cmd);
    XtVaSetValues(_browser_blu->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _filename_grn->baseWidget(),
			NULL);

    _filename_blu = new KeyinView(_w, "filename blu");
    XtVaSetValues(_filename_blu->baseWidget(),
			XmNrightAttachment, XmATTACH_WIDGET,
			XmNrightWidget, _browser_blu->baseWidget(),
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _filename_grn->baseWidget(),
			NULL);
    _filename_blu->installCallback(&SiSaveCmdInterface::runCommandCallback,
				(XtPointer)this);

    _extentList = new CmdList();
    _extentDisplay = new SiSaveComponentCmd("display", True, _extentList, this);
    _extentFile = new SiSaveComponentCmd("file", True, _extentList, this);
    _extentROI = new SiSaveComponentCmd("ROI", False, _extentList, this);

    _extentMenu = new OptionCmdMenu(_w, "extentMenu", _extentList);
    XtVaSetValues(_extentMenu->baseWidget(),
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _filename_blu->baseWidget(),
		NULL);

    _lutList = new CmdList();
    _lutRaw = new SiSaveComponentCmd("raw", True, _lutList, this);
    _lutStretch = new SiSaveComponentCmd("stretch", True, _lutList, this);
    _lutPseudo = new SiSaveComponentCmd("pseudo", True, _lutList, this);
    _lutPseudoOnly = new SiSaveComponentCmd("pseudo_only", True, _lutList,this);

    _lutMenu = new OptionCmdMenu(_w, "lutMenu", _lutList);
    XtVaSetValues(_lutMenu->baseWidget(),
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _extentMenu->baseWidget(),
		NULL);

    _byteList = new CmdList();
    _byteFalse = new SiSaveComponentCmd("preserve data type", True,
							_byteList, this);
    _byteTrue = new SiSaveComponentCmd("convert to byte", True,
							_byteList, this);

    _byteMenu = new OptionCmdMenu(_w, "byteMenu", _byteList);
    XtVaSetValues(_byteMenu->baseWidget(),
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _lutMenu->baseWidget(),
		NULL);

    _fmtList = new CmdList();
    _fmtVicar = new SiSaveComponentCmd("VICAR", True, _fmtList, this);
    _fmtTiff = new SiSaveComponentCmd("TIFF", True, _fmtList, this);

    _fmtMenu = new OptionCmdMenu(_w, "fmtMenu", _fmtList);

    XtVaSetValues(_fmtMenu->baseWidget(),
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _byteMenu->baseWidget(),
		NULL);

    Widget lbl1 = XtVaCreateManagedWidget("label1", xmLabelWidgetClass, _w,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _fmtMenu->baseWidget(),
		NULL);

    Widget lbl2 = XtVaCreateManagedWidget("label2", xmLabelWidgetClass, _w,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, lbl1,
		NULL);

    Widget UNUSED(lbl3) = XtVaCreateManagedWidget("label3", xmLabelWidgetClass, _w,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, lbl2,
		NULL);

    // Get current command value and set the interface appropriately

    SiSaveCmdValue *value = (SiSaveCmdValue *)(_cmd->getValue());
    setValue((CmdValue)value);

    // Manage everything

    _browser_red->manage();
    _filename_red->manage();
    _browser_grn->manage();
    _filename_grn->manage();
    _browser_blu->manage();
    _filename_blu->manage();
    _extentMenu->manage();
    _lutMenu->manage();
    _byteMenu->manage();
    _fmtMenu->manage();

}

//////////////////////////////////////////////////////////////
// Destructor
//////////////////////////////////////////////////////////////
SiSaveCmdInterface::~SiSaveCmdInterface()
{
    // Empty... should delete all the Cmd's and such...
}

//////////////////////////////////////////////////////////////
// Actually run the command by creating a dynamically allocated
// SiSaveCmdValue (because that's what Cmd likes), filling it in,
// and executing.
//////////////////////////////////////////////////////////////
void SiSaveCmdInterface::runCommandCallback(Widget, XtPointer clientData,
					 XtPointer)
{
    SiSaveCmdInterface *obj = (SiSaveCmdInterface *)clientData;
    obj->runCommand();
}

void SiSaveCmdInterface::runCommand()
{
    if (_suspend_updates)
	return;			// don't do anything

    SiSaveCmdValue *value = new SiSaveCmdValue();

    char *name = _filename_red->getFieldValue();
    strcpy(value->filename_red, name);
    XtFree(name);
    name = _filename_grn->getFieldValue();
    strcpy(value->filename_grn, name);
    XtFree(name);
    name = _filename_blu->getFieldValue();
    strcpy(value->filename_blu, name);
    XtFree(name);

    if (_extentDisplay->getValue())
	value->imageExtent = SaveDisplayOnly;
    if (_extentFile->getValue())
	value->imageExtent = SaveEntireFile;
    if (_extentROI->getValue())
	value->imageExtent = SaveROI;

    if (_lutRaw->getValue())
	value->lutType = XvicRAW;
    if (_lutStretch->getValue())
	value->lutType = XvicSTRETCH;
    if (_lutPseudo->getValue())
	value->lutType = XvicPSEUDO;
    if (_lutPseudoOnly->getValue())
	value->lutType = XvicPSEUDO_ONLY;

    if (_byteFalse->getValue())
	value->asByte = False;
    if (_byteTrue->getValue())
	value->asByte = True;

    if (_fmtVicar->getValue())
	strcpy(value->fileFormat, "VICAR");
    if (_fmtTiff->getValue())
	strcpy(value->fileFormat, "TIFF");

    runCmd((CmdValue)value);		// Cmd infrastructure frees the value
}

//////////////////////////////////////////////////////////////
// Update the interface to match a given value
//////////////////////////////////////////////////////////////
void SiSaveCmdInterface::setValue(CmdValue cmd_value)
{
    CmdInterface::setValue(cmd_value);	// Removes cmd from deferred list

    SiSaveCmdValue *value = (SiSaveCmdValue *)cmd_value;

    if (value == NULL)
	value = new SiSaveCmdValue;		// pick up the defaults

    _suspend_updates = True;		// avoid calling runCommand here

    _filename_red->setFieldValue(value->filename_red);
    _filename_grn->setFieldValue(value->filename_grn);
    _filename_blu->setFieldValue(value->filename_blu);

    switch (value->imageExtent) {
	case SaveDisplayOnly:
	    _extentDisplay->execute((CmdValue)True);
	    break;
	case SaveEntireFile:
	    _extentFile->execute((CmdValue)True);
	    break;
	case SaveROI:
	    _extentROI->execute((CmdValue)True);
	    break;
	default:
	    break;
    }

    switch (value->lutType) {
	case XvicRAW:
	    _lutRaw->execute((CmdValue)True);
	    break;
	case XvicSTRETCH:
	    _lutStretch->execute((CmdValue)True);
	    break;
	case XvicPSEUDO:
	    _lutPseudo->execute((CmdValue)True);
	    break;
	case XvicPSEUDO_ONLY:
	    _lutPseudoOnly->execute((CmdValue)True);
	    break;
	default:
	    break;
    }

    if (value->asByte)
	_byteTrue->execute((CmdValue)True);
    else
	_byteFalse->execute((CmdValue)True);

    // Not sure what to do if it's something else...
    if (strcmp(value->fileFormat, "TIFF") == 0)
	_fmtTiff->execute((CmdValue)True);
    else
	_fmtVicar->execute((CmdValue)False);

    _suspend_updates = False;

    if (cmd_value == NULL)
	delete value;				// only if we created it
}

//////////////////////////////////////////////////////////////
// Update the filename only (from the browse command)
//////////////////////////////////////////////////////////////

void SiSaveCmdInterface::setFilename(char *name, int which)
{
    switch (which) {
	case 0:			// red
	    _filename_red->setFieldValue(name);
	    break;
	case 1:			// grn
	    _filename_grn->setFieldValue(name);
	    break;
	default:		// must be blu
	    _filename_blu->setFieldValue(name);
	    break;
    }
    runCommand();
}

