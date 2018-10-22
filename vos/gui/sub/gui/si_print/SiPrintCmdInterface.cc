//////////////////////////////////////////////////////////////
// SiPrintCmdInterface.cc: Command interface subclass that fills
// a SiPrintCmdValue structure and executes the command.
//////////////////////////////////////////////////////////////

#include "SiPrintCmdInterface.h"
#include "SiPrintComponentCmd.h"
#include "SiPrintBrowserCmd.h"
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

#include <stdio.h>

#ifndef __VMS
#include <stdlib.h>		/* for getenv */
#endif

//////////////////////////////////////////////////////////////
// Constructor
//////////////////////////////////////////////////////////////
SiPrintCmdInterface::SiPrintCmdInterface(Widget parent, Cmd *cmd)
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

    _printToList = new CmdList();
    _printToPrinter = new SiPrintComponentCmd("to printer", True,
							_printToList, this);
    _printToFile = new SiPrintComponentCmd("to file", True, _printToList, this);

    _printToMenu = new OptionCmdMenu(_w, "printToMenu", _printToList);
    XtVaSetValues(_printToMenu->baseWidget(),
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		NULL);

    _extentList = new CmdList();
    _extentDisplay = new SiPrintComponentCmd("display", True, _extentList,this);
    _extentFile = new SiPrintComponentCmd("file", True, _extentList, this);
    _extentROI = new SiPrintComponentCmd("ROI", False, _extentList, this);

    _extentMenu = new OptionCmdMenu(_w, "extentMenu", _extentList);
    XtVaSetValues(_extentMenu->baseWidget(),
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _printToMenu->baseWidget(),
		NULL);

    _lutList = new CmdList();
    _lutRaw = new SiPrintComponentCmd("raw", True, _lutList, this);
    _lutStretch = new SiPrintComponentCmd("stretch", True, _lutList, this);
    _lutPseudo = new SiPrintComponentCmd("pseudo", True, _lutList, this);
    _lutPseudoOnly = new SiPrintComponentCmd("pseudo_only", True,_lutList,this);

    _lutMenu = new OptionCmdMenu(_w, "lutMenu", _lutList);
    XtVaSetValues(_lutMenu->baseWidget(),
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _extentMenu->baseWidget(),
		NULL);

    _browser_file_cmd = new SiPrintBrowserCmd("browse file", TRUE, this);
    _post_file_cmd = new PostSingleFileDialogCmd("post file", TRUE,
							 _browser_file_cmd);
    _browser_file = new ButtonInterface(_w, _post_file_cmd);
    XtVaSetValues(_browser_file->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _lutMenu->baseWidget(),
			NULL);

    _filename = new KeyinView(_w, "filename");
    XtVaSetValues(_filename->baseWidget(),
			XmNrightAttachment, XmATTACH_WIDGET,
			XmNrightWidget, _browser_file->baseWidget(),
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _lutMenu->baseWidget(),
			NULL);
    _filename->installCallback(&SiPrintCmdInterface::runCommandCallback,
				(XtPointer)this);

    _printerCommand = new KeyinView(_w, "printer command");
    XtVaSetValues(_printerCommand->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _filename->baseWidget(),
			NULL);
    _printerCommand->installCallback(&SiPrintCmdInterface::runCommandCallback,
				(XtPointer)this);

    Widget lbl1 = XtVaCreateManagedWidget("label1", xmLabelWidgetClass, _w,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _printerCommand->baseWidget(),
		NULL);

    _printWidth = new KeyinView(_w, "width");
    XtVaSetValues(_printWidth->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, lbl1,
			NULL);
    _printWidth->installCallback(&SiPrintCmdInterface::runCommandCallback,
				(XtPointer)this);

    _printHeight = new KeyinView(_w, "height");
    XtVaSetValues(_printHeight->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _printWidth->baseWidget(),
			NULL);
    _printHeight->installCallback(&SiPrintCmdInterface::runCommandCallback,
				(XtPointer)this);

    _printOrientationList = new CmdList();
    _printPortrait = new SiPrintComponentCmd("portrait", True,
						_printOrientationList, this);
    _printLandscape = new SiPrintComponentCmd("landscape", True,
						_printOrientationList, this);

    _printOrientationMenu = new OptionCmdMenu(_w, "orientationMenu",
							_printOrientationList);

    XtVaSetValues(_printOrientationMenu->baseWidget(),
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _printHeight->baseWidget(),
		NULL);

    _printTitleList = new CmdList();
    _printTitleFilename = new SiPrintComponentCmd("title filename", True,
						_printTitleList, this);
    _printTitleCustom = new SiPrintComponentCmd("title custom", True,
						_printTitleList, this);

    _printTitleMenu = new OptionCmdMenu(_w, "titleMenu", _printTitleList);

    XtVaSetValues(_printTitleMenu->baseWidget(),
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _printOrientationMenu->baseWidget(),
		NULL);

    _title = new KeyinView(_w, "title");
    XtVaSetValues(_title->baseWidget(),
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _printTitleMenu->baseWidget(),
			NULL);
    _title->installCallback(&SiPrintCmdInterface::runCommandCallback,
				(XtPointer)this);

    Widget lbl2 = XtVaCreateManagedWidget("label2", xmLabelWidgetClass, _w,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _title->baseWidget(),
		NULL);

    Widget UNUSED(lbl3) = XtVaCreateManagedWidget("label3", xmLabelWidgetClass, _w,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, lbl2,
		NULL);

    // Get current command value and set the interface appropriately

    SiPrintCmdValue *value = (SiPrintCmdValue *)(_cmd->getValue());
    setValue((CmdValue)value);

    // Manage everything

    _browser_file->manage();
    _filename->manage();
    _extentMenu->manage();
    _lutMenu->manage();
    _printToMenu->manage();
    _printerCommand->manage();
    _printWidth->manage();
    _printHeight->manage();
    _printOrientationMenu->manage();
    _printTitleMenu->manage();
    _title->manage();

}

//////////////////////////////////////////////////////////////
// Destructor
//////////////////////////////////////////////////////////////
SiPrintCmdInterface::~SiPrintCmdInterface()
{
    // Empty... should delete all the Cmd's and such...
}

//////////////////////////////////////////////////////////////
// Actually run the command by creating a dynamically allocated
// SiPrintCmdValue (because that's what Cmd likes), filling it in,
// and executing.
//////////////////////////////////////////////////////////////
void SiPrintCmdInterface::runCommandCallback(Widget, XtPointer clientData,
					 XtPointer)
{
    SiPrintCmdInterface *obj = (SiPrintCmdInterface *)clientData;
    obj->runCommand();
}

void SiPrintCmdInterface::runCommand()
{
    if (_suspend_updates)
	return;			// don't do anything

    SiPrintCmdValue *value = new SiPrintCmdValue();

    char *name = _filename->getFieldValue();
    strcpy(value->filename_red, name);
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

    if (_printToPrinter->getValue())
	value->print_to = PrintToPrinter;
    if (_printToFile->getValue())
	value->print_to = PrintToFile;

    name = _printerCommand->getFieldValue();
    strcpy(value->print_command, name);
    XtFree(name);

    name = _printWidth->getFieldValue();
    strcpy(value->print_width, name);
    XtFree(name);

    name = _printHeight->getFieldValue();
    strcpy(value->print_height, name);
    XtFree(name);

    if (_printPortrait->getValue())
	value->orientation = PrintPortrait;
    if (_printLandscape->getValue())
	value->orientation = PrintLandscape;

    if (_printTitleFilename->getValue())
	value->title_filename = True;
    if (_printTitleCustom->getValue())
	value->title_filename = False;

    name = _title->getFieldValue();
    strcpy(value->print_title, name);
    XtFree(name);

    // Fill in unused portions of the SaveCmdValue

    value->asByte = True;
    strcpy(value->filename_grn, "");
    strcpy(value->filename_blu, "");
    strcpy(value->fileFormat, "VICAR");

    setPrintTo();

    runCmd((CmdValue)value);		// Cmd infrastructure frees the value
}

//////////////////////////////////////////////////////////////
// Update the interface to match a given value
//////////////////////////////////////////////////////////////
void SiPrintCmdInterface::setValue(CmdValue cmd_value)
{
    CmdInterface::setValue(cmd_value);	// Removes cmd from deferred list

    SiPrintCmdValue *value = (SiPrintCmdValue *)cmd_value;

    if (value == NULL) {
	char *printer;

	value = new SiPrintCmdValue;		// pick up the defaults

	// Generate the dynamic default for the printer command, based on
	// the value of $PRINTER.

	printer = NULL;
#ifndef __VMS
	printer = getenv("PRINTER");
#endif

	//!!!! Linux uses "lpr" while everyone else uses "lp".  So set the
	//!!!! default differently based on the platform.
	//!!!! This is NOT the right way to do this!!!! There should
	//!!!! instead be a "feature define"... or at least use xvmaininc!
	if (printer == NULL) {
#if defined(__linux__) || defined(linux)
	    strcpy(value->print_command, "lpr");
#else
	    strcpy(value->print_command, "lp -c");
#endif
	}
	else {
#if defined(__linux__) || defined(linux)
	    sprintf(value->print_command, "lpr -P %s", printer);
#else
	    sprintf(value->print_command, "lp -c -d %s", printer);
#endif
	}
    }

    _suspend_updates = True;		// avoid calling runCommand here

    _filename->setFieldValue(value->filename_red);

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

    switch (value->print_to) {
	case PrintToPrinter:
	    _printToPrinter->execute((CmdValue)True);
	    break;
	case PrintToFile:
	    _printToFile->execute((CmdValue)True);
	    break;
	default:
	    break;
    }

    _printerCommand->setFieldValue(value->print_command);
    _printWidth->setFieldValue(value->print_width);
    _printHeight->setFieldValue(value->print_height);

    switch (value->orientation) {
	case PrintPortrait:
	    _printPortrait->execute((CmdValue)True);
	    break;
	case PrintLandscape:
	    _printLandscape->execute((CmdValue)True);
	    break;
	default:
	    break;
    }

    if (value->title_filename)
	_printTitleFilename->execute((CmdValue)True);
    else
	_printTitleCustom->execute((CmdValue)True);

    _title->setFieldValue(value->print_title);

    setPrintTo();

    _suspend_updates = False;

    if (cmd_value == NULL)
	delete value;				// only if we created it
}

//////////////////////////////////////////////////////////////
// Update the filename only (from the browse command)
//////////////////////////////////////////////////////////////

void SiPrintCmdInterface::setFilename(char *name)
{
    _filename->setFieldValue(name);
    runCommand();
}

//////////////////////////////////////////////////////////////
// Sets the sensitivity of the command/file keyins based on the
// state of the Print To menu.  Also sets the sensitivity of
// the Title keyin based on the setting of its option menu.
//////////////////////////////////////////////////////////////

void SiPrintCmdInterface::setPrintTo()
{
    if (_printToFile->getValue()) {
	XtSetSensitive(_filename->baseWidget(), TRUE);
	_post_file_cmd->activate();
    }
    else {
	XtSetSensitive(_filename->baseWidget(), FALSE);
	_post_file_cmd->deactivate();
    }

    if (_printToPrinter->getValue())
	XtSetSensitive(_printerCommand->baseWidget(), TRUE);
    else
	XtSetSensitive(_printerCommand->baseWidget(), FALSE);

    if (_printTitleFilename->getValue())
	XtSetSensitive(_title->baseWidget(), FALSE);
    else
	XtSetSensitive(_title->baseWidget(), TRUE);

}

