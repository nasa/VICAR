$!****************************************************************************
$!
$! Build proc for MIPL module si_print
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:38
$!
$! Execute by entering:		$ @si_print
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
$ write sys$output "*** module si_print ***"
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
$ write sys$output "Invalid argument given to si_print.com file -- ", primary
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
$   if F$SEARCH("si_print.imake") .nes. ""
$   then
$      vimake si_print
$      purge si_print.bld
$   else
$      if F$SEARCH("si_print.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake si_print
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @si_print.bld "STD"
$   else
$      @si_print.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create si_print.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack si_print.com -mixed -
	-s SiPrintCmdInterface.cc SiPrintDialog.cc SiPrintCmd.cc -
	   SiPrintComponentCmd.cc SiPrintBrowserCmd.cc -
	-i si_print.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SiPrintCmdInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiPrintDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////
// SiPrintDialog.cc: This class creates a work area for print dialog.
//////////////////////////////////////////////////////////////////////
#include "SiPrintDialog.h"
#include "SiPrintCmdInterface.h"
#include "Cmd.h"

SiPrintDialog::SiPrintDialog(const char *name, Cmd *cmd)
    : CustomDialog(name, Default, Invisible, Invisible, Visible, Visible)
{
    _cmd = cmd;
    _printCI = NULL;
}

Widget SiPrintDialog::createWorkArea(Widget parent)
{
    _printCI = new SiPrintCmdInterface(parent, _cmd);
    _printCI->setDeferredExec(_applyCmdList);
    return _printCI->baseWidget();
}

//////////////////////////////////////////////////////////////////////
// We want OK to do something even if the user doesn't enter any values
// (e.g. print again to the same place).  So, we execute the command
// when the dialog is posted, just to prime the queue.  (otherwise, the
// user has to *do* something to get an execution on the deferred list).
//////////////////////////////////////////////////////////////////////

void SiPrintDialog::post()
{
    CustomDialog::post();		// do all the real work
    if (_printCI)
        _printCI->runCommand();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiPrintCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// SiPrintCmd.h:  Prints the image according to the SiPrintCmdValue
// CmdValue passed in.
//
//!!!! NOTE:  THIS IS A HACK CURRENTLY!  The command derives from
//!!!! SiSaveAsCmd, adding the SiPrintCmdValue parameters to
//!!!! the file, then running an *external* script to implement the
//!!!! print.  It should be done internally, via save hooks in ImageData.
//
// In adition to the information included by SiSaveAsCmd, the
// following information is passed:
//
// scriptVersion=1.3		replaces 1.2 from base class
// printTo=printer		where to print: printer or file
// printCommand="string"	command to use to print
// printWidth=6.0		width of printed image, in inches.  Blank is
//				okay (width is automatically determined)
// printHeight=6.0		height of image, in inches.  Blank is okay
// orientation=tall		orientation of page: tall (portrait) or
//				wide (landscape)
// printTitle="string"		title string to print, or blank for no title
///////////////////////////////////////////////////////////

#include "SiPrintCmd.h"
#include "SiPrintCmdValue.h"
#include "ImageData.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////

SiPrintCmd::SiPrintCmd(const char *name, int active, Widget xiw,
		ImageData *image,char *script,
                Lut *sR, Lut *sG, Lut *sB, Lut *pR, Lut *pG, Lut *pB)
        : SiSaveAsCmd(name, active, xiw, image, script,
                sR, sG, sB, pR, pG, pB)
{
   // Empty
}

////////////////////////////////////////////////////////////
// Print the version string to the temp file.  This function
// should be overridden by subclasses.
////////////////////////////////////////////////////////////

void SiPrintCmd::printVersionString(FILE *tfp)
{
   fprintf(tfp, "scriptVersion=1.3\n");
}

////////////////////////////////////////////////////////////
// Print the contents to the temp file.  This function could
// be overridden by subclasses, which should call this specific
// version to output the basic info.
////////////////////////////////////////////////////////////

void SiPrintCmd::printContents(FILE *tfp)
{
   // Print basic values

   SiSaveAsCmd::printContents(tfp);

   // Get extra values from CmdValue

   SiPrintCmdValue *value = (SiPrintCmdValue *)_value;

   if (value == NULL) {
      fprintf(stderr,"No SiPrintCmdValue, internal error, file not printed!\n");
      return;
   }

   switch (value->print_to) {
      case PrintToPrinter:
         fprintf(tfp, "printTo=printer\n");
         break;
      case PrintToFile:
         fprintf(tfp, "printTo=file\n");
         break;
      default:
         break;
   }
   fprintf(tfp, "printCommand=\"%s\"\n", value->print_command);
   fprintf(tfp, "printWidth=%s\n", value->print_width);
   fprintf(tfp, "printHeight=%s\n", value->print_height);

   switch (value->orientation) {
      case PrintPortrait:
         fprintf(tfp, "orientation=tall\n");
         break;
      case PrintLandscape:
         fprintf(tfp, "orientation=wide\n");
         break;
      default:
         break;
   }
   if (value->title_filename) {		// use filename for title
      char *filename, fn[512];
      filename = (char *)_imageData->getInputDataSourceName();
      if (filename == NULL)	 // BW files end in ",,".  Cut this off.
         strcpy(fn, "");
      else
         strcpy(fn, filename);
      if (fn[strlen(fn)-1] == ',' && fn[strlen(fn)-2] == ',')
         fn[strlen(fn)-2] = '\0';
      fprintf(tfp, "printTitle=\"%s\"\n", fn);
   }
   else {				// custom title
      fprintf(tfp, "printTitle=\"%s\"\n", value->print_title);
   }

}

////////////////////////////////////////////////////////////
// Delete the CmdValue object
////////////////////////////////////////////////////////////

void SiPrintCmd::freeValue(CmdValue value)
{
   if (value)
      delete (SiPrintCmdValue *)value;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiPrintComponentCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiPrintComponentCmd.h:  Command class for any component within
// the SiPrintCmdInterface; all it does is tell the PCI to do its
// thing.  Used for simple creation of Option menus.
////////////////////////////////////////////////////////////////

#include "SiPrintComponentCmd.h"
#include "SiPrintCmdInterface.h"

SiPrintComponentCmd::SiPrintComponentCmd(const char *name, int active,
		CmdList *radioList, SiPrintCmdInterface *pci)
	: RadioCmd(name, active, radioList)
{
    _pci = pci;
}

void SiPrintComponentCmd::doit()
{
    if (_value) {
	_pci->runCommand();
    }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiPrintBrowserCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiPrintBrowserCmd:  Accepts a filename from a SingleFileSelection
// dialog (as CmdValue) and stuffs it in the SiPrintCmdInterface.
/////////////////////////////////////////////////////////////

#include "SiPrintBrowserCmd.h"
#include "SiPrintCmdInterface.h"

SiPrintBrowserCmd::SiPrintBrowserCmd(const char *name, int active,
			SiPrintCmdInterface *pci)
	: NoUndoCmd(name, active)
{
   _pci = pci;
}

void SiPrintBrowserCmd::doit()
{
   _pci->setFilename((char *)_value);
}

void SiPrintBrowserCmd::freeValue(CmdValue value)
{
   delete (char *)value;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create si_print.imake
#define SUBROUTINE si_print
#define MODULE_LIST SiPrintCmdInterface.cc SiPrintDialog.cc SiPrintCmd.cc \
 SiPrintComponentCmd.cc SiPrintBrowserCmd.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP

$ Return
$!#############################################################################
