$!****************************************************************************
$!
$! Build proc for MIPL module si_save
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:39
$!
$! Execute by entering:		$ @si_save
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
$ write sys$output "*** module si_save ***"
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
$ write sys$output "Invalid argument given to si_save.com file -- ", primary
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
$   if F$SEARCH("si_save.imake") .nes. ""
$   then
$      vimake si_save
$      purge si_save.bld
$   else
$      if F$SEARCH("si_save.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake si_save
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @si_save.bld "STD"
$   else
$      @si_save.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create si_save.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack si_save.com -mixed -
	-s SiSaveCmdInterface.cc SiSaveDialog.cc SiSaveAsCmd.cc -
	   SiSaveComponentCmd.cc SiSaveBrowserCmd.cc -
	-i si_save.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SiSaveCmdInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiSaveDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////
// SiSaveDialog.cc: This class creates a work area for save dialog.
//////////////////////////////////////////////////////////////////////
#include "SiSaveDialog.h"
#include "SiSaveCmdInterface.h"
#include "Cmd.h"

SiSaveDialog::SiSaveDialog(const char *name, Cmd *cmd)
    : CustomDialog(name, Default, Invisible, Invisible, Visible, Visible)
{
    _cmd = cmd;
    _saveCI = NULL;
}

Widget SiSaveDialog::createWorkArea(Widget parent)
{
    _saveCI = new SiSaveCmdInterface(parent, _cmd);
    _saveCI->setDeferredExec(_applyCmdList);
    return _saveCI->baseWidget();
}

//////////////////////////////////////////////////////////////////////
// We want OK to do something even if the user doesn't enter any values
// (e.g. save again to the same place).  So, we execute the command
// when the dialog is posted, just to prime the queue.  (otherwise, the
// user has to *do* something to get an execution on the deferred list).
//////////////////////////////////////////////////////////////////////

void SiSaveDialog::post()
{
    CustomDialog::post();		// do all the real work
    if (_saveCI)
        _saveCI->runCommand();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiSaveAsCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// SiSaveAsCmd.h:  Saves the image according to the SiSaveCmdValue
// CmdValue passed in.
//
//!!!! NOTE:  THIS IS A HACK CURRENTLY!  The command derives from
//!!!! SiRunStretchScriptCmd, adding the SiSaveCmdValue parameters to
//!!!! the file, then running an *external* script to implement the
//!!!! save.  It should be done internally, via save hooks in ImageData.
//
// In adition to the information included by SiRunStretchScriptCmd, the
// following information is passed:
//
// scriptVersion=1.2		replaces 1.1 from base class
// saveFilename="string"	see below
// saveImageExtent=file		how much to save: display, file, roi
// saveLutType=stretch		whether to use stretch/pseudo tables
//				(same values as lutType in parent)
// saveAsByte=1			0=retain data type (no stretch unless byte)
//				1=convert to byte (stretch/pseudo allowed)
// saveFileFormat="VICAR"	file format to save in, currently VICAR or TIFF
//
// saveFilename follows the same rules as filename in SiRunScriptCmd,
// except that band numbers (in parens) are not allowed.
///////////////////////////////////////////////////////////

#include "SiSaveAsCmd.h"
#include "SiSaveCmdValue.h"
#include "XvicImage.h"
#include "ImageToReloadGlue.h"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////

SiSaveAsCmd::SiSaveAsCmd(const char *name, int active, Widget xiw,
		ImageData *image, const char *script,
                Lut *sR, Lut *sG, Lut *sB, Lut *pR, Lut *pG, Lut *pB)
        : SiRunStretchScriptCmd(name, active, xiw, image, script,
                sR, sG, sB, pR, pG, pB)
{
   // Empty
}

////////////////////////////////////////////////////////////
// Print the version string to the temp file.  This function
// should be overridden by subclasses.
////////////////////////////////////////////////////////////

void SiSaveAsCmd::printVersionString(FILE *tfp)
{
   fprintf(tfp, "scriptVersion=1.2\n");
}

////////////////////////////////////////////////////////////
// Print the contents to the temp file.  This function could
// be overridden by subclasses, which should call this specific
// version to output the basic info.
////////////////////////////////////////////////////////////

void SiSaveAsCmd::printContents(FILE *tfp)
{
   // Print basic values

   SiRunStretchScriptCmd::printContents(tfp);

   // Get extra values from CmdValue

   SiSaveCmdValue *value = (SiSaveCmdValue *)_value;

   if (value == NULL) {
      fprintf(stderr, "No SiSaveCmdValue, internal error, file not saved!\n");
      return;
   }

   if (strlen(value->filename_grn) == 0 && strlen(value->filename_blu) == 0)
      fprintf(tfp, "saveFilename=\"%s\"\n", value->filename_red);
   else
      fprintf(tfp, "saveFilename=(\"%s\",\"%s\",\"%s\")\n",
		value->filename_red, value->filename_grn, value->filename_blu);

   switch (value->imageExtent) {
      case SaveDisplayOnly:
         fprintf(tfp, "saveImageExtent=display\n");
         break;
      case SaveEntireFile:
         fprintf(tfp, "saveImageExtent=file\n");
         break;
      case SaveROI:			// Not Implemented!!!!
         fprintf(tfp, "saveImageExtent=roi\n");
         break;
      default:
         break;
   }

   switch (value->lutType) {
      case XvicRAW:
         fprintf(tfp, "saveLutType=raw\n");
         break;
      case XvicSTRETCH:
         fprintf(tfp, "saveLutType=stretch\n");
         break;
      case XvicPSEUDO:
         fprintf(tfp, "saveLutType=pseudo\n");
         break;
      case XvicPSEUDO_ONLY:
         fprintf(tfp, "saveLutType=pseudo_only\n");
         break;
      default:
         break;
   }

   fprintf(tfp, "saveAsByte=%d\n", value->asByte ? 1 : 0);
   fprintf(tfp, "saveFileFormat=%s\n", value->fileFormat);

}

////////////////////////////////////////////////////////////
// Delete the CmdValue object
////////////////////////////////////////////////////////////

void SiSaveAsCmd::freeValue(CmdValue value)
{
   if (value)
      delete (SiSaveCmdValue *)value;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiSaveComponentCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SiSaveComponentCmd.h:  Command class for any component within
// the SiSaveCmdInterface; all it does is tell the SCI to do its
// thing.  Used for simple creation of Option menus.
////////////////////////////////////////////////////////////////

#include "SiSaveComponentCmd.h"
#include "SiSaveCmdInterface.h"

SiSaveComponentCmd::SiSaveComponentCmd(const char *name, int active,
		CmdList *radioList, SiSaveCmdInterface *sci)
	: RadioCmd(name, active, radioList)
{
    _sci = sci;
}

void SiSaveComponentCmd::doit()
{
    if (_value) {
	_sci->runCommand();
    }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiSaveBrowserCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiSaveBrowserCmd:  Accepts a filename from a SingleFileSelection
// dialog (as CmdValue) and stuffs it in the appropriate slot in
// the SiSaveCmdInterface.
/////////////////////////////////////////////////////////////

#include "SiSaveBrowserCmd.h"
#include "SiSaveCmdInterface.h"

SiSaveBrowserCmd::SiSaveBrowserCmd(const char *name, int active,
			SiSaveCmdInterface *sci, int which)
	: NoUndoCmd(name, active)
{
   _sci = sci;
   _which = which;		// r, g, or b
}

void SiSaveBrowserCmd::doit()
{
   _sci->setFilename((char *)_value, _which);
}

void SiSaveBrowserCmd::freeValue(CmdValue value)
{
   delete (char *)value;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create si_save.imake
#define SUBROUTINE si_save
#define MODULE_LIST SiSaveCmdInterface.cc SiSaveDialog.cc SiSaveAsCmd.cc \
 SiSaveComponentCmd.cc SiSaveBrowserCmd.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP

$ Return
$!#############################################################################
