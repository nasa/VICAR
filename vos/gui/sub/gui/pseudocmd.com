$!****************************************************************************
$!
$! Build proc for MIPL module pseudocmd
$! VPACK Version 1.8, Monday, July 07, 1997, 17:35:56
$!
$! Execute by entering:		$ @pseudocmd
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
$ write sys$output "*** module pseudocmd ***"
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
$ write sys$output "Invalid argument given to pseudocmd.com file -- ", primary
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
$   if F$SEARCH("pseudocmd.imake") .nes. ""
$   then
$      vimake pseudocmd
$      purge pseudocmd.bld
$   else
$      if F$SEARCH("pseudocmd.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pseudocmd
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pseudocmd.bld "STD"
$   else
$      @pseudocmd.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pseudocmd.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pseudocmd.com -mixed -
	-s PseudoCmd.cc PseudoDialog.cc PseudoModeCmd.cc LoadPseudoFileCmd.cc -
	   SavePseudoFileCmd.cc SetDeferredCmd.cc ClearMarksCmd.cc -
	-i pseudocmd.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create PseudoCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// PseudoCmd.cc: Command class to execute the PSEUDOCOLOR
//                command.
//////////////////////////////////////////////////////////
#include "PseudoCmd.h"
#include "BasicImageView.h"
#include "Lut.h"
#include "PseudoValue.h"
#include "stdio.h"

PseudoCmd::PseudoCmd ( const char *name, int active, 
	Lut *lutR, Lut *lutG, Lut *lutB ) : NoUndoCmd ( name, active )
{
    _created = FALSE;

    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;
}

void PseudoCmd::doit()
{
   // Allocate pseudocolor value dynamically
   PseudoValue *pseudoValue = (PseudoValue*)_value;

   int *r, *g, *b;

   r = pseudoValue->getRedAsArray();
   g = pseudoValue->getGrnAsArray();
   b = pseudoValue->getBluAsArray();

   _lutR->setAsArray(r);
   _lutG->setAsArray(g);
   _lutB->setAsArray(b);
}      

void PseudoCmd::freeValue(CmdValue value)
{
	if (value)
	   delete (PseudoValue *)value;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PseudoDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////
// PseudoDialog.cc: Dialog box for Pseudocolor Tool
//////////////////////////////////////////////////////////////////////
#include "PseudoDialog.h"
#include "MenuBar.h"
#include "PostSingleFileDialogCmd.h"
#include "LoadPseudoFileCmd.h"
#include "SavePseudoFileCmd.h"
#include "SetDeferredCmd.h"
#include "ClearMarksCmd.h"
#include "PseudoValue.h"
#include "PseudoMarks.h"
#include "MenuCmdList.h"
#include "PseudoCmdInterface.h"
#include "Application.h"
#include "XvicImage.h"
#include "zvproto.h"
#include <string.h>
#include <Xm/Form.h>
#include <stdio.h>

XtResource PseudoDialog::_resources[] = {
 {
   (char *)"numPseudoFiles",
   (char *)"NumPseudoFiles",
   XmRInt,
   sizeof ( int ),
   XtOffset ( PseudoDialog *, _numPseudoFiles ),
   XmRString,
   ( XtPointer ) "7",
 },
 {
   (char *)"filename",
   (char *)"Filename",
   XmRString,
   sizeof ( String ),
   XtOffset ( PseudoDialog *, _filename ),
   XmRString,
   ( XtPointer ) "",
 },
 {
   (char *)"dirUNIX",
   (char *)"DirUNIX",
   XmRString,
   sizeof ( String ),
   XtOffset ( PseudoDialog *, _dirUNIX ),
   XmRString,
   ( XtPointer ) "",
 },
 {
   (char *)"dirVMS",
   (char *)"DirVMS",
   XmRString,
   sizeof ( String ),
   XtOffset ( PseudoDialog *, _dirVMS ),
   XmRString,
   ( XtPointer ) "",
 },
};

PseudoDialog::PseudoDialog(const char *name, Cmd *cmd, Cmd *modeCmd, Widget iw)
        : MenuDialog(name, Default, Visible, Invisible, Visible, Visible)
{
   _iw = iw;
   _cmd = cmd;
   _modeCmd = modeCmd;
   _pseudoValue = NULL;
   _pseudoMarks = NULL;
}

Widget PseudoDialog::createWorkArea(Widget parent)
{
   // Make sure we're using pseudocolor tables
   _modeCmd->execute ( (CmdValue)True );

   _form = XtVaCreateWidget("workArea",
                xmFormWidgetClass, parent,
                NULL );

   XtGetSubresources(_form, (XtPointer)this, "pseudoFiles", "PseudoFiles",
                        _resources, XtNumber(_resources), NULL, 0);

   // Model for LUTs
   _pseudoValue = new PseudoValue;
   // Model for tick marks
   _pseudoMarks = new PseudoMarks;
   CmdInterface *cmdInterface = new PseudoCmdInterface(_form, _cmd, _pseudoValue, _pseudoMarks, _iw);

   // Set the command that switches between deferred and immediate execution
   _setDeferredCmd = new SetDeferredCmd ( "SetDeferredCmd", True, 
					cmdInterface, _applyCmdList, &_apply );

   XtVaSetValues( cmdInterface->baseWidget(), 
                XmNtopAttachment,    XmATTACH_FORM,
                XmNrightAttachment,  XmATTACH_FORM,
                XmNleftAttachment,   XmATTACH_FORM,
                XmNbottomAttachment, XmATTACH_FORM,
                NULL );

   cmdInterface->manage();

   return _form;
}

void PseudoDialog::createMenuPanes()
{
   MenuCmdList *cmdList;

   ////////
   // CREATE FILE PULLDOWN
   ////////

   cmdList = new MenuCmdList("File");
   
   Cmd *loadFileCmd = new LoadPseudoFileCmd ("loadFile", True, _pseudoValue);
   Cmd *loadWinFileCmd = new PostSingleFileDialogCmd ("loadWin", True, loadFileCmd);
   cmdList->add ( loadWinFileCmd );

   Cmd *saveFileCmd = new SavePseudoFileCmd ("saveFile", True, _pseudoValue);
   Cmd *saveWinFileCmd = new PostSingleFileDialogCmd ("saveWin", True, saveFileCmd);
   cmdList->add ( saveWinFileCmd );

   _menuBar->addCommands ( cmdList );
   delete cmdList;

   ////////
   // CREATE OPTIONS PULLDOWN
   ////////

   cmdList = new MenuCmdList("Options");

   cmdList->addCheckBox ( _modeCmd );

   cmdList->addCheckBox ( _setDeferredCmd );

   Cmd *clearMarksCmd = new ClearMarksCmd ( "ClearMarksCmd", True, _pseudoMarks );
   cmdList->add ( clearMarksCmd );

   cmdList->addSeparator();

   Cmd *loadDefFileCmd[10];
   char buf[16][10];
   for (int n = 0; n < _numPseudoFiles; n++) {
      sprintf(buf[n], "file%d", n+1);
      XtGetSubresources(_form, (XtPointer)this, buf[n], "File",
                        _resources, XtNumber(_resources), NULL, 0);
      char *fullFilename = new char[80];
      #if UNIX_OS
      	strcpy(fullFilename, _dirUNIX);
      	strcat(fullFilename, _filename);
      #else
	strcpy(fullFilename, _dirVMS);
        strcat(fullFilename, _filename);	
      #endif
      char *xlateFilename = new char[80];
      zvfilename(fullFilename, xlateFilename, 0);
      loadDefFileCmd[n] = new LoadPseudoFileCmd (buf[n], True, _pseudoValue, xlateFilename);
      cmdList->add ( loadDefFileCmd[n] );
   }

   _menuBar->addCommands ( cmdList );
   delete cmdList;

   loadDefFileCmd[0]->execute();

   // Add two marks at 0 and 255
   int r,g,b;
   _pseudoValue->getRGBDn(0, &r, &g, &b);
   _pseudoMarks->addMark(0, 0, r, g, b);

   _pseudoValue->getRGBDn(255, &r, &g, &b);
   _pseudoMarks->addMark(255, 0, r, g, b);
}

void PseudoDialog::post()
{
    // Make sure we're using pseudocolor tables

   _modeCmd->execute((CmdValue)True);

    MenuDialog::post();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PseudoModeCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// PseudoModeCmd.cc
//	
//	PseudoModeCmd switches between NONE and PSEUDO modes
//
////////////////////////////////////////////////////////////////
#include "PseudoModeCmd.h"
#include "XvicImage.h"

PseudoModeCmd::PseudoModeCmd(const char *name, int active, Widget iw)
		: Cmd(name, active)
{
   _iw = iw;
}

////////////////////////////////////////////////////////////////
// doit()
////////////////////////////////////////////////////////////////
void PseudoModeCmd::doit()
{
   if (_value)
       XtVaSetValues (_iw, XvicNlutType, XvicPSEUDO, NULL );
   else 
       XtVaSetValues (_iw, XvicNlutType, XvicSTRETCH, NULL );
}

////////////////////////////////////////////////////////////////
// undoit()
////////////////////////////////////////////////////////////////
void PseudoModeCmd::undoit()
{
   if (_value)
       XtVaSetValues (_iw, XvicNlutType, XvicSTRETCH, NULL );
   else
       XtVaSetValues (_iw, XvicNlutType, XvicPSEUDO, NULL );

   _value = (CmdValue)(!_value);
   newValue();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LoadPseudoFileCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// LoadPseudoFileCmd: A Command class that loads a IBIS file.  The Command
// value is a dynamically allocated single string, suitable for passing in
// to a PseudoValue subclass.
/////////////////////////////////////////////////////////
#include "LoadPseudoFileCmd.h"
#include "CmdInterface.h"
#include "PseudoValue.h"
#include "ErrorDialogManager.h"
#include <stdio.h>
#include <assert.h>

LoadPseudoFileCmd::LoadPseudoFileCmd(const char *name, int active, 
		PseudoValue *pseudoValue, char *filename)
		: NoUndoCmd(name, active)
{
   _filename = filename;
   _pseudoValue = pseudoValue;
}

void LoadPseudoFileCmd::doit()
{
   assert(_pseudoValue != NULL);

   int status;

   if (_filename) 
	status = _pseudoValue->loadFile(_filename);
   else 
	status = _pseudoValue->loadFile((char *)_value);

   if (status != 1) {
	_pseudoValue->linear(0, 255, 0, 255, 0, 255); // Ramp the tables
	_pseudoValue->setDefRedAsArray(_pseudoValue->getRedAsArray());
	_pseudoValue->setDefGrnAsArray(_pseudoValue->getGrnAsArray());
	_pseudoValue->setDefBluAsArray(_pseudoValue->getBluAsArray());
	char msg[1024];
	if (_filename)
	   sprintf( msg, "Can not process selected file\n%s", _filename );
	else 
	   sprintf( msg, "Can not process selected file\n%s", (char *)_value );
	theErrorDialogManager->post(msg);
   }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SavePseudoFileCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// SavePseudoFileCmd: A Command class that saves pseudocolor tables in  an
// IBIS file format.  The Command value is a dynamically allocated single
// string, suitable for passing in to a PseudoValue subclass.
/////////////////////////////////////////////////////////
#include "SavePseudoFileCmd.h"
#include "CmdInterface.h"
#include "PseudoValue.h"
#include "ErrorDialogManager.h"
#include <stdio.h>
#include <assert.h>

SavePseudoFileCmd::SavePseudoFileCmd(const char *name, int active, PseudoValue *pseudoValue)
		: NoUndoCmd(name, active)
{
   _pseudoValue = pseudoValue;
}

void SavePseudoFileCmd::doit()
{
   assert(_pseudoValue != NULL);

   int status = _pseudoValue->saveFile((char *)_value);

   if (status != 1) {
	char *msg = new char[1024];
	sprintf( msg, "Can not write to selected file\n%s", (char *)_value );
	theErrorDialogManager->post(msg);
	delete [] msg;
   }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetDeferredCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// SetDeferredCmd.cc: Switches between immediate and 
// deferred execution modes.  This command in intended to
// work with CheckBox widget.  If the CheckBox's value is
// True then deferred execution is set, else immediate 
// execution mode is set
////////////////////////////////////////////////////////////////
#include "SetDeferredCmd.h"
#include "CmdInterface.h"


SetDeferredCmd::SetDeferredCmd(const char *name, int active, 
		CmdInterface *interface, CmdList *applyCmdList, 
		Widget *applyButton) 
	: Cmd(name, active)
{
    _interface = interface;
    _applyCmdList = applyCmdList;
    _applyButton = applyButton;
}

////////////////////////////////////////////////////////////////
// doit()
////////////////////////////////////////////////////////////////
void SetDeferredCmd::doit()
{
    if (_value) {
	_interface->setDeferredExec(_applyCmdList);
	if (*_applyButton)
            XtSetSensitive(*_applyButton, True);
    }
    else {
	_interface->setDeferredExec(NULL);
	if (*_applyButton)
            XtSetSensitive(*_applyButton, False);
    }
}

////////////////////////////////////////////////////////////////
// undoit()
////////////////////////////////////////////////////////////////
void SetDeferredCmd::undoit()
{
    if (_value) {
	_interface->setDeferredExec(NULL);
	if (*_applyButton)
            XtSetSensitive(*_applyButton, False);
    }
    else {
	_interface->setDeferredExec(_applyCmdList);
	if (*_applyButton)
            XtSetSensitive(*_applyButton, True);
    }

    _value = (CmdValue)(!_value);
    newValue();

}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ClearMarksCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// ClearMarksCmd: A Command class that clears all the marks off
// the screen except for those with DN 0 and 255.
/////////////////////////////////////////////////////////
#include "ClearMarksCmd.h"
#include "CmdInterface.h"
#include "PseudoMarks.h"

ClearMarksCmd::ClearMarksCmd(const char *name, int active, 
		PseudoMarks *pseudoMarks)
		: NoUndoCmd(name, active)
{
   _pseudoMarks = pseudoMarks;
}

void ClearMarksCmd::doit()
{
	_pseudoMarks->clearMarks();
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pseudocmd.imake
#define SUBROUTINE pseudocmd
#define MODULE_LIST PseudoCmd.cc PseudoDialog.cc PseudoModeCmd.cc \
	LoadPseudoFileCmd.cc SavePseudoFileCmd.cc \
	SetDeferredCmd.cc ClearMarksCmd.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP
#define LIB_P2SUB
$ Return
$!#############################################################################
