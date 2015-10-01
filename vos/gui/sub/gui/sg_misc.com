$!****************************************************************************
$!
$! Build proc for MIPL module sg_misc
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:34
$!
$! Execute by entering:		$ @sg_misc
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
$ write sys$output "*** module sg_misc ***"
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
$ write sys$output "Invalid argument given to sg_misc.com file -- ", primary
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
$   if F$SEARCH("sg_misc.imake") .nes. ""
$   then
$      vimake sg_misc
$      purge sg_misc.bld
$   else
$      if F$SEARCH("sg_misc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sg_misc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sg_misc.bld "STD"
$   else
$      @sg_misc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sg_misc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sg_misc.com -mixed -
	-s SgKeyinCmdInterface.cc SgKeyinDialog.cc SgCmdDialog.cc -
	-i sg_misc.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SgKeyinCmdInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////
// SgKeyinCmdInterface:  Command interface to get string value for cmd.
// Initial values come from Cmd object.  This class is not specific
// to the sage client library and can be reused if needed.
////////////////////////////////////////////////////////
#include "SgKeyinCmdInterface.h"
#include "KeyinView.h"
#include "Cmd.h"
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <stdio.h>

String SgKeyinCmdInterface::_defaults [ ] = {
    (char *)"*label.labelString:	Input:",
     NULL,
};

//////////////////////////////////////////////////////////////
// Constructor
//////////////////////////////////////////////////////////////
SgKeyinCmdInterface::SgKeyinCmdInterface(Widget parent, Cmd *cmd)
			: CmdInterface(cmd)
{
    char *value;
    value = (char *)_cmd->getValue();

    setDefaultResources ( parent, _defaults );

    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent,
		XmNorientation, XmVERTICAL,
		NULL);
    installDestroyHandler();

    _keyinView = new KeyinView(_w, "nameView");
    if (value)
	_keyinView->setFieldValue(value);

    XtAddCallback ( _keyinView->getField(), XmNactivateCallback, 
				&CmdInterface::executeCmdCallback,
				(XtPointer)this );

    _keyinView->manage();
}

//////////////////////////////////////////////////////////////
// Destructor
//////////////////////////////////////////////////////////////
SgKeyinCmdInterface::~SgKeyinCmdInterface()
{
    delete _keyinView;	// Widgets handled by BasicComponent dtor
}

//////////////////////////////////////////////////////////////
// Set the value for the I/F.  
//////////////////////////////////////////////////////////////

void SgKeyinCmdInterface::setValue(CmdValue val)
{
    char *value = (char *)val;

    CmdInterface::setValue(value);	// Removes cmd from deferred list

    if (value) {
	_keyinView->setFieldValue(value);
    }
    else {
	_keyinView->setFieldValue((char *)"");
    }
}


//////////////////////////////////////////////////////////////
// Execute command after creating dynamic value.
//////////////////////////////////////////////////////////////

void SgKeyinCmdInterface::executeCmd(XtPointer)
{
    char *string;

    string = strdup(_keyinView->getFieldValue());
    if (!string || !strlen(string)) return;
   
    // Delete leading blanks

    while (string[0] == ' ')
	for (int i = 0; i < (int)strlen(string); i++ )
           string[i] = string[i+1];

    // Delete trailing blanks

    while ((strlen(string) > 0) && (string[strlen(string)-1] == ' '))
	string[strlen(string)-1] = '\0';

    runCmd((CmdValue)string);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgKeyinDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SgKeyinDialog.cc: A dialog class used to take single string
//      input from the user
////////////////////////////////////////////////////////////////

#include "SgKeyinDialog.h"

SgKeyinDialog::SgKeyinDialog(const char *name, Cmd *cmd)
  : SgCmdDialog(name, cmd, Invisible, Invisible, Invisible, 
		Default, Invisible)
{
    _ci = NULL;
    _command = cmd;
}

CmdInterface *SgKeyinDialog::createCmdInterface(Widget parent, Cmd *cmd)
{
  _ci = new SgKeyinCmdInterface(parent, cmd);
  return _ci;
}  
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgCmdDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// SgCmdDialog.cc: This is an abstract class that implements a 
// dialog that holds a command interface.  The subclasses must
// provide the *command interface*. The *command* itself is created
// outside the class and is passed in via the constructor.
////////////////////////////////////////////////////////////////
#include "SgCmdDialog.h"
#include "Cmd.h"
#include "CmdInterface.h"

#include <Xm/Xm.h>

SgCmdDialog::SgCmdDialog(const char *name, Cmd *cmd, ButtonState showOk,
			 ButtonState showApply,	ButtonState showReset,
			 ButtonState showCancel, ButtonState showHelp)
  : CustomDialog(name, showOk, showApply, showReset, 
		 showCancel, showHelp)
{
    _cmd = cmd; 
    _cmdInterface = NULL;
}

SgCmdDialog::~SgCmdDialog()
{
    if (_cmdInterface)
	delete _cmdInterface;
}

Widget SgCmdDialog::createWorkArea(Widget parent)
{
    _cmdInterface = createCmdInterface(parent, _cmd);

    setCmdIfDeferredExec();

    XtVaSetValues(_cmdInterface->baseWidget(),
		XmNleftAttachment,   XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopAttachment,    XmATTACH_FORM,
		NULL);

    return _cmdInterface->baseWidget();
}

// Subclasses should overwrite this function if they don't need deferred 
// execution (e.g. for very simple one-item dialogs)

void SgCmdDialog::setCmdIfDeferredExec()
{
    _cmdInterface->setDeferredExec(_applyCmdList);    
}

// Reset dialog by setting a new Command.  This new Command should have
// new value in it already.

void SgCmdDialog::resetDialog(Cmd *cmd)
{
    if (_cmdInterface)
	_cmdInterface->setCommand(cmd);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sg_misc.imake
#define SUBROUTINE sg_misc
#define MODULE_LIST SgKeyinCmdInterface.cc SgKeyinDialog.cc SgCmdDialog.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_MOTIF
#define LIB_MOTIFAPP

$ Return
$!#############################################################################
