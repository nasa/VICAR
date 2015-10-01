$!****************************************************************************
$!
$! Build proc for MIPL module tp_cmd
$! VPACK Version 1.9, Tuesday, August 17, 2010, 16:29:41
$!
$! Execute by entering:		$ @tp_cmd
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
$ write sys$output "*** module tp_cmd ***"
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
$ write sys$output "Invalid argument given to tp_cmd.com file -- ", primary
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
$   if F$SEARCH("tp_cmd.imake") .nes. ""
$   then
$      vimake tp_cmd
$      purge tp_cmd.bld
$   else
$      if F$SEARCH("tp_cmd.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tp_cmd
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tp_cmd.bld "STD"
$   else
$      @tp_cmd.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tp_cmd.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tp_cmd.com -mixed -
	-s TpCloseCmd.cc TpContrastCmd.cc TpDeletePointCmd.cc -
	   TpExitStatusCmd.cc TpListPointsCmd.cc TpLoadConfigCmd.cc -
	   TpLoadImageCmd.cc TpRemoveImageCmd.cc TpLoadPointFileCmd.cc -
	   TpClosePointFileCmd.cc TpNumDisplaysCmd.cc TpQuitCmd.cc -
	   TpRotateImageCmd.cc TpSaveAndExitCmd.cc TpSaveConfigAsCmd.cc -
	   TpSaveConfigCmd.cc TpSavePointCmd.cc TpSavePointFileAsCmd.cc -
	   TpSavePointFileCmd.cc TpSetAutofindCmd.cc TpSetPointSizeCmd.cc -
	   TpSetPointSymbolCmd.cc TpShiftCmd.cc TpSwapLockCmd.cc -
	   TpWritePointsCmd.cc TpSetPointColorCmd.cc TpSetPointColorSelCmd.cc -
	   TpSetMatchModeCmd.cc TpSetCursorSymbolCmd.cc -
	   TpSetCursorColorCmd.cc TpSetMatchModeValuesCmd.cc TpPrintCmd.cc -
	   TpRedoMatchIdsCmd.cc TpColorCodeCmd.cc TpSetMatchIdOffsetCmd.cc -
	   TpSetMatchIdNextCmd.cc -
	-i tp_cmd.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create TpCloseCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpCloseCmd.h: Select the point file then call the quit command 
// so that the application may exit.
////////////////////////////////////////////////////////////
#include "TpCloseCmd.h"
#include "TpMatchManager.h"
#include "YesNoDialog.h"
#include "PostSingleFileDialogCmd.h"
#include "TpSavePointFileAsCmd.h"
#include "TpApplication.h"
#include <stdlib.h>

TpCloseCmd::TpCloseCmd(const char *name, int active, TpMatchManager *mm, 
		       Cmd *quitCmd) 
	: NoUndoCmd(name, active)
{
    _matchManager = mm;
    _quitCmd = quitCmd;
}

void TpCloseCmd::saveCallback(void *clientData)
{
    TpCloseCmd *obj = (TpCloseCmd *)clientData;
    obj->save();
}

void TpCloseCmd::save()
{
    char *fileName = _matchManager->getIbisFileName();
    if (fileName && strlen(fileName)) {
	_matchManager->writePointsIbis();
	_quitCmd->execute();
    }
    else {
	Cmd *saveAsFileCmd = new TpSavePointFileAsCmd("saveAs", True, 
						      _matchManager, _quitCmd);
	Cmd *postCmd = new PostSingleFileDialogCmd("SaveAs", True,
						   saveAsFileCmd);
	postCmd->execute();
    }
}

void TpCloseCmd::noSaveCallback(void *clientData)
{
    TpCloseCmd *obj = (TpCloseCmd *)clientData;
    obj->noSave();
}

void TpCloseCmd::noSave()
{
    _matchManager->closePointsIbis();
    //exit(theTpApplication->getExitStatus());
    _quitCmd->execute();
}

void TpCloseCmd::doit()
{
    char *fileName = _matchManager->getIbisFileName();
    Boolean noName = False;
    if (!fileName) {
	fileName = sdup("None");
	noName = True;
    }
    char *msg = new char [33 + strlen(fileName)];
    sprintf (msg, "Do you want to save changes to %s?", fileName);
    if (noName) delete [] fileName;
    
    // Create a two-way dialog.  There is no need to delete
    // it afterwards since it destroys itself when user makes
    // a selection

    new YesNoDialog("save", (void *)this,
		    msg,
		    &TpCloseCmd::saveCallback,
		    &TpCloseCmd::noSaveCallback);

    delete [] msg;
}       
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpContrastCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// TpContrastCmd.cc: Contrast control.
///////////////////////////////////////////////////////////////////
#include "TpContrastCmd.h"
#include "TpContrastValue.h"
#include "ImageData.h"
#include "Lut.h"
#include "StretchFun.h"
#include "XvicImage.h"
#include <stdlib.h>
#include <stdio.h>

TpContrastCmd::TpContrastCmd(const char *name, int active, 
		Widget iw, Widget ziw, Widget pan, ImageData *)
	: Cmd(name, active)
{
    _iw = iw;
    _ziw = ziw;
    _pan = pan;
}

void TpContrastCmd::doit()
{
    TpContrastValue *value = (TpContrastValue *)_value;
    Lut *lut = new Lut;
    stretch_linear(lut, value->getMin(), value->getMax());
    XvicImageSetMonoLUT(_iw, lut->getAsArray());
    XvicImageSetMonoLUT(_ziw, lut->getAsArray());
    XvicImageSetMonoLUT(_pan, lut->getAsArray());
    delete lut;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpDeletePointCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpDeletePointCmd.cc: Saves currently selected match.
/////////////////////////////////////////////////////////////
#include "TpDeletePointCmd.h"
#include "TpMatchManager.h"

TpDeletePointCmd::TpDeletePointCmd(const char *name, int active, 
				   TpMatchManager *matchManager)
	: Cmd(name, active)
{
    _matchManager = matchManager;
}

void TpDeletePointCmd::doit()
{
    _matchManager->deleteCurrentSelection();
}

void TpDeletePointCmd::undoit()
{
    //_matchManager->addDeletedMatch();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpExitStatusCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////////////////
// TpExitStatusCmd.cc: Set application exit status.
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher - July 1997      JPL
//////////////////////////////////////////////////////////////////////////////
#include "TpExitStatusCmd.h"
#include "TpApplication.h"

TpExitStatusCmd::TpExitStatusCmd(const char *name, int active) 
	: NoUndoCmd(name, active)
{
    // Empty
}

void TpExitStatusCmd::doit()
{
    if (_value)
	theTpApplication->setExitStatus(1);
    else 
	theTpApplication->setExitStatus(0);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpListPointsCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpListPointsCmd.cc: Outputs all points to stdout.
/////////////////////////////////////////////////////////////
#include "TpListPointsCmd.h"
#include "TpMatchManager.h"

TpListPointsCmd::TpListPointsCmd(const char *name, int active, 
				 TpMatchManager *matchManager)
	: NoUndoCmd(name, active)
{
    _matchManager = matchManager;
}

void TpListPointsCmd::doit()
{
    _matchManager->listPoints();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpLoadConfigCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpLoadConfigCmd.cc: Load config file.
////////////////////////////////////////////////////////////
#include "TpLoadConfigCmd.h"
#include "TpWindow.h"

TpLoadConfigCmd::TpLoadConfigCmd(const char *name, int active, 
				 TpWindow *window) 
    : NoUndoCmd(name, active)
{
    _window = window;
}

void TpLoadConfigCmd::doit()
{
    _window->loadConfig((char *)_value);
}       

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpLoadImageCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpLoadImageCmd.h: 
////////////////////////////////////////////////////////////
#include "TpLoadImageCmd.h"
#include "TpDisplayer.h"
#include <stdlib.h>

TpLoadImageCmd::TpLoadImageCmd(const char *name, int active, TpDisplayer *d) : 
                    Cmd(name, active)
{
    _displayer = d;
}

void TpLoadImageCmd::doit()
{
    _displayer->addImage((char *)_value);
}       

void TpLoadImageCmd::undoit()
{
    // Empty
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpRemoveImageCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpRemoveImageCmd.h: 
////////////////////////////////////////////////////////////
#include "TpRemoveImageCmd.h"
#include "TpDisplayer.h"
#include <stdlib.h>

TpRemoveImageCmd::TpRemoveImageCmd(const char *name, int active, TpDisplayer *d) : 
                    WarnNoUndoCmd(name, active)
{
    _displayer = d;
}

void TpRemoveImageCmd::doit()
{
    int value = atoi((char *)_value);
    _displayer->deleteImage(value);
}       

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpLoadPointFileCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpLoadPointFileCmd.cc: Load ibis-2 point file.
////////////////////////////////////////////////////////////
#include "TpLoadPointFileCmd.h"
#include "TpMatchManager.h"

TpLoadPointFileCmd::TpLoadPointFileCmd(const char *name, int active, 
				       TpMatchManager *mm) 
    : NoUndoCmd(name, active)
{
    _matchManager = mm;
}

void TpLoadPointFileCmd::doit()
{
    _matchManager->readPointsIbis((char *)_value);
}       

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpClosePointFileCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpClosePointFileCmd.cc: Close ibis-2 point file.
////////////////////////////////////////////////////////////
#include "TpClosePointFileCmd.h"
#include "TpMatchManager.h"
#include "TpCloseCmd.h"

TpClosePointFileCmd::TpClosePointFileCmd(const char *name, int active, 
				       TpMatchManager *mm) 
    : NoUndoCmd(name, active)
{
    _matchManager = mm;
}

void TpClosePointFileCmd::doit()
{
    if (_matchManager->isDirty()) {
	Cmd *saveCmd = new TpCloseCmd ("close", True,
				       _matchManager, (Cmd *)this);
	saveCmd->execute();
    }
    else {
	_matchManager->closePointsIbis();
    }
}       

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpNumDisplaysCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpNumDisplaysCmd.cc: Set number of displays.
////////////////////////////////////////////////////////////
#include "TpNumDisplaysCmd.h"
#include "TpDisplayer.h"

TpNumDisplaysCmd::TpNumDisplaysCmd(const char *name, int active, 
		CmdValue starting_value, CmdList *radioList,
		TpDisplayer *displayer, int numDisplays)
	: RadioCmd(name, active, starting_value, radioList)
{
    _displayer = displayer;
    _numDisplays = numDisplays;
}

void TpNumDisplaysCmd::doit()
{
    if (_value)
	_displayer->setNumDisplays(_numDisplays);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQuitCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////////////////
// TpQuitCmd.cc: Exit the application if there is no tiepoint file to save.
// Otherwise execute TpCloseCmd and provide it with 'this' pointer so that
// TpCloseCmd object could call us again when it is done with closing project.
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher - July 1997      JPL
//////////////////////////////////////////////////////////////////////////////
#include "TpQuitCmd.h"
#include "TpMatchManager.h"
#include "TpCloseCmd.h"
#include "TpApplication.h"

TpQuitCmd::TpQuitCmd(const char *name, int active, TpMatchManager *mm) 
	: NoUndoCmd(name, active)
{
    _matchManager = mm;
}

void TpQuitCmd::doit()
{
    if (_matchManager->isDirty()) {
	Cmd *saveCmd = new TpCloseCmd ("close", True, 
				       _matchManager, (Cmd *)this);
	saveCmd->execute();
    }
    else {
	exit(theTpApplication->getExitStatus());
    }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpRotateImageCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpRotateImageCmd.cc: Rotate image,
////////////////////////////////////////////////////////////
#include "TpRotateImageCmd.h"
#include "TpSubDisplayer.h"
#include "ErrorManager.h"

TpRotateImageCmd::TpRotateImageCmd(const char *name, int active, CmdList *radioList,
		TpSubDisplayer *subDisplayer, RotationType rotation)
	: RadioCmd(name, active, radioList)
{
    _subDisplayer = subDisplayer;
    _rotation = rotation;
}

void TpRotateImageCmd::doit()
{
    if (_value) {
	if (_subDisplayer)
	    _subDisplayer->setRotationMode(_rotation);
	else 
	    theErrorManager->process(Error, "Change Rotation", 
			"You are trying to rotate an undefined image"); 
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSaveAndExitCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpSaveAndExitCmd.cc: Saves points into IBIS-2 file.  
/////////////////////////////////////////////////////////////
#include "TpSaveAndExitCmd.h"
#include "TpMatchManager.h"
#include "TpSavePointFileAsCmd.h"
#include "PostSingleFileDialogCmd.h"
#include "TpApplication.h"

TpSaveAndExitCmd::TpSaveAndExitCmd(const char *name, int active, 
				   TpMatchManager *matchManager)
    : NoUndoCmd(name, active)
{
    _matchManager = matchManager;
}

void TpSaveAndExitCmd::doit()
{
    if (_matchManager->getIbisFileName())
	_matchManager->writePointsIbis();
    else {
	Cmd *saveCmd = new TpSavePointFileAsCmd("SavePointFileAs",
						True, _matchManager);
	Cmd *cmd = new PostSingleFileDialogCmd("Save Point File As",
					       True, saveCmd);
	cmd->execute();
    }

    exit(theTpApplication->getExitStatus());
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSaveConfigAsCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpSaveConfigAsCmd.cc: Saves all points in IBIS-2 file.
/////////////////////////////////////////////////////////////
#include "TpSaveConfigAsCmd.h"
#include "TpWindow.h"
#include "QuestionDialogManager.h"
#include <stdio.h>
#ifndef __VMS
  #include <sys/types.h>
  #include <sys/stat.h>
#endif

#define TP_CONFIG_SAVE_AS_QUESTION "The file already exists.\nOverwrite?"

TpSaveConfigAsCmd::TpSaveConfigAsCmd(const char *name, int active, TpWindow *window)
    : NoUndoCmd(name, active)
{
    _window = window;
}

void TpSaveConfigAsCmd::okCallback(void *clientData)
{
    TpSaveConfigAsCmd *obj = (TpSaveConfigAsCmd *)clientData;
    obj->NoUndoCmd::execute(obj->_tmpValue);
}

void TpSaveConfigAsCmd::execute(CmdValue new_value)
{
    _tmpValue = new_value;
#ifdef __VMS
    // VMS creates new versions so there's no need to ask
    NoUndoCmd::execute(new_value);
#else
    struct stat file_info;
    if (stat((char *)new_value, &file_info) == 0) {  // File exists
	theQuestionDialogManager->post(TP_CONFIG_SAVE_AS_QUESTION,
				       (void *) this,
				       &TpSaveConfigAsCmd::okCallback);
    }
    else
	NoUndoCmd::execute(new_value);
#endif
}

void TpSaveConfigAsCmd::doit()
{
    _window->saveConfig((char *)_value);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSaveConfigCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpSaveConfigCmd.cc: Saves configuration file.
/////////////////////////////////////////////////////////////
#include "TpSaveConfigCmd.h"
#include "TpWindow.h"
#include "TpSaveConfigAsCmd.h"
#include "PostSingleFileDialogCmd.h"

TpSaveConfigCmd::TpSaveConfigCmd(const char *name, int active, TpWindow *window)
    : NoUndoCmd(name, active)
{
    _window = window;
}

void TpSaveConfigCmd::doit()
{
    char *config = _window->getConfigFileName();
    if (config && strlen(config))
	_window->saveConfig();
    else {
	Cmd *saveCmd = new TpSaveConfigAsCmd("SaveConfigAs", True, _window);
	Cmd *cmd = new PostSingleFileDialogCmd("Save Config As", True,saveCmd);
	cmd->execute();
    }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSavePointCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpSavePointCmd.cc: Saves currently selected match.
/////////////////////////////////////////////////////////////
#include "TpSavePointCmd.h"
#include "TpMatchManager.h"

TpSavePointCmd::TpSavePointCmd(const char *name, int active, 
			       TpMatchManager *matchManager)
	: Cmd(name, active)
{
    _matchManager = matchManager;
}

void TpSavePointCmd::doit()
{
    _matchManager->addMatch();
    deactivate();
}

void TpSavePointCmd::undoit()
{
    _matchManager->deleteLastAddedMatch();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSavePointFileAsCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpSavePointFileAsCmd.cc: Saves all points in IBIS-2 file.
/////////////////////////////////////////////////////////////
#include "TpSavePointFileAsCmd.h"
#include "TpMatchManager.h"
#include "QuestionDialogManager.h"
#include <stdio.h>
#ifndef __VMS
  #include <sys/types.h>
  #include <sys/stat.h>
#endif

#define TP_POINTS_SAVE_AS_QUESTION "The file already exists.\nOverwrite?"

TpSavePointFileAsCmd::TpSavePointFileAsCmd(const char *name, int active, 
			       TpMatchManager *matchManager, Cmd *quitCmd)
	: NoUndoCmd(name, active)
{
    _matchManager = matchManager;
    _quitCmd = quitCmd;
}

void TpSavePointFileAsCmd::okCallback(void *clientData)
{
    TpSavePointFileAsCmd *obj = (TpSavePointFileAsCmd *)clientData;
    obj->NoUndoCmd::execute(obj->_tmpValue);
}

void TpSavePointFileAsCmd::execute(CmdValue new_value)
{
    _tmpValue = new_value;
#ifdef __VMS
    // VMS creates new versions so there's no need to ask
    NoUndoCmd::execute(new_value);
#else
    struct stat file_info;
    if (stat((char *)new_value, &file_info) == 0) {  // File exists
	theQuestionDialogManager->post(TP_POINTS_SAVE_AS_QUESTION,
				       (void *) this,
				       &TpSavePointFileAsCmd::okCallback);
    }
    else
	NoUndoCmd::execute(new_value);
#endif
}

void TpSavePointFileAsCmd::doit()
{
    _matchManager->writePointsIbis((char *)_value);
    if (_quitCmd) _quitCmd->execute();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSavePointFileCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// TpSavePointFileCmd.cc: Saves points into IBIS-2 file.  
/////////////////////////////////////////////////////////////
#include "TpSavePointFileCmd.h"
#include "TpMatchManager.h"
#include "TpSavePointFileAsCmd.h"
#include "PostSingleFileDialogCmd.h"

TpSavePointFileCmd::TpSavePointFileCmd(const char *name, int active, 
				       TpMatchManager *matchManager)
    : NoUndoCmd(name, active)
{
    _matchManager = matchManager;
}

void TpSavePointFileCmd::doit()
{
    if (_matchManager->getIbisFileName())
	_matchManager->writePointsIbis();
    else {
	Cmd *saveCmd = new TpSavePointFileAsCmd("SavePointFileAs",
						True, _matchManager);
	Cmd *cmd = new PostSingleFileDialogCmd("Save Point File As",
					       True, saveCmd);
	cmd->execute();
    }

}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetAutofindCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpSetAutofindCmd.cc: Set number of displays.
////////////////////////////////////////////////////////////
#include "TpSetAutofindCmd.h"
#include "TpMatchManager.h"

TpSetAutofindCmd::TpSetAutofindCmd(const char *name, int active, 
		CmdValue starting_value, CmdList *radioList,
		TpMatchManager *mm, TpAutofindMode autofindMode)
    : RadioCmd(name, active, starting_value, radioList)
{
    _matchManager = mm;
    _autofindMode = autofindMode;
}

void TpSetAutofindCmd::doit()
{
    if (_value)
	_matchManager->setAutofindMode(_autofindMode);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetPointSizeCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetPointSizeCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetPointSizeCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>
#include <stdio.h>

TpSetPointSizeCmd::TpSetPointSizeCmd(const char *name, int active, 
				     CmdList *radCmdList,
				     int size, TpMatchManager *mm)
    : RadioCmd(name, active, radCmdList)
{
    _size = size;
    _matchManager = mm;
}

void TpSetPointSizeCmd::doit()
{
    if (_value) {
	_matchManager->setPointDimensions(_size, _size);
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetPointSymbolCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetPointSymbolCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetPointSymbolCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>
#include <stdio.h>

TpSetPointSymbolCmd::TpSetPointSymbolCmd(const char *name, int active, 
					 CmdList *radCmdList, 
					 TpPointSymbolShapeType shape, 
					 TpMatchManager *mm)
    : RadioCmd(name, active, radCmdList)
{
    _shape = shape;
    _matchManager = mm;
}

void TpSetPointSymbolCmd::doit()
{
    if (_value) {
	_matchManager->setPointShape(_shape);
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpShiftCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpShiftCmd.h: Shift subdisplays either to the left or to 
// the right depending on the value passed in the constructor.
////////////////////////////////////////////////////////////
#include "TpShiftCmd.h"
#include "TpDisplayer.h"
#include <stdlib.h>

TpShiftCmd::TpShiftCmd(const char *name, int active, 
		TpDisplayer *d, Boolean left) 
	: Cmd(name, active)
{
    _displayer = d;
    _leftShift = left;
}

void TpShiftCmd::doit()
{
    if (_leftShift) 
	_displayer->shiftLeft();
    else 
	_displayer->shiftRight();
}       

void TpShiftCmd::undoit()
{
    if (_leftShift) 
        _displayer->shiftRight();
    else
        _displayer->shiftLeft();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSwapLockCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpSwapLockCmd.cc: Set image swap lock option on or off.
////////////////////////////////////////////////////////////
#include "TpSwapLockCmd.h"
#include "TpDisplayer.h"

TpSwapLockCmd::TpSwapLockCmd(const char *name, int active,
		TpDisplayer *displayer, int i)
	: Cmd(name, active)
{
    _displayer = displayer;
    _imageNo = i;
}

void TpSwapLockCmd::doit()
{
    if (_value)
	_displayer->setLock(_imageNo);
    else 
	_displayer->unSetLock(_imageNo);
}

void TpSwapLockCmd::undoit()
{
//    if ((int)_value)
//        _displayer->setLock(_imageNo);
//    else
//        _displayer->unSetLock(_imageNo);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpWritePointsCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// TpWritePointsCmd.h: Write collected points
// to the ASCII file.
////////////////////////////////////////////////////////////
#include "TpWritePointsCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>

TpWritePointsCmd::TpWritePointsCmd(const char *name, int active, 
		TpMatchManager *mm) 
	: NoUndoCmd(name, active)
{
    _matchManager = mm;
}

void TpWritePointsCmd::doit()
{
    _matchManager->writePointsIbis();
}       
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetPointColorCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetPointColorCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetPointColorCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>
#include <stdio.h>

TpSetPointColorCmd::TpSetPointColorCmd(const char *name, int active, CmdValue value,
				       TpMatchManager *mm)
    : Cmd(name, active, value)
{
    _matchManager = mm;
}

void TpSetPointColorCmd::doit()
{
    _matchManager->setPointColor((char *)_value);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetPointColorSelCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetPointColorSelCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetPointColorSelCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>
#include <stdio.h>

TpSetPointColorSelCmd::TpSetPointColorSelCmd(const char *name, int active, 
					     CmdValue value, 
					     TpMatchManager *mm)
    : Cmd(name, active, value)
{
    _matchManager = mm;
}

void TpSetPointColorSelCmd::doit()
{
    _matchManager->setPointColorSel((char *)_value);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetMatchModeCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetMatchModeCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetMatchModeCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>
#include <stdio.h>

TpSetMatchModeCmd::TpSetMatchModeCmd(const char *name, int active, 
				     CmdValue starting_value,
				     CmdList *radCmdList, 
				     TpMatchMode matchMode, 
				     TpMatchManager *mm)
    : RadioCmd(name, active, starting_value, radCmdList)
{
    _matchMode = matchMode;
    _matchManager = mm;
}

void TpSetMatchModeCmd::doit()
{
    if (_value) {
	_matchManager->setMatchMode(_matchMode);
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetCursorSymbolCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetCursorSymbolCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetCursorSymbolCmd.h"
#include "TpDisplayer.h"
#include <stdlib.h>
#include <stdio.h>

TpSetCursorSymbolCmd::TpSetCursorSymbolCmd(const char *name, int active, 
					 CmdList *radCmdList, 
					 TpDisplayer *disp)
    : RadioCmd(name, active, radCmdList)
{
    _cursor = sdup(name);
    _displayer = disp;

    if (!strcmp(_displayer->getCursor(), _cursor)) {
	_value = (CmdValue)_cursor;
	newValue();
    }
}

void TpSetCursorSymbolCmd::doit()
{
    if (_value) {
	_displayer->setCursor(_cursor);
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetCursorColorCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetCursorColorCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetCursorColorCmd.h"
#include "TpDisplayer.h"
#include <stdlib.h>
#include <stdio.h>

TpSetCursorColorCmd::TpSetCursorColorCmd(const char *name, int active, 
					 CmdValue value, TpDisplayer *d)
    : Cmd(name, active, value)
{
    _displayer = d;
}

void TpSetCursorColorCmd::doit()
{
    _displayer->setCursorColor((char *)_value);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetMatchModeValuesCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetMatchModeValuesCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetMatchModeValuesCmd.h"
#include "TpMatchManager.h"
#include "TpMatchModeValues.h"
#include <stdlib.h>
#include <stdio.h>

TpSetMatchModeValuesCmd::TpSetMatchModeValuesCmd(const char *name, int active, 
		 CmdValue value, TpMatchManager *mm)
    : Cmd(name, active, value)
{
    _matchManager = mm;
}

void TpSetMatchModeValuesCmd::doit()
{
    TpMatchModeValues *value = (TpMatchModeValues *)_value;
    _matchManager->setMatchModeValues(value);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPrintCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpPrintCmd.cc: This is a command that allows user to print visual part of 
// tp images as ps files or save them vicar rgb files.  The value for this 
// command should be provided via the print dialog.
///////////////////////////////////////////////////////////////////////////////
#include "TpPrintCmd.h"
#include "TpDisplayer.h"
#include "TpSubDisplayer.h"
#include "TpImageView.h"
#include "ImageData.h"
#include "Application.h"
#include "WidgetSnapshot.h"
#include "VicarToPs.h"
#include "XvicImage.h"          // for Xvic functions
#include "XImageToVicar.h"
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#if !defined(__VMS)
  #include <unistd.h>
#else
  #include <unixlib.h>
#endif

TpPrintCmd::TpPrintCmd(const char *name, int active, TpDisplayer *displayer,
		       SgPrintValue *printValue)
    : NoUndoCmd(name, active)
{
    _displayer = displayer;
    _value = (CmdValue)printValue;
    _printValue = NULL;
}

void TpPrintCmd::doit()
{
    theApplication->setBusyCursor();

    _printValue = (SgPrintValue *)_value;

    const PaperSize *paperSize;
    if (_printValue->getPaperSize() == A2)
	paperSize = &PaperA2;
    if (_printValue->getPaperSize() == A3)
        paperSize = &PaperA3;
    if (_printValue->getPaperSize() == A4)
        paperSize = &PaperA4;
    if (_printValue->getPaperSize() == LETTER)
        paperSize = &PaperLetter;
    if (_printValue->getPaperSize() == LEGAL)
        paperSize = &PaperLegal;

    for (int i = 0; i < _displayer->getNumImages(); i++) {

	char title[1024];
	if ((_printValue->getFileType() == PS_FILE) ||
	    (_printValue->getPrintTo() == DEST_PRINTER)) {
	    
	    // Get the time in seconds
	    
	    time_t aclock;
	    time(&aclock);
	    struct tm *newtime;
	    newtime = localtime(&aclock);
	    char *currDate = sdup(asctime(newtime));
	    char *date = &currDate[4];

	    sprintf(title, "Image: %s       Date: %s",
		    _displayer->getImage(i)->getImageData()->getInputDataSourceName(), 
		    date);
	}

	char red[1024], grn[1024], blu[1024], ps[1024];

	if ((_printValue->getPrintTo() == DEST_FILE)) {
	    sprintf(red, "%s%d.red", _printValue->getFilename(), i);
	    sprintf(grn, "%s%d.grn", _printValue->getFilename(), i);
	    sprintf(blu, "%s%d.blu", _printValue->getFilename(), i); 
	    sprintf(ps, "%s%d.ps", _printValue->getFilename(), i);
	}
	else {
#if defined(__VMS)
            sprintf(red, "%s_%d_%d.red", "tp", getpid() % 1000, i);
            sprintf(grn, "%s_%d_%d.grn", "tp", getpid() % 1000, i);
            sprintf(blu, "%s_%d_%d.blu", "tp", getpid() % 1000, i);
            sprintf(ps, "%s_%d_%d.ps", "tp", getpid() % 1000, i);
#else
	    sprintf(red, "/tmp/%s%d%d.red", "tp", getpid(), i);
            sprintf(grn, "/tmp/%s%d%d.grn", "tp", getpid(), i);
            sprintf(blu, "/tmp/%s%d%d.blu", "tp", getpid(), i);
            sprintf(ps, "/tmp/%s%d%d.ps", "tp", getpid(), i);
#endif
        }

	Widget iw = _displayer->getImage(i)->getImageView()->getWidget();
	unsigned char wpp;
	XtVaGetValues(iw, XvicNworkProcPolicy, &wpp, NULL);
	XtVaSetValues(iw, XvicNworkProcPolicy, XvicNONE, NULL);
	XImageHandler *imgHandler = takeWidgetSnapshot(iw);
	writeXImageToVicarFile(imgHandler, red, grn, blu, 0, 0, 0, 0);
	XtVaSetValues(iw, XvicNworkProcPolicy, wpp, NULL);

	if( (_printValue->getFileType() == PS_FILE) ||
	    (_printValue->getPrintTo() == DEST_PRINTER)) { 
	    vicImageToPostScript(grn, ps, title, *paperSize);

	    char command[1024];
#ifndef __VMS
	    sprintf(command, "rm %s", red);
#else
	    sprintf(command, "delete %s;*", red);
#endif
	    system(command);

#ifndef __VMS
	    sprintf(command, "rm %s", grn);
#else
            sprintf(command, "delete %s;*", grn);
#endif
	    system(command);

#ifndef __VMS
	    sprintf(command, "rm %s", blu);
#else
            sprintf(command, "delete %s;*", blu);
#endif
	    system(command);

	    if( _printValue->getPrintTo() == DEST_PRINTER) { 
		sprintf(command, "%s %s", _printValue->getPrinterCmd(), ps);
		system(command);
#ifndef __VMS
		sprintf(command, "rm %s", ps);
#else
		sprintf(command, "delete %s;*", ps);
#endif
		system(command);
	    }
	}
	freeWidgetSnapshot(imgHandler);
    }

    theApplication->removeBusyCursor();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpRedoMatchIdsCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpRedoMatchIdsCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpRedoMatchIdsCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>
#include <stdio.h>

TpRedoMatchIdsCmd::TpRedoMatchIdsCmd(const char *name, int active, 
				     TpMatchManager *mm)
    : WarnNoUndoCmd(name, active)
{
    _matchManager = mm;
}

void TpRedoMatchIdsCmd::doit()
{
    _matchManager->redoIds();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpColorCodeCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpColorCodeCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpColorCodeCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>
#include <stdio.h>

TpColorCodeCmd::TpColorCodeCmd(const char *name, int active, TpMatchManager *mm)
    : Cmd(name, active)
{
    _matchManager = mm;
}

void TpColorCodeCmd::doit()
{
    _matchManager->colorCodePointsPoint(0);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetMatchIdOffsetCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetMatchIdOffsetCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetMatchIdOffsetCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>

TpSetMatchIdOffsetCmd::TpSetMatchIdOffsetCmd(const char *name, int active, 
			CmdValue value, TpMatchManager *mm)
    : Cmd(name, active, value)
{
    _matchManager = mm;
}

void TpSetMatchIdOffsetCmd::doit()
{
    char *s = (char *)_value;
    int startId = atoi(s);
    _matchManager->setStartId(startId);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSetMatchIdNextCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpSetMatchIdNextCmd.cc: 
///////////////////////////////////////////////////////////////////////////////
#include "TpSetMatchIdNextCmd.h"
#include "TpMatchManager.h"
#include <stdlib.h>

TpSetMatchIdNextCmd::TpSetMatchIdNextCmd(const char *name, int active, 
			CmdValue value, TpMatchManager *mm)
    : Cmd(name, active, value)
{
    _matchManager = mm;
}

void TpSetMatchIdNextCmd::doit()
{
    char *s = (char *)_value;
    int startId = atoi(s);
    _matchManager->setNextId(startId);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tp_cmd.imake
#define SUBROUTINE tp_cmd

#define MODULE_LIST \
	TpSavePointCmd.cc TpDeletePointCmd.cc TpListPointsCmd.cc \
	TpQuitCmd.cc TpCloseCmd.cc TpShiftCmd.cc TpLoadImageCmd.cc \
	TpRemoveImageCmd.cc TpRotateImageCmd.cc TpSetMatchIdNextCmd.cc
#define MODULE_LIST2 \
	TpWritePointsCmd.cc TpNumDisplaysCmd.cc TpSwapLockCmd.cc \
	TpSavePointFileCmd.cc TpSavePointFileAsCmd.cc TpColorCodeCmd.cc \
	TpLoadPointFileCmd.cc TpClosePointFileCmd.cc TpExitStatusCmd.cc
#define MODULE_LIST3 \
	TpSaveAndExitCmd.cc TpContrastCmd.cc TpSetAutofindCmd.cc \
	TpSetPointSymbolCmd.cc TpSetPointSizeCmd.cc TpRedoMatchIdsCmd.cc \
	TpSaveConfigCmd.cc TpSaveConfigAsCmd.cc TpLoadConfigCmd.cc
#define MODULE_LIST4 \
	TpSetPointColorCmd.cc TpSetPointColorSelCmd.cc TpSetMatchModeCmd.cc \
	TpSetCursorSymbolCmd.cc TpSetCursorColorCmd.cc TpPrintCmd.cc \
	TpSetMatchModeValuesCmd.cc TpSetMatchIdOffsetCmd.cc

#define P2_SUBLIB

#define MAIN_LANG_C_PLUS_PLUS
#define USES_C_PLUS_PLUS
#define CCC_TEMPLATES

#define LIB_MOTIF
#define LIB_MOTIFAPP
#define LIB_GUISUB

$ Return
$!#############################################################################
