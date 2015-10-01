$!****************************************************************************
$!
$! Build proc for MIPL module tp_misc
$! VPACK Version 1.9, Thursday, August 19, 2010, 09:49:14
$!
$! Execute by entering:		$ @tp_misc
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
$ write sys$output "*** module tp_misc ***"
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
$ write sys$output "Invalid argument given to tp_misc.com file -- ", primary
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
$   if F$SEARCH("tp_misc.imake") .nes. ""
$   then
$      vimake tp_misc
$      purge tp_misc.bld
$   else
$      if F$SEARCH("tp_misc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tp_misc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tp_misc.bld "STD"
$   else
$      @tp_misc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tp_misc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tp_misc.com -mixed -
	-s TpApplication.cc TpWindow.cc TpDisplayer.cc TpSubDisplayer.cc -
	   ImageOverlayView.cc TpImageView.cc TpMatchManager.cc TpMatch.cc -
	   DrawObject.cc TpPoint.cc TpPointModel.cc TpZoomControl.cc -
	   TpContrastControl.cc TpPosView.cc TpCursorModel.cc TpImageInfo.cc -
	   TpImageReference.cc TpSelectionMgr.cc TpWedgeOverlayView.cc -
	   TpMatchBrowseControl.cc TpAutofindRes.cc TpMatchModeRes.cc -
	   TpMatchModeValuesCmdIf.cc TpStripPanTool.cc -
	-i tp_misc.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create TpApplication.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// TpApplication.cc 
///////////////////////////////////////////////////////////////////////////////
#include "TpApplication.h"
#include "MainWindow.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>

TpApplication *theTpApplication = NULL;

// The subclass should contain all of these options.
// If you are updating record, always make sure 
// your changes are reflected in all the subclasses

XrmOptionDescRec  TpApplication::_options[] = {
 {(char *)"-pfile",	(char *)"*pfile", XrmoptionSepArg, NULL},
 {(char *)"-config",    (char *)"*config", XrmoptionSepArg, NULL},
 {(char *)"-v", 	(char *)"*verbose", XrmoptionSepArg, NULL},
};

XtResource TpApplication::_resources [] = {
  {
        (char *)XtpNpfile,
        (char *)XtpCPfile,
        XmRString,
        sizeof(String),
        XtOffset(TpApplication *, _pfile),
        XmRImmediate,
        (XtPointer) 0,
  },
  {
        (char *)XtpNconfig,
        (char *)XtpCConfig,
        XmRString,
        sizeof(String),
        XtOffset(TpApplication *, _config),
        XmRImmediate,
        (XtPointer) 0,
  },
  {
	(char *)XtpNverbose,
	(char *)XtpCVerbose,
	XmRBoolean,
	sizeof(Boolean),
	XtOffset(TpApplication *, _verbose),
	XmRImmediate,
	(XtPointer) False,
  },
};

TpApplication::TpApplication ( const char *appClassName ) 
	: Application ( appClassName )
{
    // Set the global Application pointer

    theTpApplication = this;

    // Initialize data members

    _pfile = NULL;
    _config = NULL;
    _verbose = False;
    _exitStatus = 0;
}

void TpApplication::initialize_hook()
{
    XtGetApplicationResources ( _w,
                (XtPointer) this,
                _resources,
                XtNumber (_resources),
                NULL, 0 );
}

TpApplication::~TpApplication()
{
    // Empty
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpWindow.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
// TpWindow.cc: Main window
///////////////////////////////////////////////////////////////
#include "Application.h"
#include "TpWindow.h"
#include "TpImageReference.h"
#include "TpDisplayer.h"
#include "SgButtonPanel.h"
#include "TpMatchBrowseControl.h"
#include "TpMatchManager.h"
#include "ErrorManager.h"
#include "PrefManager.h"
#include "SgResourceConverter.h"

#include "MenuCmdList.h"
#include "TpSavePointFileCmd.h"
#include "TpSavePointFileAsCmd.h"
#include "TpLoadPointFileCmd.h"
#include "TpClosePointFileCmd.h"
#include "TpLoadConfigCmd.h"
#include "TpSaveConfigCmd.h"
#include "TpSaveConfigAsCmd.h"
#include "TpShiftCmd.h"
#include "PostSingleFileDialogCmd.h"
#include "TpLoadImageCmd.h"
#include "TpRemoveImageCmd.h"
#include "TpSavePointCmd.h"
#include "TpDeletePointCmd.h"
#include "TpListPointsCmd.h"
#include "TpRedoMatchIdsCmd.h"
#include "TpAutoSyncPointsCmd.h"
#include "TpPrintCmd.h"
#include "TpSetMatchModeCmd.h"
#include "TpSetAutofindCmd.h"

#include "TpDisplayModeDialog.h"
#include "TpAutofindResultsDialog.h"
#include "TpMatchModeResultsDialog.h"
#include "TpQualFormatDialog.h"
#include "TpPointEditorOptsDialog.h"
#include "TpPointSymbolsDialog.h"
#include "TpPointTagsDialog.h"
#include "TpCursorSymbolsDialog.h"
#include "TpAutofindDialog.h"
#include "TpMatchModeDialog.h"
#include "SgPrintDialog.h"
#include "SgKeyinDialog.h"

#include "PostDialogCmd.h"
#include "TpSaveAndExitCmd.h"
#include "TpQuitCmd.h"
#include "TpExitStatusCmd.h"
#include "UndoCmd.h"
#include "HelpOnContextCmd.h"
#include "HelpSelfCmd.h"
#include "PrintWidgetTreeCmd.h"
#include "NoOpCmd.h"

#include <Xm/AtomMgr.h>
#include <Xm/Protocols.h>
#include <Xm/Form.h>

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

XtResource TpWindow::_resources[] = {
  {
   (char *)"enablePrintWidgetTree",
   (char *)"EnablePrintWidgetTree",
   XmRBoolean,
   sizeof(Boolean),
   XtOffsetOf(TpWindow, _enablePrintWidgetTree),
   XmRImmediate,
   (XtPointer) False,
  },
  {
   (char *)"enableSetSpecialStatus",
   (char *)"EnableSetSpecialStatus",
   XmRBoolean,
   sizeof(Boolean),
   XtOffsetOf(TpWindow, _enableSetSpecialStatus),
   XmRImmediate,
   (XtPointer) False,
  },
}; 

TpWindow::TpWindow(const char *name) : MenuWindow(name)
{
    _config = NULL;
}

TpWindow::~TpWindow()
{
    if (_config) delete [] _config;
}

void TpWindow::initErrorManager() const
{
    String appName, appClass;
    XtGetApplicationNameAndClass(theApplication->display(),
			&appName, &appClass);
    char *emTitle = new char [strlen(appName) + 15];
    sprintf(emTitle, "%s Info Window", appName);
    ErrorManager *errMgr = new ErrorManager(emTitle);
    delete [] emTitle;
}

void TpWindow::initPrefManager() const
{
    PrefManager *pmgr = new PrefManager();
}

void TpWindow::loadConfig(char *filename)
{
    if (filename && strlen(filename)) {
	if (_config) delete [] _config;
	_config = sdup(filename);
    }
    if (thePrefManager->loadPrefFile(_config) != 0)
	fprintf(stderr, "Couldn't load prefs file %s\n", _config);
}

void TpWindow::saveConfig(char *filename)
{
    if (filename && strlen(filename)) {
	if (_config) delete [] _config;
	_config = sdup(filename);
    }
    if (_config && strlen(_config)) 
	thePrefManager->savePrefFile(_config);
    else
	fprintf(stderr, "Invalid config filename!");
}

Widget TpWindow::createWorkArea(Widget parent)
{
    SgResourceConverter::registerStringToPixmapConverter();

    // Install error manager

    initErrorManager();

    // Install preferences manager

    initPrefManager();

    XtGetApplicationResources(theApplication->baseWidget(), (XtPointer)this,
			_resources, XtNumber(_resources), NULL, 0);

    Widget form = XtCreateWidget(_name, xmFormWidgetClass, parent, NULL, 0);

    TpMatchManager *matchManager = new TpMatchManager(this);
    TpImageReference *ref = new TpImageReference(form, "imageReference");
    TpDisplayer *tpDisplayer = new TpDisplayer(form, "tpDisplayer", 
					       matchManager, ref);
    matchManager->setDisplayer(tpDisplayer);

    _savePointCmd = new TpSavePointCmd("Save Point", True, matchManager);

    _deletePointCmd = new TpDeletePointCmd("Delete Point", True, matchManager);

    _listPointsCmd = new TpListPointsCmd("List Points", True, matchManager);

    _redoMatchIdsCmd = new TpRedoMatchIdsCmd("Redo Gen Quals", True,
					     matchManager);
    _autoSyncPointsCmd = new TpAutoSyncPointsCmd("Auto Sync Points", True,
						 matchManager);
    _autoSyncPointsCmd->execute((CmdValue)False);

    TpSetAutofindCmd *findCmd[2];
    _findRadioList = new CmdList;
    int i = 0;
    findCmd[i++] = new TpSetAutofindCmd("affine", True,
		(CmdValue)(matchManager->getAutofindMode() == AFFINE),
		_findRadioList, matchManager, AFFINE);
    findCmd[i++] = new TpSetAutofindCmd("manual", True,
                (CmdValue)(matchManager->getAutofindMode() == MANUAL),
                _findRadioList, matchManager, MANUAL);

    TpSetMatchModeCmd *modeCmd[2];
    _modeRadioList = new CmdList;
    i = 0;
    modeCmd[i++] = new TpSetMatchModeCmd("AutoCorr", True,
                (CmdValue)(matchManager->getMatchMode() == AUTO_CORR),
                _modeRadioList, AUTO_CORR, matchManager);
    modeCmd[i++] = new TpSetMatchModeCmd("AffineOnly", True,
                (CmdValue)(matchManager->getMatchMode() == AFFINE_ONLY),
                _modeRadioList, AFFINE_ONLY, matchManager);
 
    _quitCmd = new TpQuitCmd("Exit", True, matchManager);
    trapDeleteWindowEvent(_quitCmd);

    _saveAndExitCmd = new TpSaveAndExitCmd("Save and Exit",False,matchManager);

    if (_enableSetSpecialStatus)
	_exitStatusCmd = new TpExitStatusCmd("Status", True);
    else
	_exitStatusCmd = NULL;

    _shiftLeftCmd = new TpShiftCmd("Shift Left", True, tpDisplayer, True);
    _shiftRightCmd = new TpShiftCmd("Shift Right", True, tpDisplayer, False);

    Cmd *loadImageCmd = new TpLoadImageCmd("Load Image", True, tpDisplayer);
    _postLoadImageCmd = new PostSingleFileDialogCmd("Open Image File", True, 
						     loadImageCmd);

    TpRemoveImageCmd *rmImageCmd = new TpRemoveImageCmd("RmImage", True, 
							tpDisplayer);
    CustomDialog *removeImageDialog = new SgKeyinDialog("RmImageDialog",
                                        rmImageCmd);
    _postRemoveImageCmd = new PostDialogCmd("Remove Image", True,
                                            removeImageDialog);

    Cmd *loadPointFileCmd = new TpLoadPointFileCmd("LoadPointFile", 
					True, matchManager);
    _postLoadPointFileCmd = new PostSingleFileDialogCmd("Load Point File", 
							True, 
							loadPointFileCmd);

    _closePointFileCmd = new TpClosePointFileCmd("Close Point File",
                                                   True, matchManager);

    _savePointFileCmd = new TpSavePointFileCmd("Save Point File", False, 
					       matchManager);

    Cmd *savePointFileAsCmd = new TpSavePointFileAsCmd("SavePointFileAs", 
						       True, matchManager);
    _postSavePointFileAsCmd = new PostSingleFileDialogCmd("Save Point File As",
							  True, 
							  savePointFileAsCmd);

    _loadConfigCmd = new TpLoadConfigCmd("LoadConfig", True, this);
    _postLoadConfigCmd = new PostSingleFileDialogCmd("Load Config", 
						     True, 
						     _loadConfigCmd);
    _saveConfigCmd = new TpSaveConfigCmd("Save Config", True, this);
    _saveConfigAsCmd = new TpSaveConfigAsCmd("SaveConfigAs", True, this);
    _postSaveConfigAsCmd = new PostSingleFileDialogCmd("Save Config As", 
						       True, 
						       _saveConfigAsCmd);

    Cmd *printCmd = new TpPrintCmd("PrintCmd", True, tpDisplayer);
    SgPrintDialog *printDialog = new SgPrintDialog("Print", printCmd);
    _printCmd = new PostDialogCmd("Print", True, printDialog);

    CustomDialog *dispModeDialog = new TpDisplayModeDialog("DisplayModeDialog",
					tpDisplayer);
    _postDisplayModeCmd = new PostDialogCmd("Display Mode", True, 
					    dispModeDialog);

    _autofindResultsDialog=new TpAutofindResultsDialog("AutofindResultsDialog",
						       matchManager);
    _postAutofindResultsCmd = new PostDialogCmd("Autofind Results", True,
						_autofindResultsDialog);

    _matchModeResultsDialog = new TpMatchModeResultsDialog(
	"MatchModeResultsDialog", matchManager);
    _postMatchModeResultsCmd = new PostDialogCmd("Match Mode Results", True,
						 _matchModeResultsDialog);

    CustomDialog *qualFormatDialog;
    qualFormatDialog = new TpQualFormatDialog("QualFormatDialog", 
					      matchManager);
    _postQualFormatDialogCmd = new PostDialogCmd("Qualifier Format", True, 
						 qualFormatDialog);

    // Create Point Editor Options dialog

    CustomDialog *pointEditorOptsDialog = new TpPointEditorOptsDialog(
        "PointEditorOptsDialog", matchManager);
    _postPointEditorOptsDialogCmd = new PostDialogCmd("Point Editor Options", 
						      True,
						      pointEditorOptsDialog);

    // Create Point Symbol dialog

    CustomDialog *pointSymbolsDialog = new TpPointSymbolsDialog(
	"PointSymbolsDialog", matchManager);
    _postPointSymbolsDialogCmd = new PostDialogCmd("Point Symbols", True,
						   pointSymbolsDialog);

    // Create Point Tags dialog

    CustomDialog *pointTagsDialog = new TpPointTagsDialog("PointTagsDialog", 
							  matchManager);
    _postPointTagsDialogCmd = new PostDialogCmd("Point Tags", True, 
						pointTagsDialog);

    // Create Cursor Symbol dialog

    CustomDialog *cursorSymbolsDialog = new TpCursorSymbolsDialog(
	"CursorSymbolDialog", tpDisplayer);
    _postCursorSymbolsDialogCmd = new PostDialogCmd("Cursor Symbols", True, 
						   cursorSymbolsDialog);

    // Create Autofind dialog and command

    CustomDialog *autofindDialog = new TpAutofindDialog("AutofindDialog", 
							matchManager, 
							_findRadioList);
    _postAutofindDialogCmd = new PostDialogCmd("Autofind Mode", True, 
					       autofindDialog);

    // Create Match Mode dialog and command

    _matchModeDialog = new TpMatchModeDialog("MatchModeDialog", 
						  	  matchManager, 
							  _modeRadioList);
    _postMatchModeDialogCmd = new PostDialogCmd("Match Mode", True, 
							_matchModeDialog);

    TpMatchBrowseControl *matchBrowseControl;
    matchBrowseControl = new TpMatchBrowseControl(form, "matchBrowseControl", 
					matchManager->getSelectionMgr(), 
					matchManager);
    matchManager->setMatchBrowser(matchBrowseControl);
    matchBrowseControl->manage();

    SgButtonPanel *buttons= new SgButtonPanel(form, "buttonPanel");
    buttons->addCommands(_savePointCmd);
    buttons->addCommands(_deletePointCmd);
    buttons->addCommands(_listPointsCmd);
    buttons->addCommands(_shiftLeftCmd);
    buttons->addCommands(_shiftRightCmd);
    buttons->addOptionMenu(_findRadioList);
    buttons->addOptionMenu(_modeRadioList);

    XtVaSetValues(matchBrowseControl->baseWidget(),
		  XmNtopAttachment,     XmATTACH_FORM,
		  XmNleftAttachment,    XmATTACH_FORM,
		  XmNrightAttachment,   XmATTACH_NONE, 
		  XmNbottomAttachment,  XmATTACH_NONE,
		  NULL);
    XtVaSetValues(buttons->baseWidget(),
		  XmNorientation,       XmHORIZONTAL,
		  XmNtopAttachment,	XmATTACH_FORM,
		  XmNleftAttachment,	XmATTACH_WIDGET,
		  XmNleftWidget,        matchBrowseControl->baseWidget(),
		  XmNrightAttachment,	XmATTACH_NONE,
		  XmNbottomAttachment,	XmATTACH_NONE,
		  NULL);
    XtVaSetValues(ref->baseWidget(),
		  XmNtopAttachment,	XmATTACH_FORM,
		  XmNleftAttachment,	XmATTACH_NONE,
		  XmNrightAttachment,	XmATTACH_FORM,
		  XmNbottomAttachment,	XmATTACH_NONE,
		  NULL);
    XtVaSetValues(tpDisplayer->baseWidget(),
		  XmNtopAttachment,	XmATTACH_WIDGET,
		  XmNtopWidget, 	buttons->baseWidget(),
		  XmNleftAttachment,	XmATTACH_FORM,
		  XmNrightAttachment,	XmATTACH_FORM,
		  XmNbottomAttachment,	XmATTACH_FORM,
		  NULL);

    buttons->manage();
    ref->manage();
    tpDisplayer->manage();

    matchManager->readInitialPoints();

    return(form);
}

///////////////////////////////////////////////////////////////
// Create the menus for the main window
///////////////////////////////////////////////////////////////

void TpWindow::createMenuPanes()
{
    MenuCmdList *cmdList;

    ////////
    // Create File menu
    ////////

    cmdList = new MenuCmdList("File");
    cmdList->addButton(_postLoadImageCmd);       // Open Image File
    cmdList->addButton(_postRemoveImageCmd);	 // Remove Image
    cmdList->addSeparator();
    cmdList->addButton(_postLoadPointFileCmd);   // Load Point File
    cmdList->addButton(_closePointFileCmd);      // Close Point File
    cmdList->addButton(_savePointFileCmd);       // Save Point File
    cmdList->addButton(_postSavePointFileAsCmd); // Save Point File As
    cmdList->addSeparator();
    cmdList->addButton(_postLoadConfigCmd);      // Open Config
    cmdList->addButton(_saveConfigCmd);          // Save Config
    cmdList->addButton(_postSaveConfigAsCmd);    // Save Config As
    cmdList->addSeparator();
    cmdList->addButton(_printCmd);               // Print
    cmdList->addSeparator();
    cmdList->addButton(_saveAndExitCmd);         // Save and Exit
    cmdList->addButton(_quitCmd);                // Exit
    if (_enableSetSpecialStatus) {
	cmdList->addSeparator();
	cmdList->addCheckBox(_exitStatusCmd);    // Set Special Exit Status
    }
    _menuBar->addCommands(cmdList);
    delete cmdList;

    ////////
    // Create Edit menu
    ////////

    cmdList = new MenuCmdList("Edit");
    cmdList->addButton(theUndoCmd);              // Undo
    cmdList->addSeparator();
    cmdList->addButton(_savePointCmd);           // Save Point
    cmdList->addButton(_deletePointCmd);         // Delete Point
    cmdList->addButton(_listPointsCmd);          // List Points
    cmdList->addSeparator();
    cmdList->addButton(_redoMatchIdsCmd);        // Redo General Qualifiers
    cmdList->addCheckBox(_autoSyncPointsCmd);    // Auto Rematch Points
    _menuBar->addCommands(cmdList);
    delete cmdList;

    ////////
    // Create Point menu
    ////////

    cmdList = new MenuCmdList("Point");
    cmdList->addButton(_postQualFormatDialogCmd);      // Qualifier Format
    cmdList->addButton(_postPointEditorOptsDialogCmd); // Point Editor Options
    cmdList->addButton(_postPointSymbolsDialogCmd);    // Point Symbols
    cmdList->addButton(_postPointTagsDialogCmd);       // Point Tags
    cmdList->addButton(_postCursorSymbolsDialogCmd);   // Cursor Symbols
    cmdList->addButton(_postAutofindDialogCmd);        // Autofind Mode
    cmdList->addButton(_postMatchModeDialogCmd);       // Match Mode
    _menuBar->addCommands(cmdList);
    delete cmdList;

    ////////
    // Create View menu
    ////////

    cmdList = new MenuCmdList("View");
    cmdList->addButton(_postDisplayModeCmd);      // Display Mode
    cmdList->addButton(_postAutofindResultsCmd);  // Autofind Results
    cmdList->addButton(_postMatchModeResultsCmd); // Match Mode Results
    _menuBar->addCommands(cmdList);
    delete cmdList;

    ////////
    // Create Help menu
    ////////

    Cmd *helpOnContextCmd = new HelpOnContextCmd("On Context", True, _w);
    Cmd *helpOnHelpCmd = new HelpSelfCmd("On Help", TRUE,
		_menuBar->baseWidget(), "*Help*On Help");
    Cmd *helpOnWindowCmd = new HelpSelfCmd("On Window", TRUE,
		_menuBar->baseWidget(), "*Help*On Window");
    Cmd *helpOnKeysCmd = new HelpSelfCmd("On Keys", TRUE,
		_menuBar->baseWidget(), "*Help*On Keys");
    Cmd *helpOnVersionCmd = new HelpSelfCmd("On Version", TRUE,
		_menuBar->baseWidget(), "*Help*On Version");

    Cmd *printWidgetTreeCmd = NULL;
    if (_enablePrintWidgetTree)
	printWidgetTreeCmd = new PrintWidgetTreeCmd("Print Widget Tree",
						True, _w);

    cmdList = new MenuCmdList("Help");

    cmdList->addButton(helpOnContextCmd);
    cmdList->addButton(helpOnHelpCmd);
    cmdList->addButton(helpOnWindowCmd);
    cmdList->addButton(helpOnKeysCmd);
    cmdList->addButton(helpOnVersionCmd);

    if (_enablePrintWidgetTree) {
	cmdList->addSeparator();
	cmdList->addButton(printWidgetTreeCmd);
    }

    _menuBar->addCommands(cmdList, True);
    delete cmdList;
}

void TpWindow::deleteWindowResponse()
{
    _quitCmd->execute();
}

////////////////////////////////////////////////////////////////////////
// If a given filename is current, enable/disable the Save command based on
// whether a pathname is present.
////////////////////////////////////////////////////////////////////////

void TpWindow::resetSaveCmd(Boolean enable)
{
    if (_savePointFileCmd) {
	if (enable) {
	    _savePointFileCmd->activate();
	    _saveAndExitCmd->activate();
	}
	else {
	    _savePointFileCmd->deactivate();
	    _saveAndExitCmd->deactivate();
	}
    }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpDisplayer.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpDisplayer.cc: This class manages multiple subdisplayer windows, of which
// at most three can be displayed on the screen at one time.
//////////////////////////////////////////////////////////////////////////////
#include "TpDisplayer.h"
#include "TpImageReference.h"
#include "TpSubDisplayer.h"
#include "TpMatchManager.h"
#include "TpMatch.h"
#include "TpPointModel.h"
#include "TpImageView.h"
#include "TpPosView.h"
#include "Application.h"
#include "ErrorManager.h"
#include "PrefManager.h"
#include "TpDisplayerPrefView.h"
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <assert.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>

XtResource TpDisplayer::_resources[] = {
    {
	(char *)"numImagesDisplayed",
	(char *)"NumImagesDisplayed",
	XmRInt,
	sizeof(int),
	XtOffsetOf(TpDisplayer, _numWin),
	XmRImmediate,
	(XtPointer) 3,
    },
    {
	(char *)XvicNcursor,
	(char *)XvicCCursor,
	XtRString,
	sizeof(String),
	XtOffsetOf(TpDisplayer, _cursor),
	XtRImmediate,
	(XtPointer) "crosshair",
    },
    {
        (char *)XvicNcursorBackground,
        (char *)XvicCCursorBackground,
        XtRString,
        sizeof(String),
        XtOffsetOf(TpDisplayer, _cursorColor),
        XtRImmediate,
        (XtPointer) "white",
    },
};

//!!!! Comment out next line before delivery!
// #define DEBUG
#ifdef DEBUG
#define DPR(x) printf x
#else
#define DPR(x)
#endif

// Reload resources

void TpDisplayer::reload(TpDisplayer *copy)
{
    if (_numWin != copy->_numWin)
	setNumWin(copy->_numWin);
    if (strcmp(_cursor, copy->_cursor)) {
	setCursor(copy->_cursor);
    }
    if (strcmp(_cursorColor, copy->_cursorColor)) {
        setCursorColor(copy->_cursorColor);
    }
}

TpDisplayer::TpDisplayer(Widget parent, const char *name, 
		TpMatchManager *mm, TpImageReference *ref)
	: UIComponent(name)
{
    _imageReference = ref;
    for (int i=0; i < TP_MAX_DISPLAYS; i++) {
       _win[i] = 0;
       _locks[i] = False;
    }

    _matchManager = mm;

    for (int j = 0; j < TP_MAX_IMAGES; j++)
	_image[j] = NULL;
    _nimages = 0;

    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
    installDestroyHandler();

    getResources(_resources, XtNumber(_resources));

    if (_numWin > TP_MAX_DISPLAYS) {
        fprintf(stderr, "numImagesDisplayed cannot be greater than %d!\n",
							TP_MAX_DISPLAYS);
        fprintf(stderr, "Correct the resource file!\n");
        _numWin = TP_MAX_DISPLAYS;
    }
    if (_numWin < 1) {
        fprintf(stderr, "numImagesDisplayed cannot be less than 1!\n");
        fprintf(stderr, "Correct the resource file!\n");
        _numWin = 1;
    }

    char path[256];
    sprintf(path, "*%s*", _name);
    thePrefManager->registerResources(this,
                                      _resources, XtNumber(_resources),
                                      path, new TpDisplayerPrefView());
    
    _form = XtVaCreateManagedWidget("dispForm", xmFormWidgetClass, _w, NULL);

    // Callee is responsible for deleting ImageData when it's done!

    for (int i = 1; i < theApplication->getArgc(); i++) {
	addImage(theApplication->getParam(i));
    }

    layoutComponents();
    showComponents();

    Widget shell = parent;
    do {
        shell = XtParent(shell);
    } while (shell && !XtIsShell(shell));
    XtVaSetValues(shell, XtNallowShellResize, False, NULL);
}

TpDisplayer::~TpDisplayer()
{
    // Empty
}

void TpDisplayer::layoutComponents() const
{
    for (int i = 0; i < _nimages; i++)
	XtVaSetValues(_image[i]->baseWidget(),
		XmNx,   0,
		XmNy,   0,
		XmNtopAttachment,	XmATTACH_NONE,
		XmNleftAttachment,	XmATTACH_NONE,
		XmNrightAttachment,	XmATTACH_NONE,
		XmNbottomAttachment,	XmATTACH_NONE,
		NULL);

    if (_nimages < 1) return;

    for (int w = 0; w < _numWin; w++) {
	if (w >= _nimages) return;		// no more images
	if (_image[_win[w]] == NULL) continue;	// nothing here

	if (w == 0) {			// first one
	    XtVaSetValues(_image[_win[w]]->baseWidget(),
		XmNtopAttachment,       XmATTACH_FORM,
		XmNleftAttachment,      XmATTACH_FORM,
		XmNbottomAttachment,    XmATTACH_FORM,
		NULL);
	} else {
	    XtVaSetValues(_image[_win[w]]->baseWidget(),
		XmNtopAttachment,       XmATTACH_FORM,
		XmNleftAttachment,	XmATTACH_POSITION,
		XmNleftPosition,	(w*100)/_numWin, // + 2,
		XmNrightAttachment,     XmATTACH_FORM,
		XmNbottomAttachment,    XmATTACH_FORM,
		NULL);
	}

	if (w == _numWin-1) {		// last one
	    XtVaSetValues(_image[_win[w]]->baseWidget(),
		XmNrightAttachment,	XmATTACH_FORM,
		NULL);
	} else {
	    XtVaSetValues(_image[_win[w]]->baseWidget(),
		XmNrightAttachment,     XmATTACH_POSITION,
		XmNrightPosition,       ((w+1)*100)/_numWin,
		NULL);
	}
    }
}

///////////////////////////////////////////////////////////////////
// showComponents: Manages only the components to be shown
///////////////////////////////////////////////////////////////////
void TpDisplayer::showComponents() const
{
    for (int w=0; w < _numWin; w++) {
	if (w >= _nimages) return;
        if (_numWin > w) {
	    assert ((_win[w] >= 0) && (_win[w] < _nimages) &&
		(_win[w] >= 0) && (_win[w] < _nimages));
	    _image[_win[w]]->manage();
        }
    }
}

///////////////////////////////////////////////////////////////////
// hideComponents: Unmanages the images so as to hide them.
///////////////////////////////////////////////////////////////////
void TpDisplayer::hideComponents() const
{
    for (int i = 0; i < _nimages; i++)
	_image[i]->unmanage();
}

int TpDisplayer::addImage(char *filename)
{
    if (_nimages >= TP_MAX_IMAGES) {
	printf("Can't load any more images!!\n");
	return -1;
    }
    char name[16];
    sprintf(name,"form%d", _nimages+1);
    TpSubDisplayer *sd = new TpSubDisplayer(_form, name, filename, 
					    _nimages + 1, _matchManager, 
					    this);
    if (sd->failed()) return -1;

    _image[_nimages] = sd;
    _imageReference->indicateLoadedImage(_nimages);
    if (_nimages < _numWin) {
	_win[_nimages] = _nimages;
	_imageReference->setToVisible(_nimages);
	_nimages++;
	layoutComponents();
	showComponents();
	return 0;
    }
    _nimages++;
    return 0;
}

int TpDisplayer::deleteImage(int n)
{
    if (n <= 0)
	return 0;
    if (n > _nimages)
	return 0;

    _matchManager->deleteAllPoints(_image[n-1]->getImageData());

    hideComponents();

    //!!!delete _image[n-1];
    for (int i = n; i < _nimages; i++) {
	_image[i-1] = _image[i];
	_image[i-1]->setNumber(i);
    }
    _nimages--;

    for (int j = 0; j < TP_MAX_DISPLAYS; j++) {
	_win[j] = (_nimages > j) ? j : 0;
    }

    layoutComponents();
    showComponents();

    return 0;
}

void TpDisplayer::shiftRight()
{
    if (_nimages == 1)
	return;

    hideComponents();
    _imageReference->setAllToInvisible();

    for (int w = _numWin-1; w >=0; w--) {
        if (_locks[w])
	    continue;			// don't shift this one
	// Find the next lower slot that isn't locked and transfer it over
	int prev;
	for (prev = w-1; prev >=0; prev--) {
	    if (!_locks[prev]) {
		break;		// found one
	    }
	}
	if (prev >= 0) {
	    _win[w] = _win[prev];	// shift it
	}
        else {
	    // We're the last in line, so gotta get a new image.  Decrement to
	    // a previous image, skipping anything already being shown (in any
	    // window, locked or not).  It's possible we don't find anything
	    // (say, nimages == numWin) in which case we do nothing.

	    int orig_win = _win[w];
	    Boolean good = False;
	    for (int i = 0; i < _nimages; i++) {	// don't try forever
	        _win[w]--;
	        if (_win[w] < 0) _win[w] = _nimages-1;
	        good = True;
	        for (int w2 = 0; w2 < _numWin; w2++) {
		    if (w2 == w) continue;
		    if (_win[w] == _win[w2]) {
		        good = False;
		        break;
		    }
	        }
		if (good)
		    break;
	    }
	    if (!good) {		// couldn't find anything!
		_win[w] = orig_win;
	    }
	}
    }
    DPR(("%d %d %d %d %d %d\n", _win[0], _win[1], _win[2], _win[3], _win[4], _win[5]));

    layoutComponents();
    showComponents();

    for (int i=0; i < _numWin; i++) {
        _imageReference->setToVisible(_win[i]);
    }
}


void TpDisplayer::shiftLeft()
{
    if (_nimages == 1)
	return;

    hideComponents();
    _imageReference->setAllToInvisible();

    for (int w = 0; w <_numWin; w++) {
        if (_locks[w])
	    continue;			// don't shift this one
	// Find the next higher slot that isn't locked and transfer it over
	int next;
	for (next = w+1; next < _numWin; next++) {
	    if (!_locks[next]) {
		break;		// found one
	    }
	}
	if (next < _numWin) {
	    _win[w] = _win[next];	// shift it
	}
        else {
	    // We're the last in line, so gotta get a new image.  Increment to
	    // a later image, skipping anything already being shown (in any
	    // window, locked or not).  It's possible we don't find anything
	    // (say, nimages == numWin) in which case we do nothing.

	    int orig_win = _win[w];
	    Boolean good = False;
	    for (int i = 0; i < _nimages; i++) {	// don't try forever
	        _win[w]++;
	        if (_win[w] >= _nimages) _win[w] = 0;
	        good = True;
	        for (int w2 = 0; w2 < _numWin; w2++) {
		    if (w2 == w) continue;
		    if (_win[w] == _win[w2]) {
		        good = False;
		        break;
		    }
	        }
		if (good)
		    break;
	    }
	    if (!good) {		// couldn't find anything!
		_win[w] = orig_win;
	    }
	}
    }
    DPR(("%d %d %d %d %d %d\n", _win[0], _win[1], _win[2], _win[3], _win[4], _win[5]));

    layoutComponents();
    showComponents();

    for (int i=0; i < _numWin; i++) {
        _imageReference->setToVisible(_win[i]);
    }
}

void TpDisplayer::setLock(int i)
{
    _locks[i] = True;
    _image[_win[i]]->setLock(True);
}

void TpDisplayer::unSetLock(int i)
{
    _locks[i] = False;
    _image[_win[i]]->setLock(False);
}

void TpDisplayer::setNumDisplays(int numWin)
{
    if (numWin <= 0 || numWin > TP_MAX_DISPLAYS) {
        printf("Error in setDisplayType\n");
        numWin = 1;
    }
    if (numWin > _nimages)
	numWin = _nimages;		// not more than num images
    setNumWin(numWin);
}

void TpDisplayer::setNumWin(int numWin)
{
    if (numWin == _numWin) return;
    int oldNumWin = _numWin;
    _numWin = numWin;
    if (_numWin > oldNumWin) {
	// fill in the new images.  We do this by temporarily locking all
	// the old ones, unlocking the new ones, and calling shiftLeft to
	// fill them in one at a time.
	Boolean temp_locks[TP_MAX_DISPLAYS];
	int i;
	for (i=0; i < TP_MAX_DISPLAYS; i++)
	    temp_locks[i] = _locks[i];
	for (i=0; i < oldNumWin; i++)
	    _locks[i] = True;
	for (i = oldNumWin; i < _numWin; i++) {
	    _locks[i] = False;		// may be none unlocked
	    _win[i] = 0;
	}
	for (i = oldNumWin; i < _numWin; i++) {
	    _numWin = i+1;
	    shiftLeft();
	    _locks[i] = True;
	    _numWin = numWin;
	}
	for (i=0; i < TP_MAX_DISPLAYS; i++)
	    _locks[i] = temp_locks[i];
    }

    hideComponents();
    layoutComponents();
    showComponents();
}

void TpDisplayer::setCursor(String newCursor)
{
    _cursor = sdup(newCursor);
    for (int i = 0; i < _numWin; i++) {
	_image[i]->setCursor(newCursor);
    }
}

void TpDisplayer::setCursorColor(String newCursorColor)
{
    _cursorColor = sdup(newCursorColor);
    for (int i = 0; i < _numWin; i++) {
        _image[i]->setCursorColor(newCursorColor);
    }
}

void TpDisplayer::newMatchSelected(TpMatch *match)
{
    if (match == NULL)
	return;
    SL_ListWatch<TpPointModel *> watch;
    match->initScan(&watch);
    TpPointModel *apoint;
    while ((apoint = match->next()) != NULL) {
	_image[apoint->getImageNumber()-1]->getZoomView()->setCenter(
	    apoint->getX(), apoint->getY());
	_image[apoint->getImageNumber()-1]->getImageView()->setCenter(
	    apoint->getX(), apoint->getY());
	_image[apoint->getImageNumber()-1]->getPosView()->displayValues();
    }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSubDisplayer.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// TpSubDisplayer.cc: There is one subdisplayer per image.  It contains 
// all the components associated with that image, such as image view, 
// pan view, zoom view, contast control panel, etc.
///////////////////////////////////////////////////////////////////
#include "TpSubDisplayer.h"
#include "TpMatchManager.h"
#include "TpDisplayer.h"
#include "TpImageView.h"
#include "TpStripPanTool.h"
#include "TpCursorModel.h"
#include "TpPosView.h"
#include "TpImageInfo.h"
#include "TpZoomControl.h"
#include "TpContrastCmd.h"
#include "TpContrastControl.h"
#include "VicarImageData.h"
#include "RotatedImageData.h"
#include "ErrorManager.h"
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>

XtResource TpSubDisplayer::_resources[] = {
 {
   (char *)"autoMin",
   (char *)"AutoMin",
   XmRBoolean,
   sizeof (Boolean),
   XtOffsetOf(TpSubDisplayer, _autoMinRange),
   XmRString,
   (XtPointer)"TRUE",
 },
 {
   (char *)"autoMax",
   (char *)"AutoMax",
   XmRBoolean,
   sizeof(Boolean),
   XtOffsetOf(TpSubDisplayer, _autoMaxRange),
   XmRString,
   (XtPointer)"TRUE",
 },
 {
   (char *)"min",
   (char *)"Min",
   XmRString,
   sizeof(String),
   XtOffsetOf(TpSubDisplayer, _minValue),
   XmRImmediate,
   (XtPointer)NULL,
 },
 {
   (char *)"max",
   (char *)"Max",
   XmRString,
   sizeof(String),
   XtOffsetOf(TpSubDisplayer, _maxValue),
   XmRImmediate,
   (XtPointer)NULL,
 },
};

TpSubDisplayer::TpSubDisplayer(Widget parent, const char *name, 
			       char *filename, int number, 
			       TpMatchManager *matchManager, 
			       TpDisplayer *displayer)
	: UIComponent(name)
{
    _displayer = displayer;
    _matchManager = matchManager;
 
    _filename = sdup(filename);
    _number = number;
    _locked = False;

    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
    installDestroyHandler();

    getResources(_resources, XtNumber(_resources));

    _form = XtVaCreateWidget("dispForm", xmFormWidgetClass, _w, NULL);

    _rotationMode = ROTATE_NO;
    _imageData = new RotatedImageData(_rotationMode);

    // Set up the initial data range before loading
    
    _imageData->setMinAuto(_autoMinRange);
    _imageData->setMaxAuto(_autoMinRange);
    if (_minValue) {
        double min = atof(_minValue);
        _imageData->setMinAuto(False);
        _imageData->setDataMin(min);
    }
    if (_maxValue) {
        double max = atof(_maxValue);
        _imageData->setMaxAuto(False);
        _imageData->setDataMax(max);
    }

    _failed = False;
    if (_imageData->open(_filename) != imSUCCESS) {
	theErrorManager->process(Error, _name, "Cannot open file", _filename);
	_failed = True;
    }

    _imageInfo = new TpImageInfo(_form, "imageInfo", _filename, _number);

    _imageView = new TpImageView(_form, "imageView", 
				 _imageData, 
				 matchManager, this);

    char buf[32];
    sprintf(buf, "mainImageZoomControl%d", _number);
    _mainImageZoom = new TpZoomControl(_form, buf, _imageView);

    _panZoomForm = XtVaCreateWidget("panZoomForm", 
				    xmFormWidgetClass, _form, NULL);

    _panView = new TpStripPanTool(_panZoomForm, "panView", _imageData, 
				_imageView->getWidget(), 150, 150, False);

    _zoomView = new TpImageView(_panZoomForm, "zoomView", 
				_imageData, matchManager, this);

    sprintf(buf, "zoomControl%d", _number);
    _zoomControl = new TpZoomControl(_panZoomForm, buf, _zoomView);

    Widget *aiw = new Widget [1]; 
    aiw[0] = _zoomView->getWidget();
    TpCursorModel *cursorModel = new TpCursorModel(TRUE, 
			_imageView->getWidget(), aiw, 1);
    _posView = new TpPosView(_form, "posView", cursorModel, 
			     _imageData, 
			     matchManager, this);

    sprintf(buf, "contrastControl%d", _number);
    Cmd *contrastCmd = new TpContrastCmd(buf, True, 
					 _imageView->getWidget(), 
					 _zoomView->getWidget(),
					 _panView->getWidget(),
					 _imageData);
    _contrastControl = new TpContrastControl(_form, contrastCmd);

    _lock = XtVaCreateWidget("lock", xmLabelWidgetClass, _form, NULL);

    setCursor(_displayer->getCursor());
    setCursorColor(_displayer->getCursorColor());

    layoutComponents();
    showComponents();
}

TpSubDisplayer::~TpSubDisplayer()
{
    delete [] _filename;
    delete _imageData; // image model will delete all views

    delete _imageInfo;
    delete _zoomControl;
    delete _posView;
    delete _contrastControl;
    XtDestroyWidget(_lock);
    XtDestroyWidget(_panZoomForm);
}

void TpSubDisplayer::layoutComponents() const
{
    XtVaSetValues(_panView->baseWidget(),
                XmNtopAttachment,       XmATTACH_FORM,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_POSITION,
		XmNrightPosition,	50,
                XmNbottomAttachment,    XmATTACH_FORM,
                NULL);
    XtVaSetValues(_zoomView->baseWidget(),
                XmNtopAttachment,       XmATTACH_FORM,
                XmNleftAttachment,      XmATTACH_POSITION,
                XmNleftPosition,        50,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_WIDGET,
                XmNbottomWidget,        _zoomControl->baseWidget(),
                NULL);
    XtVaSetValues(_zoomControl->baseWidget(),
                XmNtopAttachment,       XmATTACH_NONE,
                XmNleftAttachment,      XmATTACH_WIDGET,
                XmNleftWidget,          _panView->baseWidget(),
                XmNrightAttachment,     XmATTACH_NONE,
                XmNbottomAttachment,    XmATTACH_FORM,
                NULL);


    XtVaSetValues(_posView->baseWidget(),
		XmNtopAttachment,	XmATTACH_FORM,
		XmNleftAttachment,	XmATTACH_FORM,
		XmNrightAttachment,	XmATTACH_NONE,
		XmNbottomAttachment,	XmATTACH_NONE,
		NULL);
    XtVaSetValues(_lock,
		XmNtopAttachment,       XmATTACH_FORM,
		XmNleftAttachment,	XmATTACH_NONE,
		XmNrightAttachment,	XmATTACH_FORM,
		XmNbottomAttachment,	XmATTACH_NONE,
		NULL);
    XtVaSetValues(_imageInfo->baseWidget(),
                XmNtopAttachment,       XmATTACH_WIDGET,
		XmNtopWidget,		_posView->baseWidget(),
                XmNleftAttachment,      XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_NONE,
                NULL);
    XtVaSetValues(_imageView->baseWidget(),
                XmNtopAttachment,       XmATTACH_WIDGET,
                XmNtopWidget,           _imageInfo->baseWidget(),
                XmNleftAttachment,      XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_WIDGET,
		XmNbottomWidget,	_mainImageZoom->baseWidget(),
                NULL);
    XtVaSetValues(_mainImageZoom->baseWidget(),
		XmNtopAttachment,       XmATTACH_NONE,
		XmNleftAttachment,      XmATTACH_FORM,
		XmNrightAttachment,     XmATTACH_NONE,
		XmNbottomAttachment,    XmATTACH_WIDGET,
		XmNbottomWidget,        _panZoomForm,
		NULL);
    XtVaSetValues(_panZoomForm,
                XmNtopAttachment,       XmATTACH_NONE,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_WIDGET,
		XmNbottomWidget,	_contrastControl->baseWidget(),
                NULL);

    XtVaSetValues(_contrastControl->baseWidget(),
                XmNtopAttachment,       XmATTACH_NONE,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_FORM,
                NULL);
}

///////////////////////////////////////////////////////////////////
// showComponents: Manages only the components to be shown
///////////////////////////////////////////////////////////////////
void TpSubDisplayer::showComponents() const
{
    // Show image pan window
    _panView->manage();

    // Show zoomed image
    _zoomView->manage();

    // Show zoom controls
    _zoomControl->manage();

    XtManageChild(_panZoomForm);

    // Show position coordinates
    _posView->manage();

    // Show image info
    _imageInfo->manage();

    // Show image view
    _imageView->manage();

    // Show main image view's zoom control
    _mainImageZoom->manage();

    // Show contrast window
    _contrastControl->manage();

    XtManageChild(_panZoomForm);
    XtManageChild(_form);
}

///////////////////////////////////////////////////////////////////
// hideComponents: Unmanages the images so as to hide them.
///////////////////////////////////////////////////////////////////
void TpSubDisplayer::hideComponents() const
{
    _posView->unmanage();
    _imageInfo->unmanage();
    _imageView->unmanage();
    _mainImageZoom->unmanage();
    _zoomView->unmanage();
    _zoomControl->unmanage();
    _contrastControl->unmanage();
    XtUnmanageChild(_panZoomForm);
    XtUnmanageChild(_form);
}

void TpSubDisplayer::setRotationMode(RotationType rotMode)
{
    _rotationMode = rotMode;
    _imageData->setRotationMode(rotMode);
    if (_imageData->open(_filename) != imSUCCESS) {
        theErrorManager->process(Error, _name, "Cannot open file", _filename);
    }
    _matchManager->rotationChanged((ImageData *)_imageData); //!!! Use Unrot???
}

void TpSubDisplayer::setLock(Boolean setIt)
{
    (setIt) ? XtManageChild(_lock) : XtUnmanageChild(_lock);
}

void TpSubDisplayer::setCursor(String newCursor)
{
    Widget iw = _imageView->getWidget();
    XtVaSetValues(iw, XvicNcursor, newCursor, NULL);
    iw = _zoomView->getWidget();
    XtVaSetValues(iw, XvicNcursor, newCursor, NULL);
}

void TpSubDisplayer::setCursorColor(String newCursorColor)
{
    Widget iw = _imageView->getWidget();
    XtVaSetValues(iw, XvicNcursorBackground, newCursorColor, NULL);
    iw = _zoomView->getWidget();
    XtVaSetValues(iw, XvicNcursorBackground, newCursorColor, NULL);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageOverlayView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// ImageOverlayView.h: This class is responsible for displaying
// imaging widget with overlayed objects.
////////////////////////////////////////////////////////////////
#include "ImageOverlayView.h"
#include "DrawObject.h"
#include "ErrorManager.h"
#include "ImageData.h"

ImageOverlayView::ImageOverlayView(Widget parent, const char *name, 
				   ImageData *imageData)
	: ImageDisplayView(parent, name, imageData, 0, 0)
{
    _objects = new SL_List<DrawObject *>;
}

ImageOverlayView::~ImageOverlayView()
{
    SL_ListWatch<DrawObject *> watch;
    DrawObject *current;
    
    _objects->init_scan(&watch);
    while ((current = _objects->next()) != NULL) {
	_objects->remove_current();
	delete current;
    }	
    delete _objects;
}

XvicID ImageOverlayView::addObject(DrawObject *object)
{
    _objects->add(object);
    return (object->draw());
}

void ImageOverlayView::moveObject(DrawObject *, double, double)
{
    // Empty
}

void ImageOverlayView::deleteObject(DrawObject *o)
{
    SL_ListWatch<DrawObject *> watch;
    DrawObject *current;
    
    _objects->init_scan(&watch);
    while ((current = _objects->next()) != NULL) {
        if (current == o) {
	    _objects->remove_current();     
            delete current;
	} 
    }   
}

#if defined(vms) || defined(__VMS)
#pragma define_template SL_List<DrawObject *>
#pragma define_template SL_ListWatch<DrawObject *>
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpImageView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// TpImageView.cc: This class is responsible for displaying
// imaging widget with overlayed points.
////////////////////////////////////////////////////////////////
#include "TpImageView.h"
#include "TpPoint.h"
#include "TpMatchManager.h"
#include "TpSubDisplayer.h"
#include "ImageData.h"
#include "ErrorManager.h"
#include "Application.h"
#include "ImageData.h"

Boolean TpImageView::_actionsAdded = False;

TpImageView::TpImageView(Widget parent, const char *name, 
			 ImageData *imageData, 
			 TpMatchManager *matchManager, 
			 TpSubDisplayer *subDisplayer)
    : ImageOverlayView(parent, name, imageData)
{
    _matchManager = matchManager;
    _subDisplayer = subDisplayer;
    _saved = FALSE;

    XtAddCallback(_iw, XvicNinputCallback,
	&TpImageView::inputCallback, (XtPointer) this);

    _lastClickTime = 0;
}

TpImageView::~TpImageView()
{
    // Empty
}

void TpImageView::setTitle(char *)
{
    ImageOverlayView::setTitle(_matchManager->getIbisFileName());
}

void TpImageView::inputCallback(Widget, XtPointer clientData,
			XtPointer callData)
{
    TpImageView *obj = (TpImageView *)clientData;

    obj->input(callData);
}

//////////////////////////////////////////////////////////////////////
// input: Process user input.  
/////////////////////////////////////////////////////////////////////
void TpImageView::input(XtPointer callData)
{
    XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;

    if (cb->reason != XvicCR_INPUT)
	return;				// oops

    if (cb->input_num_params != 2)
	return;				// oops

    if (!strcmp(cb->input_params[0], "tp")) {
	double x, y;
	_model->transDisplayToImageCoords(cb->x_fp, cb->y_fp, &x, &y);

	if (!strcmp(cb->input_params[1], "new")) {
	    _matchManager->processNewPoint(x, y, _subDisplayer);
	}

        if (!strcmp(cb->input_params[1], "new_zoom")) {
            _matchManager->processNewPoint(x, y, _subDisplayer, False);
        }

        if (!strcmp(cb->input_params[1], "drag")) {
            _matchManager->processNewPoint(x, y, _subDisplayer);
        }

        if (!strcmp(cb->input_params[1], "drag_zoom")) {
            _matchManager->processNewPoint(x, y, _subDisplayer, False);
        }

	if (!strcmp(cb->input_params[1], "select")) {
	    XButtonPressedEvent *bevent = (XButtonPressedEvent *)cb->event;
	    if ((bevent->time - _lastClickTime) < 500)
		_matchManager->processSelectMatch(x, y, _subDisplayer);
	    else
		_matchManager->processSelectPoint(x, y, _subDisplayer);
	    _lastClickTime = bevent->time;
	}

	if (!strcmp(cb->input_params[1], "scrollAll")) {
	    _matchManager->processScrollAll(x, y, _subDisplayer);
	}

	if ((!strcmp(cb->input_params[1], "up")) ||
		(!strcmp(cb->input_params[1], "down")) ||
		(!strcmp(cb->input_params[1], "left")) ||
		(!strcmp(cb->input_params[1], "right"))) {
	    ZoomFactor zf = getImageZoom();
	    double ratioX = (double)zf.getXOut() / (double)zf.getXIn();
	    double ratioY = (double)zf.getYOut() / (double)zf.getYIn();

	    double moveX = 0.0;
	    double moveY = 0.0;
	    if (!strcmp(cb->input_params[1], "up"))
		moveY = ratioY * (-1);	
	    if (!strcmp(cb->input_params[1], "down"))
                moveY = ratioY;
	    if (!strcmp(cb->input_params[1], "left"))
                moveX = ratioX * (-1);
	    if (!strcmp(cb->input_params[1], "right"))
                moveX = ratioX;

	    _matchManager->processMovePoint(_subDisplayer, moveX, moveY);
	}
    }	
}

void TpImageView::setCenter(const double samp, const double line)
{
    double x, y;
    _model->transImageToDisplayCoords(samp, line, &x, &y);

    Dimension width, height;
    getViewSize(width, height);

    ZoomFactor zf = getImageZoom();
    int xin = zf.getXIn();
    int xout = zf.getXOut();
    int yin = zf.getYIn();
    int yout = zf.getYOut();

    XtVaSetValues(_iw, 
		XvicNxPan, (int)x-(int)(width/(2.0*((float)xin/(float)xout))),
		XvicNyPan, (int)y-(int)(height/(2.0*((float)yin/(float)yout))),
		NULL);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchManager.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpMatchManager: Keeps track of all the tiepoint matches.
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher      JPL
//////////////////////////////////////////////////////////////////////////////
#include "TpMatchManager.h"
#include "TpDisplayer.h"
#include "TpSubDisplayer.h"
#include "TpImageView.h"
#include "VicarImageData.h"
#include "TpMatch.h"
#include "TpPointModel.h"
#include "TpPoint.h"
#include "TpPosView.h"
#include "TpSelectionMgr.h"
#include "TpQualGroupMgr.h"
#include "TpMatchModeValues.h"
#include "TpMatchManagerPrefView.h"
#include "ErrorManager.h"
#include "TpApplication.h"
#include "TpWindow.h"
#include "TpDefs.h"
#include "PrefManager.h"
#include "file_no_path.h"
#include "ibistiepnt.h"
#include "ibisfile.h"
#include "zvproto.h"
#include "affpar.h"
#include "kqkkor.h"
#include <Xm/RepType.h>
#include <assert.h>
#include <math.h>
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>

//!!!! Comment out next line before delivery!
// #define DEBUG
#ifdef DEBUG
#define DPR(x) printf x
#else
#define DPR(x)
#endif

#define TPDPR(x) if (theTpApplication->getVerbose()) printf x

XtResource TpMatchManager::_resources[] = {
  {
    (char *)"autofindMode",
    (char *)"AutofindMode",
    (char *)XtpRAutofindMode,
    sizeof(TpAutofindMode),
    XtOffset(TpMatchManager *, _autofindMode),
    XmRImmediate,
    (XtPointer)AFFINE,
  },
  {
    (char *)"matchMode",
    (char *)"matchMode",
    (char *)XtpRMatchMode,
    sizeof(TpMatchMode),
    XtOffset(TpMatchManager *, _matchMode),
    XmRImmediate,
    (XtPointer)AUTO_CORR,
  },
  {
    (char *)"pointSymbolShape",
    (char *)"PointSymbolShape",
    (char *)XtpRPointSymbolShapeType,
    sizeof(TpPointSymbolShapeType),
    XtOffset(TpMatchManager *, _pointShape),
    XmRImmediate,
    (XtPointer)CrossWithDot,
  },
  {
    (char *)"pointSymbolWidth",
    (char *)"PointSymbolWidth",
    XmRInt,
    sizeof(int),
    XtOffset(TpMatchManager *, _pointWidth),
    XmRImmediate,
    (XtPointer)15,
  },
  {
    (char *)"pointSymbolHeight",
    (char *)"PointSymbolHeight",
    XmRInt,
    sizeof(int),
    XtOffset(TpMatchManager *, _pointHeight),
    XmRImmediate,
    (XtPointer)15,
  },
  {
    (char *)"pointSymbolColor",
    (char *)"PointSymbolColor",
    XmRString,
    sizeof(String),
    XtOffset(TpMatchManager *, _pointColor),
    XmRImmediate,
    (XtPointer)"red",
  },
  {
    (char *)"pointSymbolSelColor",
    (char *)"PointSymbolSelColor",
    XmRString,
    sizeof(String),
    XtOffset(TpMatchManager *, _pointColorSel),
    XmRImmediate,
    (XtPointer)"green",
  },
  {
    (char *)"pmkCorrParm",
    (char *)"PmkCorrParm",
    XmRInt,
    sizeof(int),
    XtOffsetOf(TpMatchManager, _pmkCorrParm),
    XmRString,
    (XtPointer) "7",
  },
  {
    (char *)"lsmCorrParm", 
    (char *)"LsmCorrParm",
    XmRInt,
    sizeof(int),
    XtOffsetOf(TpMatchManager, _lsmCorrParm),
    XmRString,
    (XtPointer) "15",
  },
  {
    (char *)"winCorrParm", 
    (char *)"WinCorrParm",
    XmRInt,
    sizeof(int),
    XtOffsetOf(TpMatchManager, _winCorrParm),
    XmRString,
    (XtPointer) "20",
  },
  {
    (char *)"accCorrParm", 
    (char *)"AccCorrParm",
    XmRFloat,
    sizeof(float),
    XtOffsetOf(TpMatchManager, _accCorrParm),
    XmRString,
    (XtPointer) "0.3",
  },
  {
    (char *)"corCorrParm", 
    (char *)"CorCorrParm",
    XmRFloat,
    sizeof(float),
    XtOffsetOf(TpMatchManager, _corCorrParm),
    XmRString,
    (XtPointer) "0.5",
  },
  {
    (char *)"fullMatchIsEnforced",
    (char *)"FullMatchIsEnforced",
    XmRBoolean,
    sizeof(Boolean),
    XtOffsetOf(TpMatchManager, _fullMatchIsEnforced),
    XmRImmediate,
    (XtPointer) False,
  },
  {
    (char *)"clickPrecision",
    (char *)"ClickPrecision",
    XmRInt,
    sizeof(int),
    XtOffsetOf(TpMatchManager, _clickPrecision),
    XmRImmediate,
    (XtPointer) 4,
  },
};

////////////////////////////////////////////////////////////////////////

static Boolean CvtFloatToString(Display *dpy,
        XrmValue *, Cardinal *num_args,
        XrmValue *fromVal, XrmValue *toVal, XtPointer *)
{
   if (*num_args != 0)
      XtAppWarningMsg(
            XtDisplayToApplicationContext(dpy),
            "wrongParameters", "cvtFloatToString", "XtToolkitError",
            "Float to String conversion needs no extra arguments",
            (String *)NULL, (Cardinal *)NULL);

   static char out_val[20];

   sprintf(out_val, "%.2f", *((float *)(fromVal->addr)));

   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(char *)) {
         toVal->size = sizeof(char *);
         return FALSE;
      }
      *((char **)(toVal->addr)) = out_val;
   }
   else
      toVal->addr = (XPointer)&out_val;
   toVal->size = sizeof(char *);
   return TRUE;
}

// Reload resources

void TpMatchManager::reload(TpMatchManager *copy)
{
    if (_autofindMode != copy->_autofindMode)
	setAutofindMode(copy->_autofindMode);
    if (_matchMode != copy->_matchMode)
        setMatchMode(copy->_matchMode);
    if (_pointShape != copy->_pointShape)
	setPointShape(copy->_pointShape);
    if (_pointWidth != copy->_pointWidth)
	setPointWidth(copy->_pointWidth);
    if (_pointHeight != copy->_pointHeight)
        setPointHeight(copy->_pointHeight);
    if (strcmp(_pointColor, copy->_pointColor)) {
	TpPointModel::setPointColorStatic(copy->_pointColor);
	setPointColor(copy->_pointColor);
    }
    if (strcmp(_pointColorSel, copy->_pointColorSel)) {
	TpPointModel::setPointColorSelStatic(copy->_pointColorSel);
        setPointColorSel(copy->_pointColorSel);
    }
    if (_pmkCorrParm != copy->_pmkCorrParm)
	setPmkCorrParm(copy->_pmkCorrParm);
    if (_lsmCorrParm != copy->_lsmCorrParm)
        setLsmCorrParm(copy->_lsmCorrParm);
    if (_winCorrParm != copy->_winCorrParm)
        setWinCorrParm(copy->_winCorrParm);
    if (_accCorrParm != copy->_accCorrParm)
        setAccCorrParm(copy->_accCorrParm);
    if (_corCorrParm != copy->_corCorrParm)
        setCorCorrParm(copy->_corCorrParm);
}

Boolean TpMatchManager::_firstTime = False;

TpMatchManager::TpMatchManager(TpWindow *window)
{
    // One-time class initialization
 
    if (!_firstTime) {
	XmRepTypeId id;

        static String AutofindModeNames[] =
			{(char *)"manual", (char *)"affine", (char *)"spice"};
        static unsigned char AutofindModeValues[] = {MANUAL, AFFINE, SPICE};
        id = XmRepTypeRegister((char *)XtpRAutofindMode,
                               AutofindModeNames, AutofindModeValues,
                               XtNumber(AutofindModeNames));
        XmRepTypeAddReverse(id);

        static String MatchModeNames[] =
			{(char *)"auto_corr", (char *)"affine_only"};
        static unsigned char MatchModeValues[] = {AUTO_CORR, AFFINE_ONLY};
        id = XmRepTypeRegister((char *)XtpRMatchMode,
                               MatchModeNames, MatchModeValues,
                               XtNumber(MatchModeNames));
        XmRepTypeAddReverse(id);
 
	static String PointSymbolShapeNames[] = { 
	    (char *)"crosswithdot",
	    (char *)"rectangle",
	    (char *)"dot",
	    (char *)"cross45",
	    (char *)"crosswithhole45",
	    (char *)"rectanglewithcrosseswithdot" };
	static unsigned char PointSymbolShapeValues[] = { 
	    CrossWithDot,
	    Rectangle,
	    Dot, 
	    Cross45,
	    CrossWithHole45, 
	    RectangleWithCrossesWithDot };
        id = XmRepTypeRegister((char *)XtpRPointSymbolShapeType,
			       PointSymbolShapeNames,
                               PointSymbolShapeValues,
                               XtNumber(PointSymbolShapeNames));
	XmRepTypeAddReverse(id);

	XtSetTypeConverter(XmRFloat, XtRString, CvtFloatToString, 
			NULL, 0, XtCacheNone, (XtDestructor)NULL);

        _firstTime = True;
    }

    _window = window;

    XtGetApplicationResources(_window->baseWidget(), 
			      (XtPointer)this,
			      _resources, XtNumber(_resources),
			      NULL, 0);

    char path[100];
    sprintf(path, "*");
    thePrefManager->registerResources(theApplication->baseWidget(), 
				      (XtPointer)this, 
				      "tpMatchManager", "TpMatchManager", 
                                      _resources, XtNumber(_resources) - 2,
                                      path, new TpMatchManagerPrefView());

    _startId = 1;
    _nextId = _startId - 1;
    _autoSyncPoints = False;
    _checkingGenQualUnique = False;
    _matches = new SL_List<TpMatch *>;
    
    _selectionMgr = new TpSelectionMgr(this);
    _genQualMgr = new TpQualGroupMgr();
    _pointQualMgr = new TpQualGroupMgr();

    _ibisFilename = NULL;
    _dirtyFlag = False;

    _matchModeValues = new TpMatchModeValues(_pmkCorrParm, _lsmCorrParm, 
				_winCorrParm, _accCorrParm, _corCorrParm);

    TpPointModel::setPointShapeStatic(_pointShape);
    TpPointModel::setPointDimensionsStatic(_pointWidth, _pointHeight);
    TpPointModel::setPointColorStatic(_pointColor);
    TpPointModel::setPointColorSelStatic(_pointColorSel);
    //!!!TpPointModel::setTagPositionStatic(_tagPosition); //!!! Not resource

    if (theTpApplication->getConfig())
	_window->loadConfig(theTpApplication->getConfig());

    _referencedSubDisp = NULL;

    for (int cnt = 0; cnt < 5; cnt++)
	_matchModeResults[cnt] = 0.0;

    _autofindResults[0] = _autofindResults[4] = 1.0;
    _autofindResults[1] = _autofindResults[2] = 0.0;
    _autofindResults[3] = _autofindResults[5] = 0.0;
}

void TpMatchManager::closePointsIbis()
{
    if (_ibisFilename) delete [] _ibisFilename;
    _ibisFilename = NULL;

    TpMatch *selectedMatch= _selectionMgr->getMatch();
    _selectionMgr->clear();

    // Delete match only if it is so new, it has never been saved

    if (!isExistingMatch(selectedMatch))
	delete selectedMatch;

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        _matches->remove_current();
        delete amatch;
    }
    _window->deactivateSavePointCmd();

    char buf[1][6];
    buf[0][0] = '\0';
    _genQualMgr->setFormat(0, buf);
    _pointQualMgr->setFormat(0, buf);

    // Reset dirty flag

    _dirtyFlag = False;
}

void TpMatchManager::readInitialPoints()
{
    char *pfile = theTpApplication->getPfile();
    if (!pfile || (!strlen(pfile)))
	return;

    readPointsIbis(pfile);
}

///////////////////////////////////////////////////////////////////////
// Load points from an IBIS tiepoint file.
///////////////////////////////////////////////////////////////////////

void TpMatchManager::readPointsIbis(char *filename)
{
    closePointsIbis();

    if (filename && strlen(filename)) {
        _ibisFilename = sdup(filename);
	_window->resetSaveCmd(True);
    }
    else if (!_ibisFilename)
        return;

    int unit, ibis;
 
    int status;
    int i;
 
    // Open tiepoint file for read-only

    std::cout << "Reading tiepoint file " << _ibisFilename << std::endl;

    status = zvunit(&unit, (char *)"tp_file_1",  1, "u_name", _ibisFilename,
									NULL);
    if (status != OK) {
        theErrorManager->process(Error, "zvunit", "Cant process file");
        return;
    }

    int numImages;
    char filenames[MAX_noimgs][FNAMLEN];
    int numGenQual;
    char genQualNames[MAX_nogenqlf][STRING_32];
    char genQualFormat[MAX_nogenqlf][IFMT_SIZE];
    char genQualUnits[MAX_nogenqlf][STRING_32];
    int numPointQual;
    char pointQualNames[MAX_noimgqlf][STRING_32];
    char pointQualFormat[MAX_noimgqlf][IFMT_SIZE];
    char pointQualUnits[MAX_noimgqlf][STRING_32];
    int numMatches;
    
    status = zitiepnt_openr(unit, &ibis, &numImages, filenames,
               &numGenQual, genQualNames, genQualFormat, genQualUnits,
               &numPointQual, pointQualNames, pointQualFormat, pointQualUnits,
               &numMatches);
    if (status != OK) {
        theErrorManager->process(Error, "zitiepoint_openw",
                                 "Error opening a file for write");
	zitiepnt_close(unit);
        return;
    }

    // Now that we know the file is in the right format, clear the selection 
    // and delete all of the existing matches.  Then deactivate 'save point'
    // command in case it was active.

    //_closePointsIbis();
    _selectionMgr->clear();
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	_matches->remove_current();
	delete amatch;
    }
    _window->deactivateSavePointCmd(); // end clear

    // Set qualifiers format

    _genQualMgr->setFormat(numGenQual, genQualFormat);
    _pointQualMgr->setFormat(numPointQual, pointQualFormat);

    // Set general qualifiers name and unit

    for (i = 0; i < numGenQual; i++) {
	_genQualMgr->setQualName(i, genQualNames[i]);
	_genQualMgr->setQualUnit(i, genQualUnits[i]);
    }

    // Set point qualifiers name and unit

    for (i = 0; i < numPointQual; i++) {
        _pointQualMgr->setQualName(i, pointQualNames[i]);
	_pointQualMgr->setQualUnit(i, pointQualUnits[i]);
    }

    // Allocate arrays that are necessary for reading one row of data
    // Notice that we allocate space only once and then reuse storage.

    float *lines = new float[numImages];
    float *samps = new float[numImages];

    int *genQualsFull = new int[numGenQual];
    float *genQualsReal = new float[numGenQual];
    char *genQualsText = new char [numGenQual * 257];

    int *pointQualsFull = new int[numPointQual * numImages];
    float *pointQualsReal = new float[numPointQual * numImages];
    char *pointQualsText = new char [numPointQual * 257 * numImages];

    for (i = 1; i <= numMatches; i++) {

    	// Now declare pointers to each of the arrays so that we can do 
    	// pointer arithmetic without affecting storage for the next row of data

    	int *genQualsFullP = genQualsFull;
    	float *genQualsRealP = genQualsReal;
    	int *pointQualsFullP = pointQualsFull; 
    	float *pointQualsRealP = pointQualsReal;

	// Read one line of data

	status = zitiepnt_read(unit, i, 
                               lines, samps,
                               genQualsReal, genQualsFull, genQualsText,
                               pointQualsReal, pointQualsFull, pointQualsText);
	if (status != OK) {
	    theErrorManager->process(Error, "zitiepoint_openw",
				     "Error opening a file for write");
	    zitiepnt_close(unit);
	    return;
	}

	TpMatch *tmpMatch = new TpMatch(this);

	// Set match's general qualifiers according to the values 
	// obtained from the file

	TpQualGroup *genQual = tmpMatch->getGenQual();
	for (int jg = 0, jqg = 0; jg < numGenQual; jg++) {
	    if (genQual->getType(jg) == TpFull) {
		genQual->setValue(jg, genQualsFullP[0]);
		genQualsFullP++;
	    }
	    else if (genQual->getType(jg) == TpReal) {
		genQual->setValue(jg, genQualsRealP[0]);
                genQualsRealP++;
            }
	    else if (genQual->getType(jg) == TpText) {
		genQual->setValue(jg, genQualsText+(jqg*257));
		jqg++;
            }
	    else
		fprintf(stderr, 
			"Error in readPointsIbis (unknown qual type)\n");
	}

	for (int n = 0; n < numImages; n++) {
	    
	    TpSubDisplayer *aSubDisplayer;
	    int textQualPtr = 0;
	    Boolean fileIsLoaded = False;

	    for (int im = 0; im < _displayer->getNumImages(); im++) {
		aSubDisplayer = _displayer->getImage(im);
		char *shortFilename = sdup(aSubDisplayer->getFilename());
		file_no_path(shortFilename);
		
		if (!strcmp(shortFilename, filenames[n])) {
		    fileIsLoaded = True;

		    if (samps[n] == MISSING_POINT_VALUE &&
			lines[n] == MISSING_POINT_VALUE)
			break;
		    TpPointModel *pointModel;
		    tmpMatch->addPoint(aSubDisplayer->getFilename(),
				       aSubDisplayer->getNumber(),
				       aSubDisplayer->getImageData(),
				       samps[n], lines[n], _pointQualMgr, 
				       pointModel);
		    TpQualGroup *pointQual = pointModel->getPointQual();
		    for (int jp = 0; jp < numPointQual; jp++) {
			if (pointQual->getType(jp) == TpFull) {
			    pointQual->setValue(jp, pointQualsFullP[0]);
			    pointQualsFullP++;
			}
			else if (pointQual->getType(jp) == TpReal) {
			    pointQual->setValue(jp, pointQualsRealP[0]);
			    pointQualsRealP++;
			}
			else if (pointQual->getType(jp) == TpText) {
			    pointQual->setValue(jp, pointQualsText + 
						(textQualPtr * 257));
			    textQualPtr++;
			}
			else
			    printf("Error\n");
		    }

		    createPointViews(pointModel, aSubDisplayer);
		    delete [] shortFilename;
		}
	    }

	    // Skip qualifiers if file exists in the ibis file, but
	    // is not loaded into tp
 
	    if (!fileIsLoaded) {
		for (int m = 0; m < numPointQual; m++) {
		    if (!strcmp(pointQualFormat[m], FULL))
			pointQualsFullP++;
		    else if (!strcmp(pointQualFormat[m], REAL))
			pointQualsRealP++;
		    else
			textQualPtr++;
		}
		if (i == 1) {	// only want to print message once
		    char warnMsg[1024];
		    sprintf(warnMsg, 
		        "Image file %s exists in tiepoint file, but is not loaded",
		        filenames[n]);
		    theErrorManager->process(Error, "Open File", warnMsg);
		}
	    }
	}
	addMatch(tmpMatch);
    }

    status = zitiepnt_close(unit);
    if (status != OK) {
        theErrorManager->process(Error, "zitiepoint_close",
                                 "Error closing a file");
        return;
    }

    // Reset the dirty flag

    _dirtyFlag = False;

    // Now select the first point

    TpMatch *firstMatch = _matches->find_first();
    if (firstMatch) {
	_selectionMgr->selectMatchAndPoints(firstMatch);
	newMatchSelected();
    }

    // Set window title to name of file just loaded

    if (_displayer != NULL && _displayer->getNumImages() > 0) {
        TpSubDisplayer *img = _displayer->getImage(0);
        if (img != NULL) {
            img->getImageView()->setTitle("");  // string is unused
        }
    }

}

///////////////////////////////////////////////////////////////////////
// Save points into an IBIS tiepoint file
///////////////////////////////////////////////////////////////////////
void TpMatchManager::writePointsIbis(char *filename)
{
    if (filename && strlen(filename)) {
	_ibisFilename = sdup(filename);
	_window->resetSaveCmd(True);
    }
    else if (!_ibisFilename)
	return;

    int unit, ibis;

    int status;
    int i;

    // Get number of images

    const int numImages = _displayer->getNumImages();

    // Get image filenames

    char filenames[MAX_noimgs][FNAMLEN];
    TpSubDisplayer *aSubDisplayer;
    for (i = 0; i < _displayer->getNumImages(); i++) {
	aSubDisplayer = _displayer->getImage(i);
	sprintf(filenames[i], "%s", aSubDisplayer->getFilename());
    }

    // Get number of General Qualifiers

    const int numGenQual = _genQualMgr->getNumQuals();

    // Get names of General Qualifiers

    char genQualNames[MAX_nogenqlf][STRING_32];
    for (i = 0; i < numGenQual; i++)
	sprintf(genQualNames[i], _genQualMgr->getQualName(i));

    // Get units of General Qualifiers(e.g. pixels, m, cm, ...)
 
    char genQualUnits[MAX_nogenqlf][STRING_32];
    for (i = 0; i < numGenQual; i++)
        sprintf(genQualUnits[i], _genQualMgr->getQualUnit(i));

    // Get format of General Qualifiers

    char genQualFormat[MAX_nogenqlf][IFMT_SIZE];
    for (i = 0; i < numGenQual; i++) {
	if (_genQualMgr->getType(i) == TpFull) 
	    strcpy(genQualFormat[i], FULL);
	else if (_genQualMgr->getType(i) == TpReal)
            strcpy(genQualFormat[i], REAL);
	else
            strcpy(genQualFormat[i], TEXT);
    }

    // Get number of Image Qualifiers

    const int numPointQual = _pointQualMgr->getNumQuals();

    // Get names of Image Qualifiers

    char pointQualNames[MAX_noimgqlf][STRING_32];
    for (i = 0; i < numPointQual; i++)
	sprintf(pointQualNames[i], _pointQualMgr->getQualName(i));

    // Get unit values of Image Qualifiers

    char pointQualUnits[MAX_noimgqlf][STRING_32];
    for (i = 0; i < numPointQual; i++)
        sprintf(pointQualUnits[i], _pointQualMgr->getQualUnit(i));
    
    // Get format of Image Qualifiers
    
    char pointQualFormat[MAX_noimgqlf][IFMT_SIZE];
    for (i = 0; i < numPointQual; i++) { //!!! Implement GUI for this
        if (_pointQualMgr->getType(i) == TpFull)
            strcpy(pointQualFormat[i], FULL);
        else if (_pointQualMgr->getType(i) == TpReal)
            strcpy(pointQualFormat[i], REAL);
        else
            strcpy(pointQualFormat[i], TEXT);
    }

    // Get number of rows (number of tiepoints)

    const int numMatches = _matches->get_length();

    // Get vicar file unit

    status = zvunit(&unit, (char *)"tp_file_2", 1, "u_name",_ibisFilename,NULL);
    if (status != OK) {
        theErrorManager->process(Error, "zvunit", "Cant process file");
	return;
    }

    status = zitiepnt_openw(unit, &ibis, numImages, filenames,
	       numGenQual, genQualNames, genQualFormat, genQualUnits,
	       numPointQual, pointQualNames, pointQualFormat, pointQualUnits,
	       numMatches);
    if (status != OK) {
        theErrorManager->process(Error, "zitiepoint_openw", 
				 "Error opening a file for write");
        return;
    }

    int numFullPointQuals = _pointQualMgr->getNumFullQuals();
    int numRealPointQuals = _pointQualMgr->getNumRealQuals();
    int numTextPointQuals = _pointQualMgr->getNumTextQuals();

    // Get the data and write it row by row

    SL_ListWatch<TpMatch *>wm1;
    _matches->init_scan(&wm1);
    TpMatch *amatch;
    i = 1;
    while ((amatch = _matches->next()) != NULL) {
	float *lines = new float[numImages];
	float *samps = new float[numImages];

	int *fullPointQuals = new int [numImages * numFullPointQuals];
	float *realPointQuals = new float [numImages * numRealPointQuals];
	char *textPointQuals = new char [numImages * numTextPointQuals * 257];

	int n;                                   // Loop variable
	for (n = 0; n < numImages; n++) {
	    lines[n] = MISSING_POINT_VALUE;
	    samps[n] = MISSING_POINT_VALUE;
	}

	int fullPQCounter = 0;
	int realPQCounter = 0;
	int textPQCounter = 0;

	SL_ListWatch<TpPointModel *> wp1;
	amatch->initScan(&wp1);
	TpPointModel *apoint;
	while ((apoint = amatch->next()) != NULL) {
	    for (n = 0; n < numImages; n++) {
		if(!strcmp(apoint->getFilename(), filenames[n])) {
		    lines[n] = apoint->getY();
		    samps[n] = apoint->getX();
		    int k = 0;
		    TpQualGroup *pointQualGroup = apoint->getPointQual();
		    for (k = 0; k < numFullPointQuals; k++)
			fullPointQuals[fullPQCounter++] 
			    = pointQualGroup->getFullQuals()[k];
		    for (k = 0; k < numRealPointQuals; k++)
                        realPointQuals[realPQCounter++]
                            = pointQualGroup->getRealQuals()[k];
		    char *textQuals = pointQualGroup->getTextQuals();
		    if (textQuals) 
			strcat(textPointQuals + textPQCounter, textQuals);
		    textPQCounter += numTextPointQuals * 257;
		}
	    }
	}

	TpQualGroup *genQual = amatch->getGenQual();
	status = zitiepnt_write(unit, i++, lines, samps, 
				genQual->getRealQuals(),
				genQual->getFullQuals(), 
				genQual->getTextQuals(),
				realPointQuals, 
				fullPointQuals,
				textPointQuals);
	if (status != OK) {
	    theErrorManager->process(Error, "zitiepoint_write",
				     "Error writing a file");
	    return;
	}
    }

    status = zitiepnt_close(unit);
    if (status != OK) {
        theErrorManager->process(Error, "zitiepoint_close",
                                 "Error closing a file");
        return;
    }

    _dirtyFlag = False;

    // Set window title to name of file just saved

    if (_displayer != NULL && _displayer->getNumImages() > 0) {
        TpSubDisplayer *img = _displayer->getImage(0);
        if (img != NULL) {
            img->getImageView()->setTitle("");  // string is unused
        }
    }

}

TpMatchManager::~TpMatchManager()
{
    // Empty
}

//////////////////////////////////////////////////////////////////////////////
// Create new point at user's request.  genInMain means that the event was 
// generated in the main image and not the zoomed image.
// There are four possible actions that the 
// software will undertake.  First is pure manual, second is using affine 
// transformation only, third is use correlation only, fourth is use 
// both affine transformation and correlation followed by each other.
//////////////////////////////////////////////////////////////////////////////
void TpMatchManager::processNewPoint(const double x, const double y,
                                     TpSubDisplayer *subDisplayer,
                                     Boolean genInMain)
{
    _dirtyFlag = True;

    for (int i = 0; i < 5; i++) _matchModeResults[i] = 0.0;

    if ((_matchMode == AUTO_CORR) && (_autofindMode == AFFINE))
	automatch(x, y, subDisplayer, genInMain);
    if ((_matchMode == AFFINE_ONLY) && (_autofindMode == AFFINE))
        affine(x, y, subDisplayer, genInMain);
    if ((_matchMode == AUTO_CORR) && (_autofindMode == MANUAL))
        assist(x, y, subDisplayer, genInMain);
    if ((_matchMode == AFFINE_ONLY) && (_autofindMode == MANUAL))
        manual(x, y, subDisplayer, genInMain);
}

//////////////////////////////////////////////////////////////////////////////
// manual(): Place a point in given image at given location.
//////////////////////////////////////////////////////////////////////////////
int TpMatchManager::manual(const double x, const double y, 
			   TpSubDisplayer *subDisplayer,
			   Boolean genInMain, Boolean genByAlgorithm)
{
    if (!isInImage(x, y, subDisplayer))
	return -1;

    if (genInMain == True)
        subDisplayer->getZoomView()->setCenter(x, y);

    if (genByAlgorithm == True)
	subDisplayer->getImageView()->setCenter(x, y);

    //subDisplayer->getPosView()->cursorMoved(x, y);

    // If selection manager already reports point in this image, simply move it

    TpPointModel *apoint = selectedPoint(_selectionMgr, subDisplayer);
    if (apoint) {
	apoint->setXY(x, y);
	subDisplayer->getPosView()->displayValues();
	return 0;
    }
    
    // New point, so enable 'keep point' cmd
 
    _window->activateSavePointCmd();

    // Create a new match if necessary

    if (_selectionMgr->isEmpty())
        _selectionMgr->addMatch(new TpMatch(this));

    // If not every point in the match is already displayed, we may not 
    // need a new point.

    if (!_selectionMgr->isFullHouse()) {
	
	// Search selected match for this point on this display

	Boolean pointAlreadyExists = False;
	TpMatch *selectedMatch = _selectionMgr->getMatch();
	SL_ListWatch<TpPointModel *> wp2;
	selectedMatch->initScan(&wp2);
	while ((apoint = selectedMatch->next()) != NULL)
	    if (apoint->getImageData() == subDisplayer->getImageData()) {
		pointAlreadyExists = True;
		selectedMatch->scanDone();
		break;
	    }
	if (pointAlreadyExists) {
	    _selectionMgr->addPoint(apoint);
	    apoint->setXY(x, y);
	    subDisplayer->getPosView()->displayValues();
	    return 0;
	}
    }

    // Finally, we figured that we do need to create a new point

    TpPointModel *newPoint;
    newPoint = new TpPointModel(subDisplayer->getFilename(),
				subDisplayer->getNumber(),
				subDisplayer->getImageData(),
				_selectionMgr->getMatch(),
				_pointQualMgr, x, y);
    createPointViews(newPoint, subDisplayer);
    _selectionMgr->addPoint(newPoint);
    subDisplayer->getPosView()->displayValues();
    return 0;
}

/////////////////////////////////////////////////////////////////////////////
// Perform correlation
/////////////////////////////////////////////////////////////////////////////
void TpMatchManager::assist(const double x, const double y,
			    TpSubDisplayer *subDisplayer,
			    Boolean genInMain)
{
    // If there were no referenced image - here is the one

    if (_referencedSubDisp == NULL) {
	_referencedSubDisp = subDisplayer;
	manual(x, y, subDisplayer, genInMain);
	return;
    }

    // Return if there is a point already in the selection
 
    Boolean found = False;
    TpMatch *match = _selectionMgr->getMatch();
    SL_ListWatch<TpPointModel *> wp1;
    match->initScan(&wp1);
    TpPointModel *apoint;
    while ((apoint = match->next()) != NULL) {
        if (subDisplayer->getImageData() == apoint->getImageData())
            found = True;
    }
    if (found) {
	manual(x, y, subDisplayer, genInMain);
	return;
    }

    // Find out the referenced point and match against that point 

    double xCorr, yCorr;
    Boolean createNewPoint = False;
 
    SL_ListWatch<TpPointModel *> wp2;
    match->initScan(&wp2);
    if (_window->getMatchModeResultsDialog()->isDumpToStdout())
	printf("%s","\n");		// work around stupid g++ 3.3 bug
    while ((apoint = match->next()) != NULL) {
        if (_referencedSubDisp->getImageData() == apoint->getImageData()) {
            doMatch(apoint->getX(), apoint->getY(),
                    _referencedSubDisp, x, y, subDisplayer, xCorr, yCorr);
            createNewPoint = True;
        }
    }

    if (createNewPoint)
        manual(xCorr, yCorr, subDisplayer, True, True);
}

/////////////////////////////////////////////////////////////////////////////
// foreach subDisplayer except current do
//   foreach match except current do
//     extract point information
//   run affinpar
/////////////////////////////////////////////////////////////////////////////
void TpMatchManager::affine(const double x, const double y,
			    TpSubDisplayer *subDisplayer, Boolean genInMain)
{
    // Is this a brand new point?

    TpPointModel *pointExists = selectedPoint(_selectionMgr, subDisplayer);

    // First of all, place a point in a given image in specified location

    int status = manual(x, y, subDisplayer, genInMain);
    if (status == -1)
	return;

    if (pointExists && !_autoSyncPoints)
	return;

    // If this is the first point, we can't do affine transformation yet

    if (numMatches() == 0)
	return;

    // Now adjust all the other points in the match

    double xOut, yOut;

    TpSubDisplayer *aSubDisplayer;
    for (int i = 0; i < _displayer->getNumImages(); i++) {
        aSubDisplayer = _displayer->getImage(i);
        if (subDisplayer == aSubDisplayer)
            continue;
        status = doAffine(x, y, subDisplayer, aSubDisplayer, xOut, yOut);

	// Set the new point only if the new point is within the image

	if (status == 0)
	    manual(xOut, yOut, aSubDisplayer, True, True);
    }
}

/////////////////////////////////////////////////////////////////////////////
// First do affine transformation, then run correlator
/////////////////////////////////////////////////////////////////////////////
void TpMatchManager::automatch(const double x, const double y,
			       TpSubDisplayer *subDisplayer, 
			       Boolean genInMain)
{
    // Are there any corresponding points already?

    Boolean otherPoints = False;
    TpSubDisplayer *aSubDisplayer;
    int i;
    for (i = 0; i < _displayer->getNumImages(); i++) {
	aSubDisplayer = _displayer->getImage(i);
	if (selectedPoint(_selectionMgr, aSubDisplayer))
	    otherPoints = True;
    }

    // First of all, place a point in a given image in specified location
 
    int status = manual(x, y, subDisplayer, genInMain);
    if (status == -1)
        return;

    if (otherPoints && !_autoSyncPoints)
        return;
 
    // If this is the first point, we can't do affine transformation yet
 
    if (numMatches() == 0)
        return;
 
    // Now adjust all the other points in the match

    double xOut, yOut;

    if (_window->getMatchModeResultsDialog()->isDumpToStdout())
	printf("%s", "\n");		// work around stupid g++ 3.3 bug
    for (i = 0; i < _displayer->getNumImages(); i++) {
	aSubDisplayer = _displayer->getImage(i);
	if (subDisplayer == aSubDisplayer)
	    continue;
	int status = doAffine(x, y, subDisplayer, aSubDisplayer, xOut, yOut);
	if (status == -1)
	    continue;
	
	double korXOut, korYOut;
	status = doMatch(x, y, subDisplayer, xOut, yOut, 
			 aSubDisplayer, korXOut, korYOut);
	if (status == -1)
	    continue;
	
	xOut = korXOut;
	yOut = korYOut;
	
	manual(korXOut, korYOut, aSubDisplayer, True, True);
    }
}

/////////////////////////////////////////////////////////////////////////////
// Support code for running affine transformation.
/////////////////////////////////////////////////////////////////////////////
int TpMatchManager::doAffine(const double x, const double y, 
			     TpSubDisplayer *subDisplayerIn, 
			     TpSubDisplayer *subDisplayerOut, 
			     double &xOut, double &yOut)
{
    TPDPR(("--------------------------------------------------------\n"));
    TPDPR(("Running affpar()...\n"));
    TPDPR(("File ref: %s; File target: %s\n",
	 subDisplayerIn->getFilename(), subDisplayerOut->getFilename()));

    xOut = yOut = 0.0;  // Initialize

    int i = 0;
    int j = 0;
    int n = _matches->get_length();
    double *line0 = new double[n];
    double *samp0 = new double[n];
    double *line1 = new double[n];
    double *samp1 = new double[n];
    SL_ListWatch<TpMatch *> wma;
    _matches->init_scan(&wma);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	SL_ListWatch<TpPointModel *> wpa;
	amatch->initScan(&wpa);
	TpPointModel *apoint;
	Boolean sys0Exist = False;
	Boolean sys1Exist = False;
	while ((apoint = amatch->next()) != NULL) {
	    if (apoint->getImageData() == subDisplayerIn->getImageData()) {
		line0[i] = apoint->getY();
		samp0[i] = apoint->getX();
		sys0Exist = True;
	    }
	    if (apoint->getImageData() == subDisplayerOut->getImageData()) {
		line1[i] = apoint->getY();
		samp1[i] = apoint->getX();
		sys1Exist = True;
	    }
	}
	if (sys0Exist && sys1Exist)
	    i++;
    }

    for (j = 0; j < i; j++) {
	TPDPR(("lref[%d]=%.1f\tsref[%d]=%.1f\tltrg[%d]=%.1f\tstrg[%d]=%.1f\n",
	       j, line0[j], j, samp0[j], j, line1[j], j, samp1[j]));
    }

    affinpar(y, x, i, line0, samp0, line1, samp1, &yOut, &xOut, 
	     _autofindResults);
    TPDPR(("REF: line = %f\t samp = %f\n", y, x));
    TPDPR(("TRG: line = %f\t samp = %f\n", yOut, xOut));

    TPDPR(("Results: %f, %f, %f, %f, %f, %f\n", 
	 _autofindResults[0], _autofindResults[1], _autofindResults[2],
	 _autofindResults[3], _autofindResults[4], _autofindResults[5]));

    _window->getAutofindResultsDialog()->setValues(_autofindResults);

    TPDPR(("End running affpar().\n"));
    TPDPR(("--------------------------------------------------------\n"));

    // Check if the location is off image, in that case return status -1

    if (!isInImage(xOut, yOut, subDisplayerOut))
	return -1;

    return 0;
}

Boolean TpMatchManager::isInImage(const double x, const double y, 
				  TpSubDisplayer *sd)
{
    if ((y < 0) || (x < 0))
	return False;
    if (y > sd->getImageData()->getNumbLines())
	return False;
    if (x > sd->getImageData()->getNumbSamples())
	return False;

    return True;
}

//////////////////////////////////////////////////////////////////////////////
// Search selection manager for the point in given image
//////////////////////////////////////////////////////////////////////////////
TpPointModel *TpMatchManager::selectedPoint(TpSelectionMgr *sm,
                                           TpSubDisplayer *sd)
{
    Boolean pointAlreadyExists = False;
    SL_ListWatch<TpPointModel *> wp1;
    sm->initScan(&wp1);
    TpPointModel *apoint;
    while ((apoint = sm->nextPoint()) != NULL) {
        if (apoint->getImageData() == sd->getImageData()) {
            pointAlreadyExists = True;
            sm->scanDone();
            break;
        }
    }
    if (pointAlreadyExists)
	return apoint;
    else 
	return NULL;
}

double TpMatchManager::getAutofindResult(int i)
{
    assert((i >= 0) && (i < 6));
    return _autofindResults[i];
}

float TpMatchManager::getMatchModeResult(int i)
{
    assert((i >= 0) && (i < 5));
    return _matchModeResults[i];
}

/////////////////////////////////////////////////////////////////////////////
// Support code for running correlator
/////////////////////////////////////////////////////////////////////////////
int TpMatchManager::doMatch(const double x, const double y,
			    TpSubDisplayer *subDisplayerIn,
			    const double sx, const double sy,
			    TpSubDisplayer *subDisplayerOut,
			    double &xOut, double &yOut)
{
    TPDPR(("--------------------------------------------------------\n"));
    TPDPR(("Running correlator routines...\n"));
    TPDPR(("File ref: %s; File target: %s\n",
         subDisplayerIn->getFilename(), subDisplayerOut->getFilename()));

    // Initialize

    xOut = sx;
    yOut = sy;

    KORPAR info;
    info.suchfns = _matchModeValues->_searchWindow;
    info.pmkdim  = _matchModeValues->_pmk;
    info.kqkdim  = _matchModeValues->_lsm;
    info.lsmacu  = _matchModeValues->_accuracy;
    info.minpmk  = _matchModeValues->_threshold;
    info.affin[0] = _autofindResults[0];
    info.affin[1] = _autofindResults[1];
    info.affin[2] = _autofindResults[2];
    info.affin[3] = _autofindResults[3];
    info.affin[4] = _autofindResults[4];
    info.affin[5] = _autofindResults[5];

    INT_MATRIX musterma;
    INT_MATRIX suchma;
 
    dim_muster_such (&info, &musterma, &suchma);

    // Deal with the reference image first

    int *bufIn = new int[musterma.dimz * musterma.dims + 1];
    musterma.ptr_m = bufIn;

    // Read image

    ImageData *imDataIn = subDisplayerIn->getImageData();
    int pixelSize = imDataIn->getPixelSize();
    int numberSamples = imDataIn->getNumbSamples();
    int lineWidth = pixelSize * numberSamples;
    unsigned char *buffer = new unsigned char[lineWidth];

    int startLine = (int)(floor(y + .5) - (musterma.dimz / 2));
    int endLine = (int)(floor(y + .5) + (musterma.dimz / 2));
    int startSamp = (int)(floor(x + .5) - (musterma.dims / 2));
    int endSamp = (int)(floor(x + .5) + (musterma.dims / 2));

    int countBuf = 0;
    int i, j;
    for (i = startLine; i <= endLine; i++) {
	imDataIn->readLine(BWcolor, i, buffer);
	if (imDataIn->getPixelType() == imBYTE) {
	    imByte *bufB = (imByte *)buffer;
	    for (j = startSamp; j <= endSamp; j++)
		bufIn[countBuf++] = bufB[j];
	}
	else if (imDataIn->getPixelType() == imHALF) {
	    imHalf *bufH = (imHalf *)buffer;
	    for (j = startSamp; j <= endSamp; j++)
                bufIn[countBuf++] = bufH[j];
	}
    }
    bufIn[countBuf] = 0;
    delete [] buffer;

    // Now deal with the target image

    int *bufOut = new int[suchma.dimz * suchma.dims + 1];
    suchma.ptr_m = bufOut;
 
    // Read image
 
    ImageData *imDataOut = subDisplayerOut->getImageData();
    pixelSize = imDataOut->getPixelSize();
    numberSamples = imDataOut->getNumbSamples();
    lineWidth = pixelSize * numberSamples;
    buffer = new unsigned char[lineWidth];
 
    startLine = (int)(floor(sy + .5) - (suchma.dimz / 2));
    endLine = (int)(floor(sy + .5) + (suchma.dimz / 2));
    startSamp = (int)(floor(sx + .5) - (suchma.dims / 2));
    endSamp = (int)(floor(sx + .5) + (suchma.dims / 2));

    countBuf = 0;
    for (i = startLine; i <= endLine; i++) {
        imDataOut->readLine(BWcolor, i, buffer);
        if (imDataOut->getPixelType() == imBYTE) {
            imByte *bufBout = (imByte *)buffer;
            for (j = startSamp; j <= endSamp; j++)
                bufOut[countBuf++] = bufBout[j];
        }
        else if (imDataOut->getPixelType() == imHALF) {
            imHalf *bufHout = (imHalf *)buffer;
            for (j = startSamp; j <= endSamp; j++)
                bufOut[countBuf++] = bufHout[j];
        }
    }
    bufOut[countBuf] = 0;
    delete [] buffer;

    // Call the subroutine that does correlation
    // References for the matching algorithm:
    // Ackermann F. (1984): High Precision Digital Image Correlation. 
    //                      Proc 39th Photogrammetric Week, Stuttgart 1984.

    ERG erg; // the result will be returned in this structure
    kqkkor (&info, &musterma, &suchma, &erg);
    delete [] bufIn;
    delete [] bufOut;

    yOut = (double) sy + erg.dz;
    xOut = (double) sx + erg.ds;

    TPDPR(("%f --> %f\n", sy, yOut));
    TPDPR(("%f --> %f\n", sx, xOut));
    
    TPDPR(("Results: %.2f, %.2f, %.2f, %.2f, %.2f\n\n", 
	   erg.dz, erg.ds, erg.pmk, erg.kqk, erg.mp));

    if (_window->getMatchModeResultsDialog()->isDumpToStdout())
	printf("Match image %d to %d results: %8.2f, %8.2f, %7.4f, %7.4f, %7.4f\n",
		subDisplayerIn->getNumber(), subDisplayerOut->getNumber(),
		erg.dz, erg.ds, erg.pmk, erg.kqk, erg.mp);

    _matchModeResults[0] = erg.dz;
    _matchModeResults[1] = erg.ds;
    _matchModeResults[2] = erg.pmk;
    _matchModeResults[3] = erg.kqk;
    _matchModeResults[4] = erg.mp;

    _window->getMatchModeResultsDialog()->setValues(_matchModeResults);

    TPDPR(("End correlator routines.\n"));
    TPDPR(("--------------------------------------------------------\n"));

    return 1;
}

/////////////////////////////////////////////////////////////////////////////
// Given point model, we need to create point views for the main image 
// display and for the zoomed image display.
/////////////////////////////////////////////////////////////////////////////
void TpMatchManager::createPointViews(TpPointModel *model, 
		TpSubDisplayer *subDisplayer)
{
    TpPoint *pointI, *pointZ;
    pointI = new TpPoint(model, subDisplayer->getImageView()->getWidget());
    pointZ = new TpPoint(model, subDisplayer->getZoomView()->getWidget());
}

/////////////////////////////////////////////////////////////////////////////
// Select full match after user clicked in (x, y) on one of the 
// sub displayers.
/////////////////////////////////////////////////////////////////////////////
void TpMatchManager::processSelectMatch(const double x, const double y, 
		TpSubDisplayer *subDisplayer)
{
    _selectionMgr->clear();
    _referencedSubDisp = NULL;

    int minDistX, minDistY;
    minDistX = minDistY = _clickPrecision;
    TpMatch *closestMatch;

    SL_ListWatch<TpMatch *> watch;
    Boolean foundClosest = False;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	SL_List<TpPointModel *> *matchList = amatch->getList();
	SL_ListWatch<TpPointModel *> watch2;
	matchList->init_scan(&watch2);
	TpPointModel *apoint;
	while ((apoint = matchList->next()) != NULL) {
	    if (apoint->getImageData() == subDisplayer->getImageData()) {
		int distX = abs((int)x - (int)apoint->getX());
		int distY = abs((int)y - (int)apoint->getY());
		if ((distX < minDistX) && (distY < minDistY)) {
		    foundClosest = True;
		    closestMatch = amatch;
		}
	    }
	}
    }

    if (foundClosest) {
	_selectionMgr->selectMatchAndPoints(closestMatch);
	subDisplayer->getZoomView()->setCenter(x, y);
    }

    _dirtyFlag = True;
}

/////////////////////////////////////////////////////////////////////////////
// Select single point after user clicked in (x, y) on one of the
// sub displayers.
/////////////////////////////////////////////////////////////////////////////
void TpMatchManager::processSelectPoint(const double x, const double y, 
		TpSubDisplayer *subDisplayer)
{
    TpMatch *selectedMatch = _selectionMgr->getMatch();
    _selectionMgr->clear();
    _referencedSubDisp = NULL;

    int minDistX, minDistY;
    minDistX = minDistY = _clickPrecision;
    TpPointModel *closestPoint;
    TpMatch *closestMatch;

    SL_ListWatch<TpMatch *> watch;
    Boolean foundClosest = False;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	SL_List<TpPointModel *> *matchList = amatch->getList();
	SL_ListWatch<TpPointModel *> watch2;
	matchList->init_scan(&watch2);
	TpPointModel *apoint;
	while ((apoint = matchList->next()) != NULL) {
	    if (apoint->getImageData() == subDisplayer->getImageData()) {
		int distX = abs((int)x - (int)apoint->getX());
		int distY = abs((int)y - (int)apoint->getY());
		if ((distX < minDistX) && (distY < minDistY)) {
		    foundClosest = True;
		    closestPoint = apoint;
		    closestMatch = amatch;
		}
	    }
	}
    }

    if (foundClosest) {
	_selectionMgr->addMatch(closestMatch);
	_selectionMgr->selectPoint(closestPoint);
	subDisplayer->getZoomView()->setCenter(closestPoint->getX(), 
					       closestPoint->getY());
    }
    else {

	// Delete match only if it is so new, it has never been saved

	if (!isExistingMatch(selectedMatch))
	    delete selectedMatch;
    }

    _dirtyFlag = True;
}

void TpMatchManager::processMovePoint(TpSubDisplayer *subDisplayer, 
				const double amountX, const double amountY)
{
    TpMatch *match = _selectionMgr->getMatch();
    if (match == NULL)
	return;			// no points, so do nothing
    SL_List<TpPointModel *> *points = match->getList();
    SL_ListWatch<TpPointModel *> watch2;
    points->init_scan(&watch2);
    TpPointModel *apoint;
    while ((apoint = points->next()) != NULL) {
	if (apoint->getImageData() == subDisplayer->getImageData()) {
	    double oldX = apoint->getX();
	    double oldY = apoint->getY();
	    double newX = oldX + amountX;
	    double newY = oldY + amountY;
	    processNewPoint(newX, newY, subDisplayer, False);
	}
    }
    _dirtyFlag = True;
}

void TpMatchManager::processScrollAll(const double x, const double y,
				      TpSubDisplayer *subDisplayer)
{
    if (_autofindMode == AFFINE) {
	TpSubDisplayer *aSubDisplayer;
	for (int i = 0; i < _displayer->getNumImages(); i++) {
	    aSubDisplayer = _displayer->getImage(i);
	    if (subDisplayer == aSubDisplayer)
		continue;
	    double xOut, yOut;
	    doAffine(x, y, subDisplayer, aSubDisplayer, xOut, yOut);
	    aSubDisplayer->getZoomView()->setCenter(xOut, yOut);
	    aSubDisplayer->getImageView()->setCenter(xOut, yOut);
	}
	TPDPR(("Scroll at %f %f\n", x, y));
    }

    else if (_autofindMode == MANUAL) {
	// do nothing
    }
    
    else if (_autofindMode == SPICE) {
	//!!! Spice functions that allow to find a tiepoint go here!
    }

    else {
	fprintf(stderr, "Not a valid TpAutofindMode value!\n");
    }
}

//////////////////////////////////////////////////////////////////////////////
// isExistingMatch: Check if the match is on the list.  Return true is it is 
// on the list.  Return false if the match is not on the list (meaning it is 
// in temporary storage.
//////////////////////////////////////////////////////////////////////////////
Boolean TpMatchManager::isExistingMatch(TpMatch *match)
{
    if (match == NULL) return FALSE;

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	if (amatch == match) {
	    _matches->scan_done();
	    return TRUE;
	}
    }
    return FALSE;
}

TpMatch *TpMatchManager::getNthMatch(int n)
{
    TpMatch *match = _matches->get_nth(n-1);
    return match;
}

//////////////////////////////////////////////////////////////////////////
// Return running id of the match or the next available running id if the 
// match was not found
//////////////////////////////////////////////////////////////////////////
int TpMatchManager::getMatchNumber(TpMatch *match)
{
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    int i = 1;
    while ((amatch = _matches->next()) != NULL) {
        if (amatch == match) {
	    _matches->scan_done();
	    return i;
	}
	i++;
    }
    return i;
}

//////////////////////////////////////////////////////////////////////////
// addMatch: Add selected match to the list of matches.
//////////////////////////////////////////////////////////////////////////
void TpMatchManager::addMatch()
{
    addMatch(_selectionMgr->getMatch());
}

//////////////////////////////////////////////////////////////////////////
// addMatch: Add specific match to the list of matches.  Unselect 
// everything.
//////////////////////////////////////////////////////////////////////////
void TpMatchManager::addMatch(TpMatch *match)
{
    if (!match) {
	theErrorManager->process(Error, "Saving operation", 
				 "Nothing to save");
	return;
    }

    if (isExistingMatch(match))
	theErrorManager->process(Error, NULL, 
				 "This tiepoint had already been added!");
    else if (_fullMatchIsEnforced &&
	     (match->getList()->get_length() < _displayer->getNumImages())) {
	theErrorManager->process(Error, NULL, 
				 "Cannot add incomplete tiepoint!");
	return;
    }
    else {
	_matches->add(match);
	_nextId++;
    }

    _selectionMgr->clear();
    _referencedSubDisp = NULL;
}

///////////////////////////////////////////////////////////
// deleteCurrentSelection: Delete currently selected points 
// or the whole match.
///////////////////////////////////////////////////////////
void TpMatchManager::deleteCurrentSelection()
{
    if (_selectionMgr->isEmpty()) {
	theErrorManager->process(Error, "Delete Operation", 
	        "You have to select point or observation before deleting it");
	return;
    }

    if (_selectionMgr->isFullHouse()) {
	deleteCurrentMatch();
	_dirtyFlag = True;
	return;
    }
    else if (_fullMatchIsEnforced) {
	theErrorManager->process(Error, "Delete Point", 
			"This mode only allows to delete COMPLETE matches", 
			"see *fullMatchIsEnforced resource");
	return;
    }

    TpMatch *selectedMatch = _selectionMgr->getMatch();

    SL_ListWatch<TpPointModel *> watch;
    _selectionMgr->initScan(&watch);
    TpPointModel *apoint;
    while ((apoint = _selectionMgr->nextPoint()) != NULL) {
	_selectionMgr->deletePoint(apoint);
	selectedMatch->deletePoint(apoint);
    }
    _dirtyFlag = True;
}

///////////////////////////////////////////////////////////
// deleteAllPoints: Delete all points belonging to certain image.
///////////////////////////////////////////////////////////
void TpMatchManager::deleteAllPoints(ImageData *id)
{
    _selectionMgr->clear();

    SL_ListWatch<TpMatch *> watch;

    TpMatch *amatch;
    _matches->init_scan(&watch);
    while ((amatch = _matches->next()) != NULL) {
	amatch->deletePoint(id);
    }	
}

///////////////////////////////////////////////////////////
// deleteLastAddedMatch: Delete last added match
///////////////////////////////////////////////////////////
void TpMatchManager::deleteLastAddedMatch()
{
    _selectionMgr->clear();

    SL_ListWatch<TpMatch *> watch;

    _matches->init_scan(&watch);
    for (int i = 0; i < _matches->get_length(); i++)
	_matches->next();
    TpMatch *match = _matches->remove_current();
    delete match;
    _matches->scan_done();
}

///////////////////////////////////////////////////////////
// deleteCurrentMatch: Delete selected match.
///////////////////////////////////////////////////////////
void TpMatchManager::deleteCurrentMatch()
{
    TpMatch *toDelete = _selectionMgr->getMatch();
    _selectionMgr->clear();

    // Find a match that needs to be deleted

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	if (amatch == toDelete) {
	    _matches->remove_current();
	    _matches->scan_done();
	    delete amatch;
	    toDelete = NULL; //!!! Not sure if this is needed
	    break;
	}
    }

    // If the match was new and not saved, then we have to delete it 
    // explicitly

    if (toDelete) delete toDelete;
}

void TpMatchManager::listPoints()
{
    SL_ListWatch<TpMatch *> watch;
    int i = 0;

    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	char buf[512];
	sprintf(buf, "Tiepoint #%d:", ++i);
	theErrorManager->process(Warning, NULL, buf);
	amatch->listPoints();
    }
    theErrorManager->process(Warning, NULL,
                "====================================================");
}

void TpMatchManager::newMatchSelected()
{
    _displayer->newMatchSelected(_selectionMgr->getMatch());
    _matchBrowser->newMatchSelected(_selectionMgr->getMatch());
}

void TpMatchManager::setPointShape(TpPointSymbolShapeType shape)
{
    _pointShape = shape;

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setPointShape(shape);
    }
}

void TpMatchManager::setTagPosition(TpTagPosition tagPosition)
{
    _tagPosition = tagPosition;
 
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setTagPosition(_tagPosition);
    }
}

void TpMatchManager::setPointWidth(int width)
{
    _pointWidth = width;

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setPointWidth(width);
    }
}

void TpMatchManager::setPointHeight(int height)
{
    _pointHeight = height;

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setPointHeight(height);
    }
}

void TpMatchManager::setPointDimensions(int width, int height)
{
    _pointWidth = width;
    _pointHeight = height;

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setPointDimensions(width, height);
    }
}

void TpMatchManager::setPointColor(char *str)
{
    _pointColor = sdup(str);
 
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setPointColor(str);
    }
}

void TpMatchManager::setPointColorSel(char *str)
{
    _pointColorSel = sdup(str);
 
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->setPointColorSel(str);
    }
}

void TpMatchManager::showPointLabels(Boolean show)
{
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->showPointLabels(show);
    }    
}

void TpMatchManager::rotationChanged(ImageData *id)
{
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	amatch->rotationChanged(id);
    }
}

void TpMatchManager::setPmkCorrParm(int p)
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_window->getMatchModeDialog()->getSetMatchModeValuesCmd()->getValue();
    value->_pmk = p;
    _window->getMatchModeDialog()->getSetMatchModeValuesCmd()->execute(value);
}

void TpMatchManager::setLsmCorrParm(int p)
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_window->getMatchModeDialog()->getSetMatchModeValuesCmd()->getValue();
    value->_lsm = p;
    _window->getMatchModeDialog()->getSetMatchModeValuesCmd()->execute(value);
}

void TpMatchManager::setWinCorrParm(int p)
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_window->getMatchModeDialog()->getSetMatchModeValuesCmd()->getValue();
    value->_searchWindow = p;
    _window->getMatchModeDialog()->getSetMatchModeValuesCmd()->execute(value);
}

void TpMatchManager::setAccCorrParm(float p)
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_window->getMatchModeDialog()->getSetMatchModeValuesCmd()->getValue();
    value->_accuracy = p;
    _window->getMatchModeDialog()->getSetMatchModeValuesCmd()->execute(value);
}

void TpMatchManager::setCorCorrParm(float p)
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_window->getMatchModeDialog()->getSetMatchModeValuesCmd()->getValue();
    value->_threshold = p;
    _window->getMatchModeDialog()->getSetMatchModeValuesCmd()->execute(value);
}

// Color code points based on the general qualifier
void TpMatchManager::colorCodePointsGen(int n)
{
    int minValue;
    _genQualMgr->getMinValue(n, minValue);
    int maxValue;
    _genQualMgr->getMaxValue(n, maxValue);

    float a = 255.0 / float(maxValue - minValue);
    float b = 255.0 - (maxValue * a);

    DPR(("min = %d, max = %d, a = %f, b = %f\n", minValue, maxValue, a, b));

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->colorCodePointsGen(n, a, b);
    }
}

// Color code points based on the point qualifier
void TpMatchManager::colorCodePointsPoint(int n)
{
    int minValue;
    int status;
    status = _pointQualMgr->getMinValue(n, minValue);
    if (status == -1)
	return;		// no usable qualifier
    int maxValue;
    status = _pointQualMgr->getMaxValue(n, maxValue);
    if (status == -1)
	return;		// no usable qualifier

    float a = 255.0 / float(maxValue - minValue);
    float b = 255.0 - (maxValue * a);

    DPR(("min = %d, max = %d, a = %f, b = %f\n", minValue, maxValue, a, b));

    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
        amatch->colorCodePointsPoint(n, a, b);
    }
}

void TpMatchManager::redoIds()
{
    _nextId = _startId - 1;
    SL_ListWatch<TpMatch *> watch;
    _matches->init_scan(&watch);
    TpMatch *amatch;
    while ((amatch = _matches->next()) != NULL) {
	amatch->setId(_nextId + 1);
	_nextId++;
    }

    _dirtyFlag = True;
}

#if defined(vms) || defined(__VMS)
#pragma define_template SL_List<TpMatch *>
#pragma define_template SL_ListWatch<TpMatch *>
#pragma define_template SL_List<TpPointModel *>
#pragma define_template SL_ListWatch<TpPointModel *>
#pragma define_template SL_List<TpSubDisplayer *>
#pragma define_template SL_ListWatch<TpSubDisplayer *>
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatch.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpMatch.cc: This class keeps a list of a single tiepoint set.
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher     JPL
//////////////////////////////////////////////////////////////////////////////
#include "TpMatch.h"
#include "TpMatchManager.h"
#include "TpPointModel.h"
#include "TpSelectionMgr.h"
#include "TpQualGroup.h"
#include "TpQualGroupMgr.h"
#include "ErrorManager.h"
#include "TpDefs.h"
#include <iostream>

TpMatch::TpMatch(TpMatchManager *manager)
{
    _manager = manager;

    _current = FALSE;
    _list = new SL_List<TpPointModel *>;

    _genQual = new TpQualGroup(_manager->getGenQualMgr());

    if (_genQual->getNumQuals() > 0)
	if (_genQual->getType(0) == TpFull) //!!! && !strcmp(name, "ID")
	    _genQual->setValue(0, _manager->getNextId() + 1);
}

TpMatch::~TpMatch()
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *current;

    _list->init_scan(&watch);
    while ((current = _list->next()) != NULL) {
	_list->remove_current();
	delete current;
    }
    delete _list;
}

void TpMatch::setId(int id)
{
    if (_genQual->getNumQuals() > 0) {
	if (_genQual->getType(0) == TpFull) {
	    _genQual->setValue(0, id);
	    redrawAll();
	}
    }
}

void TpMatch::redrawAll()
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
	apoint->updateViews();
    }
}

///////////////////////////////////////////////////////////////////
// Return point model that is being added.  Return True if point is 
// new, False if already exist.
///////////////////////////////////////////////////////////////////
Boolean TpMatch::addPoint(char *filename, int n, ImageData *imageData, 
			  const double x, const double y,
			  TpQualGroupMgr *qualMgr,
			  TpPointModel *&added)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;

    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
	if (apoint->getImageData() == imageData) {
	    apoint->setXY(x, y);
	    added = apoint;
	    return False;
	}
    }

    // Prepare a new point

    added = new TpPointModel(filename, n, imageData, this, qualMgr, x, y);

    addPoint(added);

    return True;
}

///////////////////////////////////////////////////////////////////
// This is fast and unsafe way to add point.  The caller should 
// make sure that the point doesn't already exist in the match.
///////////////////////////////////////////////////////////////////
void TpMatch::addPoint(TpPointModel *newPoint)
{
    // Copy the list, adding the new point in the right sequence, so that
    // points are sorted in ascending order
 
    SL_List<TpPointModel *> *newList = new SL_List<TpPointModel *>;
    SL_ListWatch<TpPointModel *> watch;
    _list->init_scan(&watch);
    TpPointModel *apoint;
    Boolean isAdded = False;
    int n = newPoint->getImageNumber();
    while ((apoint = _list->next()) != NULL) {
        if (!isAdded && (apoint->getImageNumber() > n)) {
            newList->add(newPoint);
	    isAdded = True;
	}
        newList->add(apoint);
    }
    if (!isAdded)
	newList->add(newPoint);
 
    delete _list;
    _list = newList;
}

void TpMatch::deletePoint(TpPointModel *p)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *toDelete;

    _list->init_scan(&watch);
    while ((toDelete = _list->next()) != NULL) {
	if (toDelete == p) {
	    _list->remove_current();
	    delete p;
	    _list->scan_done();
	    break;
	}
    }
}

void TpMatch::deletePoint(ImageData *id)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *toDelete;

    _list->init_scan(&watch);
    while ((toDelete = _list->next()) != NULL) {
        if (toDelete->getImageData() == id) {
            _list->remove_current();
            delete toDelete;
            _list->scan_done();
            break;
        }
    }
}

void TpMatch::clear()
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *toDelete;

    _list->init_scan(&watch);
    while ((toDelete = _list->next()) != NULL) {
	_list->remove_current();
	delete toDelete;
    }
}

void TpMatch::setCurrent(TpSelectionMgr *selectionMgr)
{
    _current = True;

    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;

    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
	selectionMgr->addPoint(apoint);
    }

    selectionMgr->addMatch(this);
}

//////////////////////////////////////////////////////////
// listPoints: Output info about each point in the match
//////////////////////////////////////////////////////////
void TpMatch::listPoints()
{
    if (_genQual->getNumQuals() > 0) {
        if (_genQual->getType(0) == TpFull) { //!!! && !strcmp(name, "ID")
	    char buf[256];
	    sprintf(buf, "Point ID = %s", _genQual->valueToString(0));
            theErrorManager->process(Warning, NULL, buf); 
	}
    }

    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *point;

    _list->init_scan(&watch);
    while ((point = _list->next()) != NULL) {
	point->listPoint();
    }
    theErrorManager->process(Warning, NULL, 
		"----------------------------------------------------");
}

void TpMatch::setPointShape(TpPointSymbolShapeType shape)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setPointShape(shape);
    }
}

void TpMatch::setTagPosition(TpTagPosition tagPosition)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setTagPosition(tagPosition);
    }
}

void TpMatch::setPointWidth(int width)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setPointWidth(width);
    }
}

void TpMatch::setPointHeight(int height)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setPointHeight(height);
    }
}

void TpMatch::setPointDimensions(int width, int height)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setPointDimensions(width, height);
    }
}

void TpMatch::setPointColor(char *str)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setPointColor(str);
    }
}

void TpMatch::setPointColorSel(char *str)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->setPointColorSel(str);
    }
}

void TpMatch::showPointLabels(Boolean show)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->showPointLabels(show);
    }
}

void TpMatch::rotationChanged(ImageData *id)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
	if (id == apoint->getImageData())
	    apoint->rotationChanged();
    }
}

void TpMatch::colorCodePointsGen(int n, float a, float b)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->colorCodePointsGen(n, a, b);
    }
}

void TpMatch::colorCodePointsPoint(int n, float a, float b)
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;
    _list->init_scan(&watch);
    while ((apoint = _list->next()) != NULL) {
        apoint->colorCodePointsPoint(n, a, b);
    }
}

int TpMatch::initScan(SL_ListWatch<TpPointModel *> *watch)
{
    int status;
    status = _list->init_scan(watch);
    // failed = _list->failed;
    return status;
}

TpPointModel *TpMatch::next()
{
    TpPointModel *p = _list->next();
    // failed = _list->failed;
    return p;
}

int TpMatch::scanDone()
{
    int status;
    status = _list->scan_done();
    // failed = _list->failed;
    return status;
}

int TpMatch::matchCmp(TpMatch *match, void *toMatch)
{
    return (match == (TpMatch *) toMatch);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DrawObject.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// DrawObject.cc: 
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#include "DrawObject.h"

DrawObject::DrawObject(Widget iw, double x, double y)
{
    _iw = iw;
    _x = x;
    _y = y;

    _id = 0;
    XGCValues values;
    _gc = XvicImageCreateGC(_iw, 0L, &values);
    setColor("black");
}

DrawObject::~DrawObject()
{
    erase();
}

void DrawObject::setColor(XColor *color)
{
    _color = XvicImageGetGrColor(_iw, color);
}

void DrawObject::setColor(int r, int g, int b)
{
    _color = XvicImageGetGrColorRGB(_iw, r, g, b);
}

void DrawObject::setColor(const char *str)
{
    XColor xcolor;
    XParseColor(XtDisplay(_iw), DefaultColormapOfScreen(XtScreen(_iw)),
		(char *)str, &xcolor);
    setColor(&xcolor);
}

void DrawObject::move(double x, double y)
{
    XvicImageMoveObject(_iw, _id, x, y);
}

void DrawObject::erase()
{
    XvicImageEraseObject(_iw, _id);
    _id = 0;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPoint.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpPoint.cc: This class is responsible for drawing a visual representation 
// of a tiepoint over the image.  It supports different sizes and shapes.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#include "TpPoint.h"
#include "TpPointModel.h"
#include "TpQualGroup.h"
#include "TpMatch.h"
#include "TpDefs.h"
#include "ImageData.h"
#include "ErrorManager.h"
#include <Xm/RepType.h>
#include <stdio.h>

int TpPoint::_tpPointInit = False;

XtResource TpPoint::_resources [ ] = {
 {
    (char *)"fontList",
    (char *)"FontList",
    XmRString,
    sizeof(String),
    XtOffset(TpPoint *, _fontname),
    XmRImmediate,
    (XtPointer)"6x10",
 },
};

TpPoint::TpPoint(TpPointModel *model, Widget iw)
	: DrawObject(iw, model->getX(), model->getY())
{
    _model = model;

    XtGetApplicationResources(_iw, (XtPointer)this, 
		_resources, XtNumber(_resources),
                NULL, 0);

    _fontStruct = XLoadQueryFont(XtDisplay(_iw), _fontname);
    if(_fontStruct == NULL) {
        theErrorManager->process(Error, "TpPoint", "No such font", _fontname);
        _fontStruct = XQueryFont(XtDisplay(_iw), 
				 XGContextFromGC(DefaultGCOfScreen(
				     XtScreen(_iw))));
    }

    XGCValues values;
    values.font = _fontStruct->fid;
    XvicImageChangeGC(_iw, _gc, GCFont, &values);

    _model->attachView(this);
}

TpPoint::~TpPoint()
{
    if(_iw && _fontStruct)
	XFreeFont(XtDisplay(_iw), _fontStruct);
}

XvicID TpPoint::draw()
{
    XvicID id;

    _width = _model->getPointWidth();
    _height = _model->getPointHeight();

    if (_model->isCurrent()) 
	setColor(_model->getPointColorSel());
    else 
	setColor(_model->getPointColor());

    switch (_model->getPointShape()) {
	case CrossWithDot: 
	    id = drawCrossDot();
	    break;
	case Rectangle: 
	    id = drawRectangle();
	    break;
        case Dot:
	    id = drawDot();
	    break;
        case Cross45:
	    id = drawCross45();
	    break;
	case CrossWithHole45:
	    id = drawCrossWithHole45();
	    break;
        case RectangleWithCrossesWithDot:
	    id = drawCrossDot();
	    id = drawRectangle();
	    id = drawCrossWithHole45();
	    break;
	default: 
	    id = drawCrossDot();
    }

    TpQualGroup *qualGroup = _model->getMatch()->getGenQual();
    if (qualGroup->getNumQuals() > 0) {
        _labelString = qualGroup->valueToString(0); //!!! customize this
    }
    else
        _labelString = NULL;

    if (_model->getShowPointLabels() && _labelString && strlen(_labelString)) {
	id = drawLabel();
    }

    return id;
}

XvicID TpPoint::drawRectangle()
{
    _id = XvicImageDrawRectangle(_iw, _id, _gc, _color, 
				 _x-(_width/2), _y-(_height/2), 
				 _width, _height);
    return _id;
}

XvicID TpPoint::drawDot()
{
    _id = XvicImageFillRectangle(_iw, _id, _gc, _color,
				 _x, _y, 1, 1);
    return _id;
}

XvicID TpPoint::drawCross45()
{
    _id = XvicImageDrawLine(_iw, _id, _gc, _color,
                _x-(_width/2), _y-(_height/2),
                _x+(_width/2), _y+(_height/2));
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x-(_width/2), _y+(_height/2),
                _x+(_width/2), _y-(_height/2));

    return _id;
}

XvicID TpPoint::drawCrossDot()
{
    _id = XvicImageDrawLine(_iw, _id, _gc, _color,
                _x, _y-(_height/2),
                _x, _y-(_height/4));
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x-(_width/2), _y,
                _x-(_width/4), _y);
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x+(_width/4), _y,
                _x+(_width/2), _y);
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x, _y+(_height/2),
                _x, _y+(_height/4));
    XvicImageDrawPoint(_iw, _id, _gc, _color,
		_x, _y);

    return _id;
}

XvicID TpPoint::drawCrossWithHole45()
{
    _id = XvicImageDrawLine(_iw, _id, _gc, _color,
                _x-(_width/2), _y-(_height/2), 
		_x-(_width/4), _y-(_height/4));
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x-(_width/2), _y+(_height/2),
                _x-(_width/4), _y+(_height/4));
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x+(_width/2), _y-(_height/2),
                _x+(_width/4), _y-(_height/4));
    XvicImageDrawLine(_iw, _id, _gc, _color,
                _x+(_width/2), _y+(_height/2),
                _x+(_width/4), _y+(_height/4));

    return _id;
}

XvicID TpPoint::drawLabel()
{
    switch (_model->getTagPosition()) {
    case NorthEast:
	_id = XvicImageDrawString(_iw, _id, _gc, _color,
				  _x+(_width/2), _y-(_height/2),
				  _labelString, strlen(_labelString),
				  XvicJUST_LEFT);
	break;
    case NorthWest:
	_id = XvicImageDrawString(_iw, _id, _gc, _color,
                                  _x-(_width/2), _y-(_height/2),
                                  _labelString, strlen(_labelString),
                                  XvicJUST_RIGHT);
        break;
    case SouthWest:
        _id = XvicImageDrawString(_iw, _id, _gc, _color,
                                  _x-(_width/2), _y+(_height),
                                  _labelString, strlen(_labelString),
                                  XvicJUST_RIGHT);
        break;
    case SouthEast:
        _id = XvicImageDrawString(_iw, _id, _gc, _color,
                                  _x+(_width/2), _y+(_height),
                                  _labelString, strlen(_labelString),
                                  XvicJUST_LEFT);
        break;
    case Center:
        _id = XvicImageDrawString(_iw, _id, _gc, _color,
                                  _x, _y,
                                  _labelString, strlen(_labelString),
                                  XvicJUST_LEFT);
        break;
    }
    return _id;
}

void TpPoint::update(TpPointModel *model)
{
    _model->getImageData()->transImageToDisplayCoords(model->getX(),
                                                      model->getY(),
                                                      &_x, &_y);

    if (_id == 0) {
	draw();
    }
    else {
	erase();
	draw();
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPointModel.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpPointModel.cc: This class serves as a Model for individual
// points (observations).
//////////////////////////////////////////////////////////////////////////////
// Vadim Parizher     JPL
//////////////////////////////////////////////////////////////////////////////
#include "TpPointModel.h"
#include "TpPoint.h"
#include "TpMatch.h"
#include "TpQualGroup.h"
#include "TpQualGroupMgr.h"
#include "TpMatchManager.h"
#include "ViewMacros.h"
#include "TpDefs.h"
#include "ErrorManager.h"
#include "file_no_path.h"
#include "ibisfile.h"
#include "zvproto.h"
#include <assert.h>
#include <stdio.h>

//!!!! Comment out next line before delivery!
// #define DEBUG
#ifdef DEBUG
#define DPR(x) printf x
#else
#define DPR(x)
#endif

TpPointSymbolShapeType TpPointModel::_pointShape = Dot;
TpTagPosition TpPointModel::_tagPosition = NorthEast;
int TpPointModel::_pointWidth = 10;
int TpPointModel::_pointHeight = 10;
char *TpPointModel::_pointColor = (char *)"red";
char *TpPointModel::_pointColorSel = (char *)"green";
Boolean TpPointModel::_showLabel = True;
#ifndef __VMS
  char *TpPointModel::_colorCodeFilename = (char *)"$V2DATA/gui/ps1.ibis-2";
#else
  char *TpPointModel::_colorCodeFilename = (char *)"v2$data:[gui]ps1.ibis-2";
#endif

TpPointModel::TpPointModel(char *filename, int imageNumber, ImageData *image, 
			   TpMatch *match, TpQualGroupMgr *qualMgr, 
			   double x, double y)
{
    _filename = sdup(filename);
    _imageNumber = imageNumber;
    _image = image;
    _x = x;
    _y = y;
    _views = NULL;
    _numViews = 0;
    _isCurrent = False;

    assert(match != NULL);
    _match = match;
    _pointQual = new TpQualGroup(qualMgr);

    if (_pointQual->getNumQuals() > 0)
        if (_pointQual->getType(0) == TpReal) //!!! && !strcmp(name,"Corr_Parm")
            _pointQual->setValue(0, 
			_match->getMatchManager()->getMatchModeResult(3));

}

TpPointModel::~TpPointModel()
{
    for (int i = 0; i < _numViews; i++)
	delete _views[i];
    delete [] _filename;
}

void TpPointModel::attachView(TpPoint *point)
{
    AttachViewMacro(TpPoint, _views, _numViews, point);
    point->update(this);
}

void TpPointModel::detachView(TpPoint *point)
{
    DetachViewMacro(TpPoint, _views, _numViews, point);
    point->update(this);
}

void TpPointModel::updateViews()
{
    for (int i = 0; i < _numViews; i++) 
	_views[i]->update(this);
}

void TpPointModel::setX(double x) 
{ 
    _x = x; 
    updateViews();
    if (_pointQual->getNumQuals() > 0)
        if (_pointQual->getType(0) == TpReal) //!!! && !strcmp(name,"Corr_Parm")
            _pointQual->setValue(0,
                        _match->getMatchManager()->getMatchModeResult(3));
}

void TpPointModel::setY(double y) 
{ 
    _y = y; 
    updateViews();
    if (_pointQual->getNumQuals() > 0)
        if (_pointQual->getType(0) == TpReal) //!!! && !strcmp(name,"Corr_Parm")
            _pointQual->setValue(0,
                        _match->getMatchManager()->getMatchModeResult(3));
 
}

void TpPointModel::setXY(double x, double y) 
{ 
    _x = x; 
    _y = y; 
    updateViews();
    if (_pointQual->getNumQuals() > 0)
        if (_pointQual->getType(0) == TpReal) //!!! && !strcmp(name,"Corr_Parm")
            _pointQual->setValue(0,
                        _match->getMatchManager()->getMatchModeResult(3));
}

void TpPointModel::setCurrent(Boolean isCurrent)
{
    _isCurrent = isCurrent;
    updateViews();	
}

void TpPointModel::listPoint()
{
    char buf[512];
    sprintf(buf, "\tImage %d: \tL = %.3f\t S = %.3f", _imageNumber, _y, _x);
    theErrorManager->process(Warning, NULL, buf);
}

void TpPointModel::setPointShape(TpPointSymbolShapeType shape)
{
    _pointShape = shape;
    updateViews();
}

void TpPointModel::setTagPosition(TpTagPosition position)
{
    _tagPosition = position;
    updateViews();
}

void TpPointModel::setPointWidth(int width)
{
    _pointWidth = width;
    updateViews();
}

void TpPointModel::setPointHeight(int height)
{
    _pointHeight = height;
    updateViews();
}

void TpPointModel::setPointDimensions(int width, int height)
{
    _pointWidth = width;
    _pointHeight = height;
    updateViews();
}

void TpPointModel::setPointColor(char *str)
{
    _pointColor = sdup(str);
    updateViews();
}

void TpPointModel::setPointColorSel(char *str)
{
    _pointColorSel = sdup(str);
    updateViews();
}

void TpPointModel::showPointLabels(Boolean show)
{
    _showLabel = show;
    updateViews();
}

void TpPointModel::rotationChanged()
{
    updateViews();
}

void TpPointModel::colorCodePointsGen(int n, float a, float b)
{
    float value;
    if (_match->getGenQual()->getType(n) == TpText)
        return;
    else if (_match->getGenQual()->getType(n) == TpFull) {
        int valueF;
        _match->getGenQual()->getValue(n, valueF);
        value = (float)valueF;
    }
    else if (_match->getGenQual()->getType(n) == TpReal) {
        _match->getGenQual()->getValue(n, value);
    }
    int y = (int)(a * value + b);
    printf("value=%f, y=%d\n", value, y);

    // Read ibis file
    int unit, ibis, status, record, i;
    int lut[256][3];

    char *filename = _colorCodeFilename;
    // open IBIS file for reading
    status = zvunit(&unit, (char *)"in_file",  1, "u_name", filename, NULL);
    if (status != 1) return;
    status = IBISFileOpen(unit, &ibis, (char *)IMODE_READ, 0, 0, 0, 0);
    if (status != 1) return;

    char *stripFilename = sdup(filename);
    file_no_path(stripFilename);

    ICLGetLOOKUP_TABLE(ibis, (char *)"$MyLut", 0, (char *)"PSEUDOCOLOR",
							stripFilename);
    ICLGetRGB(ibis, (char *)"$MyRED", (char *)"$MyGRN", (char *)"$MyBLU",
							(char *)"$MyLut");
    
    status = IBISRecordOpen(ibis, &record, (char *)"$MyRED | $MyGRN | $MyBLU",
			    0, 0, (char *)IFMT_FULL);
    if (status != 1) return;

    for (i=1; i<=256; i++) {
	status = IBISRecordRead(record, (char*)lut[i-1], i);
	if (status != 1) return;
    }

    IBISFileClose(ibis, 0);

    delete [] stripFilename;

    int red = lut[y][0];
    int grn = lut[y][1];
    int blu = lut[y][2];

    printf("red = %d, green = %d, blue = %d\n", red, grn, blu);
    char buf[16];
    sprintf(buf, "#%02x%02x%02x", red, grn, blu);
    printf("color = %s\n", buf);
    setPointColor(buf);
}

void TpPointModel::colorCodePointsPoint(int n, float a, float b)
{
    float value;
    if (_pointQual->getType(n) == TpText)
	return;
    else if (_pointQual->getType(n) == TpFull) {
	int valueF;
	_pointQual->getValue(n, valueF);
	value = (float)valueF;
    }
    else if (_pointQual->getType(n) == TpReal) {
	_pointQual->getValue(n, value);
    }
    int y = (int)(a * value + b);
    DPR(("value=%f, y=%d\n", value, y));

    // Read ibis file
    int unit, ibis, status, record, i;
    int lut[256][3];
 
#ifndef __VMS
    char *filename = (char *)"$V2DATA/gui/ps1.ibis-2";
#else
    char *filename = (char *)"v2$data:[gui]ps1.ibis-2";
#endif
    // open IBIS file for reading
    status = zvunit(&unit, (char *)"in_file",  1, "u_name", filename, NULL);
 
    status = IBISFileOpen(unit, &ibis, (char *)IMODE_READ, 0, 0, 0, 0);
    if(status!=1) return;

    char *stripFilename = sdup(filename);
    file_no_path(stripFilename);
 
    ICLGetLOOKUP_TABLE(ibis, (char *)"$MyLut", 0, (char *)"PSEUDOCOLOR",
							stripFilename);
    ICLGetRGB(ibis, (char *)"$MyRED", (char *)"$MyGRN", (char *)"$MyBLU",
							(char *)"$MyLut");
 
    status = IBISRecordOpen(ibis, &record, (char *)"$MyRED | $MyGRN | $MyBLU",
                            0, 0, (char *)IFMT_FULL);
    if (status != 1) return;
 
    for (i=1; i<=256; i++) {
        status = IBISRecordRead(record, (char*)lut[i-1], i);
        if (status != 1) return;
    }
 
    IBISFileClose(ibis, 0);
 
    delete [] stripFilename;

    int red = lut[y][0];
    int grn = lut[y][1];
    int blu = lut[y][2];
 
    DPR(("red = %d, green = %d, blue = %d\n", red, grn, blu));
    char buf[16];
    sprintf(buf, "#%02x%02x%02x", red, grn, blu);
    DPR(("color = %s\n", buf));
    setPointColor(buf);
}

void TpPointModel::setPointShapeStatic(TpPointSymbolShapeType shape)
{
    _pointShape = shape;
}

void TpPointModel::setTagPositionStatic(TpTagPosition tagPosition)
{
    _tagPosition = tagPosition;
}
 
void TpPointModel::setPointWidthStatic(int width)
{
    _pointWidth = width;
}
 
void TpPointModel::setPointHeightStatic(int height)
{
    _pointHeight = height;
}
 
void TpPointModel::setPointDimensionsStatic(int width, int height)
{
    _pointWidth = width;
    _pointHeight = height;
}

void TpPointModel::setPointColorStatic(char *str)
{
    _pointColor = sdup(str);
}

void TpPointModel::setPointColorSelStatic(char *str)
{
    _pointColorSel = sdup(str);
}

int TpPointModel::pointCmp(TpPointModel *point, void *toPoint)
{
    return (point == (TpPointModel *) toPoint);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpZoomControl.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// TpZoomControl.cc: Zoom control is not handled through 
// Cmd/CmdInterface mechanism as usually.  There is only one interface, 
// so user interactions are handled directly through callbacks rather
// than commands.
///////////////////////////////////////////////////////////////////
#include "TpZoomControl.h"
#include "TpImageView.h"
#include "PrefManager.h"
#include "TpZoomControlPrefView.h"
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>

XtResource TpZoomControl::_resources [ ] = {
  {
    (char *)"zoomValue",
    (char *)"ZoomValue",
    XmRInt,
    sizeof(int),
    XtOffset (TpZoomControl *, _zoomValue),
    XmRImmediate,
    (XtPointer) 2,
  },
};

void TpZoomControl::reload(TpZoomControl *copy)
{
    if (_zoomValue != copy->_zoomValue)
        setZoom(copy->_zoomValue);
}

TpZoomControl::TpZoomControl(Widget parent, const char *name,
						TpImageView *zoomView)
	: UIComponent(name)
{
    _zoomView = zoomView;

    _w = XtVaCreateWidget(_name, xmFormWidgetClass, parent, NULL);
    installDestroyHandler();

    getResources(_resources, XtNumber(_resources));

    char path[100];
    sprintf(path, "*%s*", _name);
    thePrefManager->registerResources(this,
                                      _resources, XtNumber(_resources),
				      path, new TpZoomControlPrefView());

    // Create zoom label widget 

    Widget label = XtVaCreateManagedWidget("zoomLabel", 
				xmLabelWidgetClass, _w,
				NULL);

    // Create left button to decrement zoom by one

    Widget decZoom = XtVaCreateManagedWidget("decZoom", 
				xmPushButtonWidgetClass, _w, 
				NULL);
    XtAddCallback(decZoom, XmNactivateCallback, 
				&TpZoomControl::decZoomCallback,
				(XtPointer)this);

    // Create text field to keyin zoom value

    _keyinZoom = XtVaCreateManagedWidget("keyinZoom", 
				xmTextFieldWidgetClass, _w,
				NULL);
    XtAddCallback(_keyinZoom, XmNlosingFocusCallback, 
				&TpZoomControl::setZoomCallback, 
				(XtPointer)this);
    XtAddCallback(_keyinZoom, XmNactivateCallback,
				&TpZoomControl::setZoomCallback,
				(XtPointer)this);

    // Create right button to increment zoom by one

    Widget incZoom = XtVaCreateManagedWidget("incZoom",
				xmPushButtonWidgetClass, _w,
				NULL);
    XtAddCallback(incZoom, XmNactivateCallback,
				&TpZoomControl::incZoomCallback,
				(XtPointer)this);

    // Set form attachments

    XtVaSetValues(label,
                XmNtopAttachment,       XmATTACH_NONE,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_NONE,
                XmNbottomAttachment,    XmATTACH_FORM,
                NULL);
    XtVaSetValues(decZoom, 
		XmNtopAttachment,       XmATTACH_NONE,
		XmNleftAttachment,      XmATTACH_WIDGET,
		XmNleftWidget, 		label,
		XmNrightAttachment,     XmATTACH_NONE,
		XmNbottomAttachment,    XmATTACH_FORM,
		NULL);
    XtVaSetValues(_keyinZoom,
                XmNtopAttachment,       XmATTACH_NONE,
                XmNleftAttachment,      XmATTACH_WIDGET,
		XmNleftWidget, 		decZoom,
                XmNrightAttachment,     XmATTACH_WIDGET,
		XmNrightWidget,		incZoom,
                XmNbottomAttachment,    XmATTACH_FORM,
                NULL);
    XtVaSetValues(incZoom,
                XmNtopAttachment,       XmATTACH_NONE,
                XmNleftAttachment,      XmATTACH_NONE,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNbottomAttachment,    XmATTACH_FORM,
                NULL);

    this->setZoom(_zoomValue);
}

void TpZoomControl::incZoomCallback(Widget, XtPointer clientData,
			XtPointer)
{
    TpZoomControl *obj;
    obj = (TpZoomControl *)clientData;
    if (obj != NULL)
	obj->incZoom(1);
}

void TpZoomControl::decZoomCallback(Widget, XtPointer clientData,
                        XtPointer)
{
    TpZoomControl *obj;
    obj = (TpZoomControl *)clientData;
    if (obj != NULL)
        obj->incZoom(-1);
}

void TpZoomControl::setZoomCallback(Widget w, XtPointer clientData, 
			XtPointer)
{
    // Accept only positive ineger values.  If illegal value was 
    // typed, assume 1 and set that value back into the text field.

    int i = atoi(XmTextFieldGetString(w));
    //if (i <= 0) i = 1;
    char buf[16]; 
    sprintf(buf, "%d", i);
    XmTextFieldSetString(w, buf);

    TpZoomControl *obj;
    obj = (TpZoomControl *)clientData;
    if (obj != NULL)
	obj->setZoom(i);
}

//////////////////////////////////////////////////////////////////
// incZoom: Increment zoom by an integer.  Note that since zoom 
// values are assumed to be the same in both x and y directions, 
// only x value is obtained from the current zoom factor.
/////////////////////////////////////////////////////////////////
void TpZoomControl::incZoom(int i)
{
    ZoomFactor zoom = _zoomView->getImageZoom();
    int xin = zoom.getXIn();
    if (xin == 1) {
	int xout = zoom.getXOut();
	if (xout != 1)
	    xin = (-1) * xout;
    }
    if ((xin > 0) && (i == (-1)*xin)) xin = -2; // Special case
    else xin += i;

    if (xin == -1) xin = 1;

    setZoom(xin);
}

void TpZoomControl::setZoom(int i)
{
    if (i == -1) i = 1;

    _zoomValue = i;

    // Retain the center of the zoom window while zooming

    int x1, y1, x2, y2;
    XvicImageDisplayBounds(_zoomView->getWidget(), &x1, &y1, &x2, &y2);
    int x = (x2 + x1) / 2;
    int y = (y2 + y1) / 2;

    // Zoom to the required factor

    ZoomFactor *zoom;
    if (i == 0)
	i = 1;
    if (i > 0)
	zoom = new ZoomFactor(i, i, 1, 1);
    else if (i < 0)
	zoom = new ZoomFactor(1, 1, abs(i), abs(i));

    _zoomView->setUserZoom(*zoom);

    Dimension width, height;
    _zoomView->getViewSize(width, height);

    ZoomFactor zf = _zoomView->getImageZoom();
    int xin = zf.getXIn();
    int xout = zf.getXOut();
    int yin = zf.getYIn();
    int yout = zf.getYOut();

    XtVaSetValues(_zoomView->getWidget(),
		XvicNxPan, x-(int)(width/(2.0*((float)xin/(float)xout))),
		XvicNyPan, y-(int)(height/(2.0*((float)yin/(float)yout))),
		NULL);

    char buf[16];
    sprintf(buf, "%d", i);
    XmTextFieldSetString(_keyinZoom, buf);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpContrastControl.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpContrastControl.cc: Contrast control.
//////////////////////////////////////////////////////////////////////////////
#include "TpContrastControl.h"
#include "TpWedgeOverlayView.h"
#include "TpContrastValue.h"
#include "Cmd.h"
#include "PrefManager.h"
#include "TpContrastControlPrefView.h"
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/LabelG.h>
#include <Xm/TextF.h>
#include <stdlib.h>
#include <stdio.h>

XtResource TpContrastControl::_resources[] = {
    {
	(char *)"minContrast", 
	(char *)"MinContrast", 
	XmRInt,
	sizeof(int),
	XtOffset(TpContrastControl *, _min),
	XmRImmediate,
	(XtPointer)0,
    },
    {
        (char *)"maxContrast", 
        (char *)"MaxContrast", 
        XmRInt,
        sizeof(int),
        XtOffset(TpContrastControl *, _max),
        XmRImmediate,
        (XtPointer)255,
    },
};

void TpContrastControl::reload(TpContrastControl *copy)
{
    if (_min != copy->_min)
	setLowContrast(copy->_min);
    if (_max != copy->_max)
        setHighContrast(copy->_max);
}

TpContrastControl::TpContrastControl(Widget parent, Cmd *cmd) 
	: CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
    installDestroyHandler();

    getResources(_resources, XtNumber(_resources));

    char path[100];
    sprintf(path, "*%s*", _name);
    thePrefManager->registerResources(this,
				      _resources, XtNumber(_resources),
				      path, new TpContrastControlPrefView());

    XtVaCreateManagedWidget ("contrastFrameLabel",
                xmLabelGadgetClass, _w,
                XmNchildType, XmFRAME_TITLE_CHILD,
                XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
                NULL );

    Widget form = XtVaCreateManagedWidget("form", xmFormWidgetClass, _w, NULL);

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
 
    if (_active)
        activate();
    else
        deactivate();

    // Create text field to keyin low contrast value

    _keyinLowContrast = XtVaCreateManagedWidget("keyinLowContrast", 
				xmTextFieldWidgetClass, form,
				NULL);
    XtAddCallback(_keyinLowContrast, XmNlosingFocusCallback, 
				&TpContrastControl::setLowContrastCallback, 
				(XtPointer)this);
    XtAddCallback(_keyinLowContrast, XmNactivateCallback,
				&TpContrastControl::setLowContrastCallback,
				(XtPointer)this);

    // Create text field to keyin high contrast value

    _keyinHighContrast = XtVaCreateManagedWidget("keyinHighContrast",
                                xmTextFieldWidgetClass, form,
                                NULL);
    XtAddCallback(_keyinHighContrast, XmNlosingFocusCallback,
                                &TpContrastControl::setHighContrastCallback,
                                (XtPointer)this);
    XtAddCallback(_keyinHighContrast, XmNactivateCallback,
                                &TpContrastControl::setHighContrastCallback,
                                (XtPointer)this);

    // Create gray wedge

    _wedge = new TpWedgeOverlayView(form, "wedge", _cmd);

    // Set form attachments

    XtVaSetValues(_keyinLowContrast, 
		  XmNtopAttachment,       XmATTACH_FORM,
		  XmNleftAttachment,      XmATTACH_FORM,
		  XmNrightAttachment,     XmATTACH_NONE,
		  XmNbottomAttachment,    XmATTACH_FORM,
		  NULL);
    XtVaSetValues(_wedge->baseWidget(),
		  XmNtopAttachment,       XmATTACH_FORM,
		  XmNleftAttachment,      XmATTACH_WIDGET,
		  XmNleftWidget,          _keyinLowContrast,
		  XmNrightAttachment,     XmATTACH_NONE,
		  //XmNrightWidget,         _keyinHighContrast,
		  XmNbottomAttachment,    XmATTACH_FORM,
		  NULL);
    XtVaSetValues(_keyinHighContrast,
		  XmNtopAttachment,       XmATTACH_FORM,
		  XmNleftAttachment,      XmATTACH_WIDGET,
		  XmNleftWidget,          _wedge->baseWidget(),
		  XmNrightAttachment,     XmATTACH_NONE,
		  XmNbottomAttachment,    XmATTACH_FORM,
		  NULL);

    _wedge->manage();

    TpContrastValue *value = new TpContrastValue(_min, _max);
    _cmd->execute((CmdValue)value);
}

void TpContrastControl::setLowContrastCallback(Widget w, 
		XtPointer clientData, XtPointer)
{
    int i = atoi(XmTextFieldGetString(w));

    TpContrastControl *obj;
    obj = (TpContrastControl *)clientData;
    if (obj != NULL)
	obj->setLowContrast(i);
}

void TpContrastControl::setHighContrastCallback(Widget w, 
		XtPointer clientData, XtPointer)
{
    int i = atoi(XmTextFieldGetString(w));

    TpContrastControl *obj;
    obj = (TpContrastControl *)clientData;
    if (obj != NULL)
        obj->setHighContrast(i);
}

void TpContrastControl::setLowContrast(int i)
{
    TpContrastValue *value = (TpContrastValue *)(_cmd->getValue());
    value->setMin(i);

    _cmd->execute((CmdValue *)value);
}

void TpContrastControl::setHighContrast(int i)
{
    TpContrastValue *value = (TpContrastValue *)(_cmd->getValue());
    value->setMax(i);
 
    _cmd->execute((CmdValue *)value);
}

void TpContrastControl::setValue(CmdValue v)
{
    TpContrastValue *value = (TpContrastValue *)v;

    _min = value->getMin();
    _max = value->getMax();

    char buf[16];

    sprintf(buf, "%d", _min);
    XmTextFieldSetString(_keyinLowContrast, buf);

    sprintf(buf, "%d", _max);
    XmTextFieldSetString(_keyinHighContrast, buf);

    _wedge->update(value);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpPosView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpPosView.cc
//////////////////////////////////////////////////////////////////////////////
#include "TpPosView.h"
#include "TpCursorModel.h"
#include "KeyinView.h"
#include "ImageData.h"
#include "TpMatchManager.h"
#include "TpSelectionMgr.h"
#include "TpSubDisplayer.h"
#include "TpMatch.h"
#include "TpPointModel.h"
#include "TpQualGroup.h"
#include "TpQualGroupMgr.h"
#include <Xm/RowColumn.h>
#include <stdio.h>

TpPosView::TpPosView(Widget parent, const char *name, 
		     TpCursorModel *cursorModel, ImageData *imageData, 
		     TpMatchManager *mm, TpSubDisplayer *subDisplayer)
    : UIComponent(name)
{
    _cursorModel = cursorModel;
    _imageData = imageData;
    _matchManager = mm;
    _subDisplayer = subDisplayer;

    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent, 
			  XmNorientation, XmHORIZONTAL,
			  XmNpacking, XmPACK_COLUMN,
			  XmNnumColumns, 1,
			  NULL);
    installDestroyHandler();

    _linePosition = new KeyinView(_w, "linePosition");
    _sampPosition = new KeyinView(_w, "sampPosition");
    _qualPosition = new KeyinView(_w, "qualPosition");

    _linePosition->installCallback(&TpPosView::changePosCallback, this);
    _sampPosition->installCallback(&TpPosView::changePosCallback, this);
    _qualPosition->installCallback(&TpPosView::changeQualCallback, this);

    _linePosition->manage();
    _sampPosition->manage();
    _qualPosition->manage();

    startTracking();
}

TpPosView::~TpPosView()
{
    XtRemoveCallback(_cursorModel->getImageWidget(),
		     XvicNcursorCallback,
		     &TpPosView::cursorMovedCallback,
		     (XtPointer)this);
}

Boolean TpPosView::isCursorOnImage(XvicImageCallbackStruct *cb)
{
    return(cb->on_screen &&
	   cb->x >= 0 && cb->x < _imageData->getNumbSamples() &&
	   cb->y >= 0 && cb->y < _imageData->getNumbLines());
}

void TpPosView::cursorIsOnImage(Boolean onImage)
{
    _isOnImage = onImage;
}

void TpPosView::startTracking()
{
    // Declare callbacks on the main image

    XtAddCallback(_cursorModel->getImageWidget(),
		  XvicNcursorCallback,
		  &TpPosView::cursorMovedCallback,
		  (XtPointer)this);

    // Declare callbacks on additional images, in our case zoomed image

    Widget *aiw;
    int n;
    _cursorModel->getImageWidgets(aiw, n);
    for (int i = 0; i < n; i++)
	XtAddCallback(aiw[i],
		      XvicNcursorCallback,
		      &TpPosView::cursorMovedCallback,
		      (XtPointer)this);
}

void TpPosView::stopTracking()
{
    // Remove callbacks on the main image

    XtRemoveCallback(_cursorModel->getImageWidget(),
		     XvicNcursorCallback,
		     &TpPosView::cursorMovedCallback,
		     (XtPointer)this);

    // Remove callbacks on additional images, in our case zoomed image

    Widget *aiw;
    int n;
    _cursorModel->getImageWidgets(aiw, n);
    for (int i = 0; i < n; i++)
	XtRemoveCallback(aiw[i],
			 XvicNcursorCallback,
			 &TpPosView::cursorMovedCallback,
			 (XtPointer)this);
}

void TpPosView::cursorMovedCallback(Widget,
                        XtPointer client_data, XtPointer call_data)
{
    TpPosView *obj = (TpPosView *)client_data;
    XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)call_data;
 
    if (obj->isCursorOnImage(cb) == False)
        obj->cursorIsOnImage(FALSE);
    else
        obj->cursorIsOnImage(TRUE);
 
    obj->cursorMoved(cb);
}

// Update the cursor position only if no points are selected in the 
// current image
void TpPosView::cursorMoved(XvicImageCallbackStruct * cb)
{
    TpSelectionMgr *selectionMgr;
    selectionMgr = _matchManager->getSelectionMgr();
    
    if (!selectionMgr->isEmpty()) {
	SL_ListWatch<TpPointModel *> watch;
	selectionMgr->initScan(&watch);
	TpPointModel *apoint;
	while ((apoint = selectionMgr->nextPoint()) != NULL) {
	    if (apoint->getImageData() == _imageData) {
		cursorMoved(apoint->getX(), apoint->getY());
		return;
	    }
	}
    }
    
    // Note that the program never gets to this point 
    // if current point was found on this image

    if (!_isOnImage)
	blankDisplay();
    else {
	double x, y;
	_imageData->transDisplayToImageCoords(cb->x_fp, cb->y_fp, &x, &y);
	cursorMoved(x, y);
    }
}

void TpPosView::cursorMoved(int x, int y)
{
    cursorMoved((double)x, (double)y);
}

void TpPosView::cursorMoved(double x, double y)
{
    char buf[132];

    sprintf(buf, "%5.3f", x);
    _sampPosition->setFieldValue(buf);

    sprintf(buf, "%5.3f", y);
    _linePosition->setFieldValue(buf);
}

void TpPosView::blankDisplay()
{
    _sampPosition->setFieldValue((char *)"");
    _linePosition->setFieldValue((char *)"");
    _qualPosition->setFieldValue((char *)"");
}

void TpPosView::changePosCallback(Widget, XtPointer clientData,
				  XtPointer)
{
    TpPosView *obj = (TpPosView *)clientData;
    obj->changePos();
}

void TpPosView::changePos()
{
    char *samp = _sampPosition->getFieldValue();
    char *line = _linePosition->getFieldValue();

    _matchManager->processNewPoint(atof(samp), atof(line), _subDisplayer);
	
}

void TpPosView::changeQualCallback(Widget, XtPointer clientData,
                                   XtPointer)
{
    TpPosView *obj = (TpPosView *)clientData;
    obj->changeQual();
}

void TpPosView::changeQual()
{
    char *valueString = _qualPosition->getFieldValue();
    
    // Get selected match

    TpSelectionMgr *selMgr = _matchManager->getSelectionMgr();
    TpMatch *match = selMgr->getMatch();
    if (match == NULL) {
	_qualPosition->setFieldValue((char *)"");
	return;
    }

    // Find affected point and get its qualifier model

    SL_ListWatch<TpPointModel *> watch;
    match->initScan(&watch);
    TpPointModel *apoint;
    while ((apoint = match->next()) != NULL) {
	if (_subDisplayer->getImageData() == apoint->getImageData()) {
	    match->scanDone();
	    break;
	}
    }

    TpQualGroup *pointQual = apoint->getPointQual();

    if (pointQual->getNumQuals() == 0) {
        TpQualType type = TpQualGroup::getValueType(valueString);
        _matchManager->getPointQualMgr()->incNumQuals(type);
    }
 
    pointQual->setValue(0, valueString);

    apoint->updateViews();
}

/////////////////////////////////////////////////////////////////////////////
// This function should be called to refresh displayed values
/////////////////////////////////////////////////////////////////////////////
void TpPosView::displayValues()
{
    TpSelectionMgr *selMgr = _matchManager->getSelectionMgr();
    TpMatch *match = selMgr->getMatch();
    if (match == NULL) {
        _qualPosition->setFieldValue((char *)"");
        return;
    }
 
    SL_ListWatch<TpPointModel *> watch;
    match->initScan(&watch);
    TpPointModel *apoint;
    while ((apoint = match->next()) != NULL) {
        if (_subDisplayer->getImageData() == apoint->getImageData()) {
            match->scanDone();
            break;
        }
    }

    TpQualGroup *pointQual = apoint->getPointQual();
 
    if (pointQual->getNumQuals() > 0) {
	char *buf = pointQual->valueToString(0);
	_qualPosition->setFieldValue(buf);
	delete [] buf;
    }
    else {
	_qualPosition->setFieldValue((char *)"");
    }

    char buf1[16];
    sprintf(buf1, "%.3f", apoint->getX());
    _sampPosition->setFieldValue(buf1);
    sprintf(buf1, "%.3f", apoint->getY());
    _linePosition->setFieldValue(buf1);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpCursorModel.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// TpCursorModel.cc: Same as CursorModel, only it sets tracking
// on not just one, but on an array of n imaging widgets.
////////////////////////////////////////////////////////////////
#include "TpCursorModel.h"
#include "CursorBasicView.h"
#include "XvicImage.h"

TpCursorModel::TpCursorModel(Boolean trackingEnabled, 
		Widget iw, Widget *aiw, int n) 
	: CursorModel(trackingEnabled, iw)
{
    _aiw = aiw;
    _n = n;

    // SET RESOURCE IN WIDGET TO TRACK 
    for (int i = 0; i < _n; i++)
	XtVaSetValues(_aiw[i], XvicNtrackFloatingCursor, True, NULL);
}

TpCursorModel::~TpCursorModel()
{
    for (int i = 0; i < _n; i++)
	XtVaSetValues(_aiw[i], XvicNtrackFloatingCursor, False, NULL);
}

void TpCursorModel::getImageWidgets(Widget *&aiw, int &n)
{
    n = _n;
    aiw = new Widget [_n];
    for (int i = 0; i < _n; i++)
	aiw[i] = _aiw[i];
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpImageInfo.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// TpImageInfo.h: This component displays image filename and
// image number.
///////////////////////////////////////////////////////////////////
#include "TpImageInfo.h"
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <stdio.h>

TpImageInfo::TpImageInfo(Widget parent, const char *name, 
		char *filename, int number)
	: UIComponent(name)
{
    _w = XtVaCreateWidget(_name, xmFormWidgetClass, parent, 
			  NULL);
    installDestroyHandler();

    char buf[16];
    sprintf(buf, "Image %d", number);
    XmString numStr = XmStringCreateLocalized (buf);
 
    _imageNumber = XtVaCreateManagedWidget("imageNumber",
                            xmLabelWidgetClass, _w,
                            XmNlabelString, numStr,
                            XmNtopAttachment, XmATTACH_FORM,
                            XmNleftAttachment, XmATTACH_FORM,
                            XmNrightAttachment, XmATTACH_NONE,
                            XmNbottomAttachment, XmATTACH_FORM,
                            NULL);
    
    XmString fileStr = XmStringCreateLocalized(filename);
    XtVaCreateManagedWidget("filename",
                            xmLabelWidgetClass, _w,
                            XmNlabelString, fileStr,
                            XmNalignment, XmALIGNMENT_END,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, _imageNumber, 
			    XmNleftOffset, 10,
			    XmNrightAttachment, XmATTACH_FORM,
			    XmNbottomAttachment, XmATTACH_FORM,
                            NULL);
}

void TpImageInfo::setNumber(int n) 
{
    char buf[16];
    sprintf(buf, "Image %d", n);
    XmString numStr = XmStringCreateLocalized (buf);

    XtVaSetValues(_imageNumber, XmNlabelString, numStr, NULL);

    XmStringFree(numStr);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpImageReference.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// TpImageReference
///////////////////////////////////////////////////////
#include "TpImageReference.h"
#include "ButtonInterface.h"
#include "TpDefs.h"
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <stdio.h>

TpImageReference::TpImageReference(Widget parent, const char *name)
	: UIComponent(name)
{
    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent,
		XmNorientation, XmVERTICAL,
		XmNnumColumns, TP_MAX_IMAGES,
		XmNpacking, XmPACK_COLUMN,
		XmNspacing, 1,
		NULL);
    installDestroyHandler();

    _fgLoaded = sdup("blue");
    _bgVisible = sdup("maroon");
    XtVaGetValues(_w,
                  XmNforeground, &_fg,
                  XmNbackground, &_bg,
                  NULL);

    _labels1 = new Widget [TP_MAX_IMAGES];
    _labels2 = new Widget [TP_MAX_IMAGES];

    for (int i = 0; i < TP_MAX_IMAGES; i++) {
	char buf[3];
	sprintf(buf, "%d", i+1);
//	_labels1[i] = XtVaCreateManagedWidget(buf, 
	_labels1[i] = XtVaCreateWidget(buf, 
			xmLabelWidgetClass, _w, 
			NULL);
//	_labels2[i] = XtVaCreateManagedWidget(buf,
//			xmLabelWidgetClass, _w,
//			NULL);
    }
}

TpImageReference::~TpImageReference()
{
    // Empty
}

// Visible, Invisible, Referenced

void TpImageReference::setAllToInvisible()
{
    for (int i = 0; i < TP_MAX_IMAGES; i++)
	setToInvisible(_labels1[i]);
}

void TpImageReference::setToVisible(int i)
{
    setToVisible(_labels1[i]);
}

void TpImageReference::setReferencedImage(int)
{
    // setTo
}

void TpImageReference::setToVisible(Widget w)
{
    XtVaSetValues(w, XtVaTypedArg, 
		  XmNbackground, XmRString,
		  _bgVisible, (strlen(_bgVisible) + 1),
		  NULL);
}

void TpImageReference::setToInvisible(Widget w)
{
    XtVaSetValues(w, XmNbackground, _bg, NULL);
}

void TpImageReference::indicateLoadedImage(int i)
{
    indicateLoadedImage(_labels1[i]);
}

void TpImageReference::indicateLoadedImage(Widget w)
{
    XtVaSetValues(w, XtVaTypedArg, 
		  XmNforeground, XmRString, 
		  "green", (strlen("green") + 1),
		  NULL);
    XtVaGetValues(_w,
                  XmNforeground, &_fg,
                  XmNbackground, &_bg,
                  NULL);
    XtManageChild(w);

}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpSelectionMgr.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////
// TpSelectionMgr.cc:  Manages points and matches selected by 
// user.  At the core of this class are two elements.  First, a 
// match that is currently being selected.  Because only some 
// points from the match could be selected, it is necessary also 
// to keep a list of points from the match that have been selected.
// A list of points stored by selection manager is a subset of 
// points in the selected match.  
///////////////////////////////////////////////////////////////
#include "TpSelectionMgr.h"
#include "TpMatchManager.h"
#include "TpMatch.h"
#include "TpPointModel.h"

TpSelectionMgr::TpSelectionMgr(TpMatchManager *mm)
{
    _matchManager = mm;
    _match = NULL;
    _points = new SL_List<TpPointModel *>;
}

TpSelectionMgr::~TpSelectionMgr()
{
    // Empty
}

///////////////////////////////////////////////////////////////////
// Add match, no points
///////////////////////////////////////////////////////////////////
void TpSelectionMgr::addMatch(TpMatch *match)
{
    // Save match

    _match = match;

    _matchManager->newMatchSelected();
}

///////////////////////////////////////////////////////////////////
// Add match and all its points.
///////////////////////////////////////////////////////////////////
void TpSelectionMgr::selectMatchAndPoints(TpMatch *match)
{
    if (_match != NULL) {
	if (!_matchManager->isExistingMatch(_match)) {
	    delete _match;
	    _points->clear();
	    _match = NULL;
        }
        else {
	    clear();
        }
    }

    if (!match) return;

    // Save match

    addMatch(match);

    // Select all points from selected match

    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *apoint;

    _match->initScan(&watch);
    while ((apoint = _match->next()) != NULL) {
	selectPoint(apoint);
    }
}

///////////////////////////////////////////////////////////////////
// Add point to the match and to selection manager.
///////////////////////////////////////////////////////////////////
void TpSelectionMgr::addPoint(TpPointModel *p)
{
    _match->addPoint(p);
    selectPoint(p);
}

///////////////////////////////////////////////////////////////////
// Select point, don't add it to the match, it's already in the 
// match.
///////////////////////////////////////////////////////////////////
void TpSelectionMgr::selectPoint(TpPointModel *p)
{
    p->setCurrent(True);
    _points->add_replace(p);
}

///////////////////////////////////////////////////////////////////
// Delete point
///////////////////////////////////////////////////////////////////
void TpSelectionMgr::deletePoint(TpPointModel *p)
{
    p->setCurrent(False);

    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *toDelete;

    _points->init_scan(&watch);
    while ((toDelete = _points->next()) != NULL) {
	if (toDelete == p)
	    _points->remove_current();
    }

    // Clear match if this was the last point
    if (_points->get_length() == 0)
	addMatch(NULL);
}

///////////////////////////////////////////////////////////////////
// Clear list of points and match.
///////////////////////////////////////////////////////////////////
void TpSelectionMgr::clear()
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *toDelete;

    _points->init_scan(&watch);
    while ((toDelete = _points->next()) != NULL) {
	toDelete->setCurrent(False);
	_points->remove_current();
    }

    addMatch(NULL);
}

///////////////////////////////////////////////////////////////////
// The following functions help other classes to interact with 
// the list of points.  They merely imitate the functions of list 
// template class and allow to protect the list itself.
///////////////////////////////////////////////////////////////////
int TpSelectionMgr::initScan(SL_ListWatch<TpPointModel *> *watch)
{
    int status;
    status = _points->init_scan(watch);
    // failed = _points->failed; 
    return status;
}

TpPointModel *TpSelectionMgr::nextPoint()
{
    TpPointModel *p;
    p = _points->next();
    // failed = _points->failed;
    return p;
}

int TpSelectionMgr::scanDone()
{
    int status;
    status = _points->scan_done();
    // failed = _points->failed;
    return status;
}

//////////////////////////////////////////////////////////
// Check if all points in match are selected, or just a 
// subset.
//////////////////////////////////////////////////////////
Boolean TpSelectionMgr::isFullHouse()
{
    if (_points->get_length() == _match->getNumPoints())
	return True;
    else
	return False;
}

//////////////////////////////////////////////////////////
// listPoints: Output info about each point in the match, mostly 
// for testing purposes.  This function should probably be removed 
// or modified for the final delivery.
//////////////////////////////////////////////////////////
void TpSelectionMgr::listPoints()
{
    SL_ListWatch<TpPointModel *> watch;
    TpPointModel *point;

    initScan(&watch);
    while ((point = _points->next()) != NULL) {
	point->listPoint();
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpWedgeOverlayView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpWedgeOverlayView.h: Provides overlay graphics support for wedge class
// The class allows to manupulate two marks that are used by contrasting 
// tool in TP.
/////////////////////////////////////////////////////////////////////////////
#include "TpWedgeOverlayView.h"
#include "TpContrastValue.h"
#include "WedgeView.h"
#include "Cmd.h"
#include <Xm/Frame.h>
#include <stdlib.h>

XtResource TpWedgeOverlayView::_resources [ ] = {
 {
    (char *)"markColor",
    (char *)"MarkColor",
    XmRString,
    sizeof ( String ),
    XtOffset ( TpWedgeOverlayView *, _markColor ),
    XmRImmediate,
    ( XtPointer ) "red",
 },
 {
    (char *)"markLength",
    (char *)"MarkLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( TpWedgeOverlayView *, _markLength ),
    XmRImmediate,
    ( XtPointer ) 20,
 },
  {
    (char *)"markThickness",
    (char *)"MarkThickness",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( TpWedgeOverlayView *, _markThickness ),
    XmRImmediate,
    ( XtPointer ) 6,
 },
};

String TpWedgeOverlayView::_defaults[] = {
    (char *)"*wedgeView.scrollBarDisplayPolicy: NEVER",
    (char *)"*wedgeView.shadowThickness:        0", 
    (char *)"*wedgeView.highlightThickness:     0", 
    (char *)"*wedgeView.orientation:            HORIZONTAL", 
    (char *)"*wedgeView.nsteps:                 256",
    (char *)"*wedgeView.viewWidth:              256",
    (char *)"*wedgeView.imageWidth:             256", 
    NULL,
};

TpWedgeOverlayView::TpWedgeOverlayView(Widget parent, const char *name, Cmd *cmd)
    : UIComponent(name)
{
    _cmd = cmd;

    setDefaultResources(parent, _defaults);

    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
    installDestroyHandler();

    getResources(_resources, XtNumber(_resources));
    
    WedgeView *wedge = new WedgeView(_w, "wedgeView");
    wedge->manage();
    _iw = wedge->getWidget();
    
    XtVaSetValues(_iw,
		  XvicNlutType, XvicRAW,
		  NULL);
    
    // Set color and GC for graphics overlay

    XColor xcolor;
    XParseColor(XtDisplay(_iw), DefaultColormapOfScreen(XtScreen(_iw)),
                _markColor, &xcolor);
    _lineColor = XvicImageGetGrColor(_iw, &xcolor);
    
    // Set GC for graphics overlay

    XGCValues values;
    values.line_width = _markThickness;
    _lineGC=XvicImageCreateGC(_iw, GCLineWidth, &values);

    // Declare input callback

    XtAddCallback(_iw, 
		  XvicNinputCallback, 
		  &TpWedgeOverlayView::inputCallback, 
		  (XtPointer)this);
}

void TpWedgeOverlayView::inputCallback(Widget, XtPointer clientData, 
				       XtPointer callData)
{
    TpWedgeOverlayView *obj = (TpWedgeOverlayView *)clientData;
    obj->input(callData);
}

void TpWedgeOverlayView::input(XtPointer callData)
{
    XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)callData;
 
    if (cb->reason != XvicCR_INPUT)
	return;                           // oops
    
    if (cb->input_num_params != 2)
	return;                           // oops
    
    TpContrastValue *value = new TpContrastValue 
	(*(TpContrastValue *)(_cmd->getValue()));
    int distMin = abs(value->getMin() - cb->x);
    int distMax = abs(value->getMax() - cb->x);
    if (distMin < distMax)
	value->setMin(cb->x);
    else
	value->setMax(cb->x);

    _cmd->execute(value);
}

///////////////////////////////////////////////////////////////
// update 
///////////////////////////////////////////////////////////////
void TpWedgeOverlayView::update(TpContrastValue *value)
{
    XvicImageEraseOverlay(_iw);

    _min = value->getMin();
    _max = value->getMax();

    XvicPoint points[5];
    points[0].x = _min; 
    points[0].y = _markLength;

    points[1].x = -1 * _markThickness;
    points[1].y = -1 * _markLength / 2;
    
    points[2].x = 0;
    points[2].y = -1 * _markLength / 2;

    points[3].x = 2 * _markThickness;
    points[3].y = 0;

    points[4].x = 0;
    points[4].y = _markLength / 2;

    XvicImageFillPolygon(_iw, 0, _lineGC, _lineColor,
			 points, 5, 
			 Complex, CoordModePrevious);

    points[0].x = _max;
    points[0].y = _markLength;
 
    points[1].x = -1 * _markThickness;
    points[1].y = -1 * _markLength;
 
    points[2].x = 2 * _markThickness;
    points[2].y = 0;
 
    XvicImageFillPolygon(_iw, 0, _lineGC, _lineColor,
                         points, 3,
                         Complex, CoordModePrevious);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchBrowseControl.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// TpMatchBrowseControl.cc: This component displays general qualifier 
// information and allows user to change it.  It also allows user to 
// select tiepoints by typing the point sequence number.
///////////////////////////////////////////////////////////////////
#include "TpMatchBrowseControl.h"
#include "TpSelectionMgr.h"
#include "TpQualGroup.h"
#include "TpQualGroupMgr.h"
#include "TpMatchManager.h"
#include "QuestionDialogManager.h"
#include "Application.h"
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/ArrowB.h>
#include <Xm/Label.h>
#include <assert.h>
#include <stdio.h>

XtResource TpMatchBrowseControl::_resources[] = {
  {
    (char *)"enableGotoQual",
    (char *)"EnableGotoQual",
    XmRBoolean,
    sizeof(Boolean),
    XtOffsetOf(TpMatchBrowseControl, _enableGotoQual),
    XmRImmediate,
    (XtPointer) False,
  },
};

String TpMatchBrowseControl::_defaults[] = {
    (char *)"*number.columns: 3",
    (char *)"*value.columns: 21", 
    (char *)"*value.maxLength: 255",
    (char *)"*prev.arrowDirection: ARROW_DOWN",
    (char *)"*next.arrowDirection: ARROW_UP",
    NULL,
};

int _displayed = 0; //!!!

TpMatchBrowseControl::TpMatchBrowseControl(Widget parent, const char *name, 
			TpSelectionMgr *sm, TpMatchManager *mm)
    : UIComponent(name)
{
    _selMgr = sm;
    _matchManager = mm;
    _numberInt = 0;

    setDefaultResources(parent, _defaults);

    XtGetApplicationResources(theApplication->baseWidget(), (XtPointer)this,
			_resources, XtNumber(_resources), NULL, 0);

    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent,
                          XmNshadowThickness, 3,
                          NULL);
    installDestroyHandler();

    Widget rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, _w,
					XmNorientation, XmHORIZONTAL,
					NULL);

    _numberField = XtVaCreateManagedWidget("number", 
					   xmTextFieldWidgetClass, rc, 
					   NULL);
    _valueField = XtVaCreateManagedWidget("value",
					  xmTextFieldWidgetClass, rc, 
					  NULL);

    // This field shows the qualifier like _valueField does, but when typed
    // in, instead of changing the value it selects ("goes to") the first
    // tiepoint found with that qualifier.  Or does nothing if not found.

    if (_enableGotoQual) {
	_gotoLabel = XtVaCreateManagedWidget("gotoLabel",
					xmLabelWidgetClass, rc, NULL);
	_gotoField = XtVaCreateManagedWidget("gotoField",
					xmTextFieldWidgetClass, rc, NULL);
    }

    Widget prev = XtVaCreateManagedWidget("prev", 
					  xmArrowButtonWidgetClass, rc,
					  NULL);
    Widget next = XtVaCreateManagedWidget("next", 
                                          xmArrowButtonWidgetClass, rc,
                                          NULL);

    // Declare all necessary callbacks

    XtAddCallback(_numberField, XmNactivateCallback,
                  &TpMatchBrowseControl::setNumberCallback, 
		  (XtPointer)this);

    XtAddCallback(_valueField, XmNactivateCallback, 
                  &TpMatchBrowseControl::setValueCallback, 
		  (XtPointer)this);

    if (_enableGotoQual) {
	XtAddCallback (_gotoField, XmNactivateCallback,
		  &TpMatchBrowseControl::gotoQualCallback,
		  (XtPointer)this);
    }

    XtAddCallback(prev, XmNactivateCallback, 
		  &TpMatchBrowseControl::decNumberCallback, 
		  (XtPointer)this);
    XtAddCallback(next, XmNactivateCallback, 
                  &TpMatchBrowseControl::incNumberCallback, 
		  (XtPointer)this);
}

void TpMatchBrowseControl::setNumberCallback(Widget, XtPointer clientData,
					     XtPointer)
{
    TpMatchBrowseControl *obj = (TpMatchBrowseControl *)clientData;
    obj->setNumber();
}

void TpMatchBrowseControl::setValueCallback(Widget, XtPointer clientData,
					    XtPointer)
{
    TpMatchBrowseControl *obj = (TpMatchBrowseControl *)clientData;
    obj->setValue(False);
}

void TpMatchBrowseControl::gotoQualCallback(Widget, XtPointer clientData,
					    XtPointer)
{
    TpMatchBrowseControl *obj = (TpMatchBrowseControl *)clientData;
    obj->gotoQual();
}

void TpMatchBrowseControl::confirm(void *clientData)
{
    TpMatchBrowseControl *obj = (TpMatchBrowseControl *)clientData;
    obj->setValue(True);
}

void TpMatchBrowseControl::incNumberCallback(Widget, XtPointer clientData,
					     XtPointer)
{
    TpMatchBrowseControl *obj = (TpMatchBrowseControl *)clientData;
    obj->incNumber();
}

void TpMatchBrowseControl::decNumberCallback(Widget, XtPointer clientData,
					     XtPointer)
{
    TpMatchBrowseControl *obj = (TpMatchBrowseControl *)clientData;
    obj->decNumber();
}

/////////////////////////////////////////////////////////////////////////////
// Execute this on user typing a new running id
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::setNumber()
{
    // Get the desired match number provided by the user

    char *numberString = XmTextFieldGetString(_numberField);
    _numberInt = atoi(numberString);

    setNumber(_numberInt);
}

/////////////////////////////////////////////////////////////////////////////
// Given the new running id, check it first, then select new tiepoint and
// display correspoding general qualifier value.
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::setNumber(int num)
{
    _numberInt = num;
    if (_matchManager->numMatches() == 0)
	_numberInt = 0;

    setNumberField((char *)"");

    if (_numberInt > _matchManager->numMatches())
        _numberInt = _matchManager->numMatches();
    else if ((_numberInt <= 0) && (_matchManager->numMatches() > 0))
        _numberInt = 1;
    
    TpMatch *match = _matchManager->getNthMatch(_numberInt);
    if (match)
	_selMgr->selectMatchAndPoints(match);
    
    char buf[16];
    sprintf(buf, "%d", _numberInt);
    setNumberField(buf);

    displayValue();
}

/////////////////////////////////////////////////////////////////////////////
// Execute this on user typing into the Goto Qual box
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::gotoQual()
{
    // Get the desired qual value provided by the user

    char *gotoString = XmTextFieldGetString(_gotoField);

    gotoQual(gotoString);
}

/////////////////////////////////////////////////////////////////////////////
// Given a general qualifier value, find the first tiepoint that has that
// qual value and select the corresponding tiepoint.
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::gotoQual(char *gotoString)
{

    // We could do this in a slicker way by using a matching function on
    // the SL_List to find the appropriate match.  But in the interests of
    // time, this simple brute-force method should work...  (rgd 6/2010)

    int nmatch = _matchManager->numMatches();

    for (int i=1; i <= nmatch; i++) {
	TpMatch *match = _matchManager->getNthMatch(i);
	if (match == NULL) continue;

        TpQualGroup *genQual = match->getGenQual();
	if (genQual == NULL) continue;

        if (genQual->getNumQuals() > _displayed) {
	    char *buf = genQual->valueToString(_displayed);
	    if (strcmp(buf, gotoString) == 0) {		// found it!
		setNumber(i);
		if (buf) delete [] buf;
		return;
	    }
	    if (buf) delete [] buf;
	}
    }

    // It wasn't found, so issue an error... such as it is

    if (_enableGotoQual)
	XmTextFieldSetString(_gotoField, (char *)"UNK");
}

/////////////////////////////////////////////////////////////////////////////
// Find out general qualifier value and show it.
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::displayValue()
{
    // Get currently-selected match

    TpMatch *match = _selMgr->getMatch();
    if (match == NULL) {
	setValueField(NULL);
	return;
    }

    TpQualGroup *genQual = match->getGenQual();

    char *buf = NULL;
    if (genQual->getNumQuals() > _displayed) {
	buf = genQual->valueToString(_displayed);
    }
    else {
	buf = sdup("No qualifier defined");
    }
    setValueField(buf);
    if (buf) delete [] buf;
}

/////////////////////////////////////////////////////////////////////////////
// Execute this on user typing a new qualifier value.  Note that only one 
// general qualifier is supported right now, although TpQualGroup class
// has support for multiple qualifiers.  That is why setGenQual() function 
// always gets 1 as a second argument right now.  This should be changed 
// to the qualifier number when it will be possible to set more than one 
// qualifier. 
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::setValue(Boolean confirmed)
{
    char *valueString = XmTextFieldGetString(_valueField);

    TpMatch *match = _selMgr->getMatch();
    if (match == NULL) {
	setValueField(NULL);
	return;
    }

    TpQualGroup *genQual = match->getGenQual();

    if (genQual->getNumQuals() <= _displayed) {
	setValueField((char *)"No qualifier defined");
	return;
    }

    if (_matchManager->isCheckingGenQualUnique() && !confirmed) {
	if (!_matchManager->getGenQualMgr()->isUnique(genQual, _displayed, 
						      valueString)) {
	    char msg[] ="You are entering non-unique qualifier. Are you sure?";
	    theQuestionDialogManager->post(msg, (void *)this,
					   &TpMatchBrowseControl::confirm);
	    return;
	}
    }
    genQual->setValue(_displayed, valueString);
}

/////////////////////////////////////////////////////////////////////////////
// Execute this on user pressing increment by one button
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::incNumber()
{
    setNumber(_numberInt+1);
}

/////////////////////////////////////////////////////////////////////////////
// Execute this on user pressing decrement by one button
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::decNumber()
{
    setNumber(_numberInt-1);
}

/////////////////////////////////////////////////////////////////////////////
// Just a convenience function to set the running id text field.
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::setNumberField(char *text)
{
    if (text && strlen(text))
	XmTextFieldSetString(_numberField, text);
    else
	XmTextFieldSetString(_numberField, (char *)"");
}

/////////////////////////////////////////////////////////////////////////////
// Just a convenience function to set the qualifier value text field.
// Also sets the same text in the Goto field, if enabled.
/////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::setValueField(char *text)
{
    if (text) {
	XmTextFieldSetString(_valueField, text);
	if (_enableGotoQual)
	    XmTextFieldSetString(_gotoField, text);
    }
    else {
	XmTextFieldSetString(_valueField, (char *)"No Tiepoints Selected");
	if (_enableGotoQual)
	    XmTextFieldSetString(_gotoField, (char *)"");
    }
}

////////////////////////////////////////////////////////////////////////////
// This function gets called every time the user made a selection of a 
// different match.
////////////////////////////////////////////////////////////////////////////
void TpMatchBrowseControl::newMatchSelected(TpMatch *match)
{
    if (match == NULL) {
	setNumberField(NULL);
	setValueField(NULL);
	return;
    }

    _numberInt = _matchManager->getMatchNumber(match);
    char buf[16];
    sprintf(buf, "%d", _numberInt);
    setNumberField(buf);

    displayValue();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpAutofindRes.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// TpAutofindResults
///////////////////////////////////////////////////////
#include "TpAutofindRes.h"
#include "TpMatchManager.h"
#include "KeyinView.h"
#include <Xm/RowColumn.h>
#include <stdio.h>

TpAutofindResults::TpAutofindResults(Widget parent, const char *name, 
				     TpMatchManager *mm)
	: UIComponent(name)
{
    _matchManager = mm;

    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent,
		XmNorientation, XmVERTICAL,
		XmNnumColumns, 2,
		XmNpacking, XmPACK_COLUMN,
		NULL);
    installDestroyHandler();

    for (int i = 0; i < 6; i++) {
	char buf[16];
	sprintf(buf, "affPar%d", i);
	_labels[i] = new KeyinView(_w, buf);
	_labels[i]->manage();
    }
}

TpAutofindResults::~TpAutofindResults()
{
    // Empty
}

void TpAutofindResults::setValues(double a[6])
{
    for (int i = 0; i < 6; i++) {
	char buf[16];
	sprintf(buf, "%f", a[i]);
	_labels[i]->setFieldValue(buf);
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchModeRes.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// TpMatchModeResults
///////////////////////////////////////////////////////
#include "TpMatchModeRes.h"
#include "TpMatchManager.h"
#include "KeyinView.h"
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <stdio.h>

TpMatchModeResults::TpMatchModeResults(Widget parent, const char *name, 
				       TpMatchManager *mm)
	: UIComponent(name)
{
    _matchManager = mm;

    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent,
		XmNorientation, XmVERTICAL,
		XmNnumColumns, 1,
		XmNpacking, XmPACK_COLUMN,
		NULL);
    installDestroyHandler();

    for (int i = 0; i < 5; i++) {
	char buf[8];
	sprintf(buf, "erg%d", i);
	_labels[i] = new KeyinView(_w, buf);
	_labels[i]->manage();
    }

    // This is cheating... should use a CheckBoxInterface.  But we just check
    // this for printing, we don't really execute a command, and it's easier
    // this way...

    _dumpStdout = XtVaCreateManagedWidget("dump stdout",
			xmToggleButtonWidgetClass, _w,
			XmNindicatorType, XmN_OF_MANY,
			XmNset, False,
			NULL);

}

TpMatchModeResults::~TpMatchModeResults()
{
    // Empty
}

void TpMatchModeResults::setValues(float a[5])
{
    for (int i = 0; i < 5; i++) {
	char buf[32];
	sprintf(buf, "%f", a[i]);
	_labels[i]->setFieldValue(buf);
    }
}

Boolean TpMatchModeResults::isDumpToStdout()
{
    if (XmToggleButtonGetState(_dumpStdout))
	return True;
    return False;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpMatchModeValuesCmdIf.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpSetMatchModeValuesCmdIf.cc:  
//////////////////////////////////////////////////////////////////////////////
#include "TpMatchModeValuesCmdIf.h"
#include "TpMatchModeValues.h"
#include "KeyinView.h"
#include "Cmd.h"
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <stdlib.h>
#include <stdio.h>

TpSetMatchModeValuesCmdIf::TpSetMatchModeValuesCmdIf(Widget parent, Cmd *cmd) 
    : CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
    installDestroyHandler();

    XtVaCreateManagedWidget("matchModeValuesFrameLabel",
                            xmLabelGadgetClass, _w,
                            XmNchildType, XmFRAME_TITLE_CHILD,
                            XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
                            NULL);

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
 
    if (_active)
        activate();
    else
        deactivate();

    Widget rc = XtVaCreateManagedWidget("matchModeValuesRC", 
					xmRowColumnWidgetClass, _w, 
					NULL);

    _pmk = new KeyinView(rc, "pmk");
    _lsm = new KeyinView(rc, "lsm");
    _sw = new KeyinView(rc, "sw");
    _accuracy = new KeyinView(rc, "accuracy");
    _threshold = new KeyinView(rc, "threshold");

    _pmk->installCallback(&TpSetMatchModeValuesCmdIf::setPMKCallback, 
			  (XtPointer)this);
    _pmk->manage();

    _lsm->installCallback(&TpSetMatchModeValuesCmdIf::setLSMCallback,
                          (XtPointer)this);
    _lsm->manage();

    _sw->installCallback(&TpSetMatchModeValuesCmdIf::setSWCallback,
			 (XtPointer)this);
    _sw->manage();

    _accuracy->installCallback(&TpSetMatchModeValuesCmdIf::setAccuracyCallback,
			       (XtPointer)this);
    _accuracy->manage();

    _threshold->installCallback(
	&TpSetMatchModeValuesCmdIf::setThresholdCallback,
	(XtPointer)this);
    _threshold->manage();

    setValue(_cmd->getValue());
}

void TpSetMatchModeValuesCmdIf::setPMKCallback(Widget, 
		XtPointer clientData, XtPointer)
{
    TpSetMatchModeValuesCmdIf *obj;
    obj = (TpSetMatchModeValuesCmdIf *)clientData;
    if (obj != NULL)
        obj->setPMK();
}

void TpSetMatchModeValuesCmdIf::setLSMCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpSetMatchModeValuesCmdIf *obj;
    obj = (TpSetMatchModeValuesCmdIf *)clientData;
    if (obj != NULL)
        obj->setLSM();
}

void TpSetMatchModeValuesCmdIf::setSWCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpSetMatchModeValuesCmdIf *obj;
    obj = (TpSetMatchModeValuesCmdIf *)clientData;
    if (obj != NULL)
        obj->setSW();
}

void TpSetMatchModeValuesCmdIf::setAccuracyCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpSetMatchModeValuesCmdIf *obj;
    obj = (TpSetMatchModeValuesCmdIf *)clientData;
    if (obj != NULL)
        obj->setAccuracy();
}

void TpSetMatchModeValuesCmdIf::setThresholdCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpSetMatchModeValuesCmdIf *obj;
    obj = (TpSetMatchModeValuesCmdIf *)clientData;
    if (obj != NULL)
        obj->setThreshold();
}

void TpSetMatchModeValuesCmdIf::setPMK()
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_cmd->getValue();
    value->_pmk = atoi(_pmk->getFieldValue());

    runCmd(value);
}

void TpSetMatchModeValuesCmdIf::setLSM()
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_cmd->getValue();
    value->_lsm = atoi(_lsm->getFieldValue());

    runCmd(value);
}

void TpSetMatchModeValuesCmdIf::setSW()
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_cmd->getValue();
    value->_searchWindow = atoi(_sw->getFieldValue());

    runCmd(value);
}

void TpSetMatchModeValuesCmdIf::setAccuracy()
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_cmd->getValue();
    value->_accuracy = atof(_accuracy->getFieldValue());

    runCmd(value);
}

void TpSetMatchModeValuesCmdIf::setThreshold()
{
    TpMatchModeValues *value;
    value = (TpMatchModeValues *)_cmd->getValue();
    value->_threshold = atof(_threshold->getFieldValue());

    runCmd(value);
}

void TpSetMatchModeValuesCmdIf::setValue(CmdValue value)
{
    TpMatchModeValues *v;
    v = (TpMatchModeValues *)value;

    char buf[16];
    
    sprintf(buf, "%d", v->_pmk);
    _pmk->setFieldValue(buf);

    sprintf(buf, "%d", v->_lsm);
    _lsm->setFieldValue(buf);

    sprintf(buf, "%d", v->_searchWindow);
    _sw->setFieldValue(buf);

    sprintf(buf, "%.2f", v->_accuracy);
    _accuracy->setFieldValue(buf);

    sprintf(buf, "%.2f", v->_threshold);
    _threshold->setFieldValue(buf);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpStripPanTool.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////

#include "TpStripPanTool.h"
#include "XvicImage.h"
#include <math.h>
#include "LutToImageWidgetGlue.h"

// Resources for this class

XtResource TpStripPanTool::_resources[] = {
 {
   (char *)XvicNapplyStretchToPan,
   (char *)XvicCApplyStretchToPan,
   XmRBoolean,
   sizeof(Boolean),
   XtOffset(TpStripPanTool *, _apply_stretch_to_pan),
   XmRImmediate,
   (XtPointer) False,
 },
 {
   (char *)XvicNpanBoxColor,
   (char *)XvicCPanBoxColor,
   XmRString,
   sizeof(String),
   XtOffset(TpStripPanTool *, _box_color_string),
   XmRImmediate,
   (XtPointer) "red",
 },
};

TpStripPanTool::TpStripPanTool(Widget parent, const char *name,
		ImageData *model, Widget big_iw,
		Dimension view_height, Dimension view_width,
		Boolean preserve_aspect,
        	Lut *rlut, Lut *glut, Lut *blut,
		Lut *rplut, Lut *gplut, Lut *bplut) :
			// = True
	ImageDisplayView(parent, name, model,
		computeSizeY(view_height, view_width, preserve_aspect, big_iw),
		computeSizeX(view_height, view_width, preserve_aspect, big_iw))
{
   XGCValues values;

   _preserve_aspect = preserve_aspect;
   setAspectRatio();

   getResources(_resources, XtNumber(_resources));

   _big_iw = big_iw;
   setUserZoom2Fit();

   copyDisplayModeResources();
   copyDataRangeResources();

   // Add callbacks to keep track of the big widget

   XtAddCallback(_big_iw, XvicNvisibleAreaCallback,
		&TpStripPanTool::bigWidgetChangeCallback, (XtPointer) this);
   XtAddCallback(_big_iw, XvicNresizeCallback,
		&TpStripPanTool::bigWidgetChangeCallback, (XtPointer) this);
   XtAddCallback(_big_iw, XvicNpanCallback,
		&TpStripPanTool::bigWidgetChangeCallback, (XtPointer) this);

   // Add an Input callback so the user can manipulate our widget

   XtAddCallback(_iw, XvicNinputCallback,
		&TpStripPanTool::inputCallback, (XtPointer) this);

   // Connect the stretch to the main, if requested

   _lutGlue = NULL;
   _pseudoLutGlue = NULL;
   if (_apply_stretch_to_pan && rlut != NULL && glut != NULL && blut != NULL)
      _lutGlue = new LutToImageWidgetGlue(rlut, glut, blut, _iw, True);
   if (_apply_stretch_to_pan && rplut != NULL && gplut != NULL && bplut != NULL)
      _pseudoLutGlue = new LutToImageWidgetGlue(rplut, gplut, bplut, _iw,False);

   _box_id = 0;

   XColor xcolor;
   XParseColor(XtDisplay(_iw), DefaultColormapOfScreen(XtScreen(_iw)),
	_box_color_string, &xcolor);
   _box_color = XvicImageGetGrColor(_iw, &xcolor);

   values.line_width = 1;
   values.dashes = 4;
   values.line_style = LineOnOffDash;
   _box_gc=XvicImageCreateGC(_iw, GCLineWidth|GCDashList|GCLineStyle, &values);

   _input_x = 0;
   _input_y = 0;

   drawNewBox();

}

////////////////////////////////////////////////////////////////////////

TpStripPanTool::~TpStripPanTool()
{
   if (_lutGlue)
      delete _lutGlue;
   if (_pseudoLutGlue)
      delete _pseudoLutGlue;

   XtRemoveCallback(_big_iw, XvicNvisibleAreaCallback,
                &TpStripPanTool::bigWidgetChangeCallback, (XtPointer) this);
   XtRemoveCallback(_big_iw, XvicNresizeCallback,
                &TpStripPanTool::bigWidgetChangeCallback, (XtPointer) this);
   XtRemoveCallback(_big_iw, XvicNpanCallback,
                &TpStripPanTool::bigWidgetChangeCallback, (XtPointer) this);
   XtRemoveCallback(_iw, XvicNinputCallback,
                &TpStripPanTool::inputCallback, (XtPointer) this);

}

////////////////////////////////////////////////////////////////////////
// Callback from the big widget to let us know something changed.
////////////////////////////////////////////////////////////////////////

void TpStripPanTool::bigWidgetChangeCallback(Widget, XtPointer clientData,
					      XtPointer callData)
{
   TpStripPanTool *obj = (TpStripPanTool *)clientData;

   obj->bigWidgetChange(callData);
}

////////////////////////////////////////////////////////////////////////
// Something changed in the big widget.  Deal with it.
////////////////////////////////////////////////////////////////////////

void TpStripPanTool::bigWidgetChange(XtPointer callData)
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;

   switch (cb->reason) {

      case XvicCR_VISIBLE_AREA:
         if (cb->flags & XvicSIZE_CHANGED) {
            newSize();
            drawNewBox();
         }
         if ((cb->flags & XvicZOOM_CHANGED) ||
	     (cb->flags & XvicSIZE_CHANGED) ||
	     (cb->flags & XvicSUBPIXEL_CHANGED)) {
            drawNewBox();
         }
         else if (cb->flags & XvicPAN_CHANGED) {
            moveBox();		// drawNewBox will handle pans as well
         }
         if (cb->flags & XvicDITHER_CHANGED) {
            copyDisplayModeResources();
         }
         if (cb->flags & XvicRANGE_CHANGED) {
            copyDataRangeResources();
         }
         // MODE_CHANGED is ignored because we must be notified of that
         // (color/bw and data type) via the model.

         break;

      case XvicCR_RESIZE:
         setAspectRatio();
         drawNewBox();
         break;

      case XvicCR_PAN:
         moveBox();
   }
}

////////////////////////////////////////////////////////////////////////
// Callback from our widget when the user takes an action.
////////////////////////////////////////////////////////////////////////

void TpStripPanTool::inputCallback(Widget, XtPointer clientData,
				    XtPointer callData)
{
   TpStripPanTool *obj = (TpStripPanTool *)clientData;

   obj->input(callData);
}

////////////////////////////////////////////////////////////////////////
// Process user input from our widget.  You will notice a remarkable
// similarity between the Input actions here and the true Actions
// provided by the image widget.  Possible actions are:
//	Input(pan_mouse, start)	Start a mouse pan.  May be triggered by
//				ButtonPress/Release, Enter/LeaveNotify,
//				KeyPress/Release, or MotionNotify.
//	Input(pan_mouse, drag)	Continue a mouse pan.  This one actually does
//				a pan (based on the deltas from "start").
//				The same events are valid.  A "start" action
//				must happen first or the results are undefined.
//	Input(pan_one, direct)	Move box one image pixel in the indicated
//				direction (one of "left", "right", "up", "down")
//	Input(pan_half_view, direct) Move box half of its view size in the
//				indicated direction.
//	Input(pan_edge, direct)	Move box to edge of image in the indicated
//				direction.
////////////////////////////////////////////////////////////////////////

void TpStripPanTool::input(XtPointer callData)
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;
   int x_pan, y_pan;

   if (cb->reason != XvicCR_INPUT)
      return;				// oops

   if (cb->input_num_params != 2)
      return;				// oops

   XtVaGetValues(_big_iw, XvicNxPan, &x_pan, XvicNyPan, &y_pan, NULL);

   if (strcmp(cb->input_params[0], "pan_mouse") == 0) {
      if (strcmp(cb->input_params[1], "start") == 0) {
         _input_x = cb->x;
         _input_y = cb->y;
      }
      else if (strcmp(cb->input_params[1], "drag") == 0) {
         XtVaSetValues(_big_iw, XvicNxPan, x_pan + (cb->x - _input_x),
				XvicNyPan, y_pan + (cb->y - _input_y),
				NULL);
         _input_x = cb->x;
         _input_y = cb->y;
      }
      else if (strcmp(cb->input_params[1], "move") == 0) {
	 Dimension big_width, big_height;
	 XtVaGetValues(_big_iw, XvicNviewWidth, &big_width, 
				XvicNviewHeight, &big_height,
				NULL);
	 XtVaSetValues(_big_iw, XvicNxPan, cb->x - (big_width / 2), 
				XvicNyPan, cb->y - (big_height / 2), NULL);
      }
   }

   else {
      int x_amount = 0;
      int y_amount = 0;

      if (strcmp(cb->input_params[0], "pan_one") == 0) {
         x_amount = 1;
         y_amount = 1;
      }
      else if (strcmp(cb->input_params[0], "pan_half_view") == 0) {
         // Don't just use viewH/W / 2 because we want Image coordinates
         int x1, x2, y1, y2;
         XvicImageDisplayBounds(_big_iw, &x1, &y1, &x2, &y2);
         x_amount = (x2-x1+1) / 2;
         y_amount = (y2-y1+1) / 2;
      }
      else if (strcmp(cb->input_params[0], "pan_edge") == 0) {
         XtVaGetValues(_big_iw, XvicNimageWidth, &x_amount,
				XvicNimageHeight, &y_amount, NULL);
      }

      if (x_amount == 0 && y_amount == 0)
         return;				// invalid string

      if (strcmp(cb->input_params[1], "left") == 0)
         XtVaSetValues(_big_iw, XvicNxPan, x_pan - x_amount, NULL);
      else if (strcmp(cb->input_params[1], "right") == 0)
         XtVaSetValues(_big_iw, XvicNxPan, x_pan + x_amount, NULL);
      else if (strcmp(cb->input_params[1], "up") == 0)
         XtVaSetValues(_big_iw, XvicNyPan, y_pan - y_amount, NULL);
      else if (strcmp(cb->input_params[1], "down") == 0)
         XtVaSetValues(_big_iw, XvicNyPan, y_pan + y_amount, NULL);
   }
}

////////////////////////////////////////////////////////////////////////
// Draw a new box, eraseing the old one first if necessary
////////////////////////////////////////////////////////////////////////

void TpStripPanTool::drawNewBox()
{
   int x1, x2, y1, y2;

   if (_box_id) {
      XvicImageEraseObject(_iw, _box_id);
   }

   // Get the currently displayed area

   XvicImageDisplayBounds(_big_iw, &x1, &y1, &x2, &y2);

   // Draw the box.  We use four separate lines rather than a rectangle or
   // polyline because the bounding box of a rect or polyline is the entire
   // enclosed area, while the bounding box for four separate lines is just
   // the lines.  Since the widget will repaint the whole bounding box,
   // this is actually much faster.

   // Top
   _box_id = XvicImageDrawLine(_iw, 0, _box_gc, _box_color, x1, y1, x2, y1);
   // Right
   XvicImageDrawLine(_iw, _box_id, _box_gc, _box_color, x2, y1, x2, y2);
   // Bottom
   XvicImageDrawLine(_iw, _box_id, _box_gc, _box_color, x2, y2, x1, y2);
   // Right
   XvicImageDrawLine(_iw, _box_id, _box_gc, _box_color, x1, y2, x1, y1);

   _box_x = x1;
   _box_y = y1;
}

////////////////////////////////////////////////////////////////////////
// Simply move the existing box based on the new pan
////////////////////////////////////////////////////////////////////////

void TpStripPanTool::moveBox()
{
   int x_pan, y_pan;

   if (_box_id == 0) {		// oops!
      drawNewBox();
      return;
   }

   XtVaGetValues(_big_iw, XvicNxPan, &x_pan, XvicNyPan, &y_pan, NULL);

   XvicImageMoveObject(_iw, _box_id, (x_pan - _box_x), (y_pan - _box_y));

   _box_x = x_pan;
   _box_y = y_pan;

   int x1, x2, y1, y2;
   XvicImageDisplayBounds(_big_iw, &x1, &y1, &x2, &y2);
   int x3, x4, y3, y4;
   XvicImageDisplayBounds(_iw, &x3, &y3, &x4, &y4);
   if (x3 > x1) {
      XtVaSetValues(_iw, XvicNxPan, x1, NULL);
   }
   if (y3 > y1) {
      XtVaSetValues(_iw, XvicNyPan, y1, NULL);
   }
   if (x4 < x2) {
      int w = x4 - x3;
      XtVaSetValues(_iw, XvicNxPan, x2 - w, NULL);
   }
   if (y4 < y2) {
      int h = y4 - y3;
      XtVaSetValues(_iw, XvicNyPan, y2 - h, NULL);
   }
}

////////////////////////////////////////////////////////////////////////
// Set the Shell's aspect ratio based on the image size
////////////////////////////////////////////////////////////////////////

void TpStripPanTool::setAspectRatio()
{
   Widget shell;

   if (!_preserve_aspect)
      return;

   if (!_iw)
      return;

   shell = _iw;
   do {
      shell = XtParent(shell);
   } while (shell && !XtIsShell(shell));

   if (shell) {		// found the shell
      int width, height;
      XtVaGetValues(_iw, XvicNimageWidth, &width, XvicNimageHeight, &height,
			NULL);
      XtVaSetValues(shell,
		XmNminAspectX, width, XmNmaxAspectX, width,
		XmNminAspectY, height, XmNmaxAspectY, height,
		NULL);
   }
}

////////////////////////////////////////////////////////////////////////
// Copy the display-related resources (those that could affect what colormap
// is used) from the big widget to here, to prevent flashing.
////////////////////////////////////////////////////////////////////////

void TpStripPanTool::copyDisplayModeResources()
{
   unsigned char bw_dither, color_dither, visual_type;
   unsigned char stretch_policy, colormap_policy, lut_type, lut16_type;
   int gray_levels, red_levels, green_levels, blue_levels;

   XtVaGetValues(_big_iw,
		XvicNvisualType, &visual_type,
		XvicNbwDither, &bw_dither,
		XvicNcolorDither, &color_dither,
		XvicNstretchPolicy, &stretch_policy,
		XvicNcolormapPolicy, &colormap_policy,
		XvicNlutType, &lut_type,
		XvicNlut16Type, &lut16_type,
		XvicNgrayLevels, &gray_levels,
		XvicNredLevels, &red_levels,
		XvicNgreenLevels, &green_levels,
		XvicNblueLevels, &blue_levels,
		NULL);

   XtVaSetValues(_iw,
		XvicNvisualType, visual_type,
		XvicNbwDither, bw_dither,
		XvicNcolorDither, color_dither,
		XvicNstretchPolicy, stretch_policy,
		XvicNcolormapPolicy, colormap_policy,
		XvicNlutType, lut_type,
		XvicNlut16Type, lut16_type,
		XvicNgrayLevels, gray_levels,
		XvicNredLevels, red_levels,
		XvicNgreenLevels, green_levels,
		XvicNblueLevels, blue_levels,
		NULL);
}

////////////////////////////////////////////////////////////////////////
// Copy the data-type resources from the big widget to here, so the
// display will be visible.
////////////////////////////////////////////////////////////////////////

void TpStripPanTool::copyDataRangeResources()
{
   double raw_data_min, raw_data_max;
   int scaled_data_max, output_data_max;

   XtVaGetValues(_big_iw,
		XvicNrawDataMin, &raw_data_min,
		XvicNrawDataMax, &raw_data_max,
		XvicNscaledDataMax, &scaled_data_max,
		XvicNoutputDataMax, &output_data_max,
		NULL);

   XtVaSetValues(_iw,
		XvicNrawDataMin, XvicDOUBLE_ARG(raw_data_min),
		XvicNrawDataMax, XvicDOUBLE_ARG(raw_data_max),
		XvicNscaledDataMax, scaled_data_max,
		XvicNoutputDataMax, output_data_max,
		NULL);
}

////////////////////////////////////////////////////////////////////////
// Called whenever the size of the main image changes.  We must set the
// imageW/H resources (which is taken care of by the model update but
// it's needed sooner than that for the aspect ratio), then we must force
// the window to be a new size.  Note that XmNallowShellResize must be True
// on the shell for this to work.  We set the tile size to the same as the
// image size so the widget does minimal work - the tile size will be reset
// when the model update is called.
////////////////////////////////////////////////////////////////////////

void TpStripPanTool::newSize()
{
   int width, height;
   Dimension old_width, old_height;
   Dimension new_width, new_height;

   if (!_preserve_aspect)
      return;

   if (!_iw)
      return;

   XtVaGetValues(_big_iw, XvicNimageWidth, &width, XvicNimageHeight, &height,
			NULL);
   XtVaSetValues(_iw, XvicNimageWidth, width, XvicNimageHeight, height,
		      XvicNtileWidth, width, XvicNtileHeight, height, NULL);

   setUserZoom2Fit();		// Recalc zoom since the size changed

   setAspectRatio();

   // Figure out what size to be to cover the same "area"

   XtVaGetValues(_iw, XvicNviewWidth, &old_width,
		      XvicNviewHeight, &old_height, NULL);

   new_width = computeSizeX(old_width, old_height, _preserve_aspect, _big_iw);
   new_height = computeSizeY(old_width, old_height, _preserve_aspect, _big_iw);

   XtVaSetValues(_iw, XvicNviewWidth, new_width, XvicNviewHeight, new_height,
			NULL);

}

////////////////////////////////////////////////////////////////////////
// Determine the size that most closely matches the desired one.
// We want the "area" to be about the same as the desired area, but it
// must match the aspect ratio.  The calling sequence for these is all
// messed up because it must be able to be called from the superclass
// constructor.  Is there a better way to do this??
////////////////////////////////////////////////////////////////////////

Dimension TpStripPanTool::computeSizeX(int desired_width, int desired_height,
	Boolean preserveAspect, Widget big_iw)
{
   int width, height;

   if (!preserveAspect)
      return desired_width;

   XtVaGetValues(big_iw, XvicNimageWidth, &width, XvicNimageHeight, &height,
			NULL);

   double aspect = (double)width / (double)height;

   return (int)sqrt(desired_width * desired_height * aspect);
}

Dimension TpStripPanTool::computeSizeY(int desired_width, int desired_height,
	Boolean preserveAspect, Widget big_iw)
{
   int width, height;

   if (!preserveAspect)
      return desired_height;

   XtVaGetValues(big_iw, XvicNimageWidth, &width, XvicNimageHeight, &height,
			NULL);

   double aspect = (double)width / (double)height;

   return (int)sqrt(desired_width * desired_height / aspect);
}

void TpStripPanTool::calcZoomToFit(Widget iw, int *in, int *out)
{
    Dimension view_width, view_height;
    int image_width, image_height;
    
    XtVaGetValues(iw,
                  XvicNviewWidth, &view_width,
                  XvicNviewHeight, &view_height,
                  XvicNimageWidth, &image_width,
                  XvicNimageHeight, &image_height,
                  NULL);

    // Sanity checks
    
    if (view_width <= 0)
        view_width = 0;
    if (view_height <= 0)
        view_height = 0;
    if (image_width <= 0)
        image_width = 0;
    if (image_height <= 0)
        image_height = 0;

    if (image_width > 2 * image_height) {
        *in = view_height;
        *out = image_height;
    }
    else if (image_height > 2 * image_width) {
        *in = view_width;
        *out = image_width;
    }
    else {
        ImageDisplayView::calcZoomToFit(iw, in, out);
    }
}

void TpStripPanTool::setUserZoom2Fit()
{
   // Save old scrollbar policy and set to XvicNEVER so we can return
   // to this policy when user selects a new zoom.

   if (_zoom2Fit == False) {
      _zoom2Fit = True;
     XtVaGetValues(_iw, XvicNscrollBarDisplayPolicy,
                        &_SaveScrollBarDisplayPolicyDuringZoom2Fit,
                        NULL);
   }

   resizeImage();
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tp_misc.imake
#define SUBROUTINE tp_misc
#define MODULE_LIST \
 TpApplication.cc TpWindow.cc TpDisplayer.cc TpSubDisplayer.cc \
 ImageOverlayView.cc TpImageView.cc
#define MODULE_LIST2 \
 TpMatchManager.cc TpMatch.cc DrawObject.cc TpPoint.cc TpPointModel.cc \
 TpZoomControl.cc TpContrastControl.cc
#define MODULE_LIST3 \
 TpPosView.cc TpCursorModel.cc TpImageInfo.cc TpImageReference.cc \
 TpSelectionMgr.cc TpWedgeOverlayView.cc TpMatchBrowseControl.cc
#define MODULE_LIST4 \
 TpMatchModeValuesCmdIf.cc \
 TpAutofindRes.cc TpMatchModeRes.cc TpStripPanTool.cc

#define P2_SUBLIB

#define MAIN_LANG_C_PLUS_PLUS
#define USES_C_PLUS_PLUS
#define CCC_TEMPLATES

#define LIB_P2SUB
#define LIB_GUISUB
#define LIB_MOTIFAPP
#define LIB_MOTIF
#define LIB_XMU
#define LIB_XPM

$ Return
$!#############################################################################
