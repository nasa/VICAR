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

