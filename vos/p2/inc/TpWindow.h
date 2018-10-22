//////////////////////////////////////////////////////////////////////////////
// TpWindow.h
//////////////////////////////////////////////////////////////////////////////
#ifndef TPWINDOW_H
#define TPWINDOW_H
#include "MenuWindow.h"
#include "Cmd.h"
#include "TpDefs.h"
#include "TpAutofindResultsDialog.h"
#include "TpMatchModeResultsDialog.h"
#include "TpMatchModeDialog.h"

class TpWindow : public MenuWindow {

  private:

    static XtResource _resources[];

  protected:

    Boolean _enablePrintWidgetTree;
    Boolean _enableSetSpecialStatus;

    char *_config;

    Cmd *_savePointCmd;
    Cmd *_deletePointCmd;
    Cmd *_listPointsCmd;
    Cmd *_redoMatchIdsCmd;
    Cmd *_autoSyncPointsCmd;
    Cmd *_quitCmd;
    Cmd *_exitStatusCmd;
    Cmd *_shiftRightCmd;
    Cmd *_shiftLeftCmd;
    Cmd *_postLoadImageCmd;
    Cmd *_postRemoveImageCmd;
    Cmd *_postLoadPointFileCmd;
    Cmd *_closePointFileCmd;

    Cmd *_postDisplayModeCmd;
    Cmd *_postAutofindResultsCmd;
    Cmd *_postMatchModeResultsCmd;
    Cmd *_postQualFormatDialogCmd;
    Cmd *_postPointEditorOptsDialogCmd;
    Cmd *_postPointSymbolsDialogCmd;
    Cmd *_postPointTagsDialogCmd;
    Cmd *_postCursorSymbolsDialogCmd;
    Cmd *_postAutofindDialogCmd;
    Cmd *_postMatchModeDialogCmd;

    Cmd *_savePointFileCmd;
    Cmd *_saveAndExitCmd;
    Cmd *_postSavePointFileAsCmd;
    Cmd *_loadConfigCmd;
    Cmd *_postLoadConfigCmd;
    Cmd *_saveConfigCmd;
    Cmd *_saveConfigAsCmd;
    Cmd *_postSaveConfigAsCmd;
    Cmd *_printCmd;

    CmdList *_modeRadioList;
    CmdList *_findRadioList;

    TpAutofindResultsDialog *_autofindResultsDialog;
    TpMatchModeResultsDialog *_matchModeResultsDialog;
    TpMatchModeDialog *_matchModeDialog;

    void initErrorManager() const;
    void initPrefManager() const;
    virtual Widget createWorkArea(Widget parent);
    virtual void createMenuPanes();

    void deleteWindowResponse();

  public:

    TpWindow(const char *name);
    ~TpWindow();

    void resetSaveCmd(Boolean enable);

    void activateSavePointCmd() { _savePointCmd->activate(); }
    void deactivateSavePointCmd() { _savePointCmd->deactivate(); }

    void loadConfig(char *filename=NULL);
    void saveConfig(char *filename=NULL);
    char *getConfigFileName() { return _config; }

    TpAutofindResultsDialog *getAutofindResultsDialog() 
	{ return _autofindResultsDialog; }

    TpMatchModeResultsDialog *getMatchModeResultsDialog()
        { return _matchModeResultsDialog; }

    TpMatchModeDialog *getMatchModeDialog()
	{ return _matchModeDialog; }

};
#endif
