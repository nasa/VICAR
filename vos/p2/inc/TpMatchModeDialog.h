//////////////////////////////////////////////////////////////////////////////
// TpMatchModeDialog.h: Dialog containing Match Mode values.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPMATCHMODEDIALOG_H
#define TPMATCHMODEDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;
class TpSetMatchModeCmd;
class CmdList;
class Cmd;

class TpMatchModeDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;
    CmdList *_modeRadioList;

    Cmd *_setMatchModeValuesCmd;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpMatchModeDialog(const char *name, TpMatchManager *, CmdList *);

    virtual Widget createWorkArea(Widget);

    Cmd *getSetMatchModeValuesCmd() { return _setMatchModeValuesCmd; }
};

#endif
