//////////////////////////////////////////////////////////////////////////////
// TpAutofindDialog.h: Dialog containing autofind values.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPAUTOFINDDIALOG_H
#define TPAUTOFINDDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;
class TpSetAutofindCmd;
class CmdList;

class TpAutofindDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;
    CmdList *_findRadioList;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpAutofindDialog(const char *name, TpMatchManager *, CmdList *);

    virtual Widget createWorkArea(Widget);

};

#endif
