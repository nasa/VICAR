//////////////////////////////////////////////////////////////////////////////
// TpAutofindResultsDialog.h: Dialog containing autofind mode values.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPAUTOFINDRESULTSDIALOG_H
#define TPAUTOFINDRESULTSDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;
class TpAutofindResults;

class TpAutofindResultsDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;

    TpAutofindResults *_autofindResults;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpAutofindResultsDialog(const char *name, TpMatchManager *);

    virtual Widget createWorkArea(Widget);

    void setValues(double [6]);
};

#endif
