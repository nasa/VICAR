//////////////////////////////////////////////////////////////////////////////
// TpMatchModeResultsDialog.h: Dialog containing matchMode mode values.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPMATCHMODERESULTSDIALOG_H
#define TPMATCHMODERESULTSDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;
class TpMatchModeResults;

class TpMatchModeResultsDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;

    TpMatchModeResults *_matchModeResults;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpMatchModeResultsDialog(const char *name, TpMatchManager *);

    virtual Widget createWorkArea(Widget);

    void setValues(float [5]);

    Boolean isDumpToStdout();   // True if we should dump results to stdout too
                                // We should really dump in here, but for
                                // headers and such we must dump outside.

};

#endif
