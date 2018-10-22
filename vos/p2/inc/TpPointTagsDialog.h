//////////////////////////////////////////////////////////////////////////////
// TpPointTagsDialog.h: Dialog containing point tag options.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPPOINTTAGSDIALOG_H
#define TPPOINTTAGSDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;

class TpPointTagsDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpPointTagsDialog(const char *name, TpMatchManager *);

    virtual Widget createWorkArea(Widget);

};

#endif
