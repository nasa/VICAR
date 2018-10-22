//////////////////////////////////////////////////////////////////////////////
// TpPointEditorOptsDialog.h: Dialog containing point editor options.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPPOINTEDITOROPTSDIALOG_H
#define TPPOINTEDITOROPTSDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;

class TpPointEditorOptsDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpPointEditorOptsDialog(const char *name, TpMatchManager *);

    virtual Widget createWorkArea(Widget);

};

#endif
