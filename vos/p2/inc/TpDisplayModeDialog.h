////////////////////////////////////////////////////////////
// TpDisplayModeDialog.h: Dialog containing display mode values.
////////////////////////////////////////////////////////////
#ifndef TpDisplayModeDialog_H
#define TpDisplayModeDialog_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpDisplayer;

class TpDisplayModeDialog : public CustomDialog {

  protected:

    TpDisplayer *_displayer;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpDisplayModeDialog(const char *name, TpDisplayer *);

    virtual Widget createWorkArea(Widget);

};

#endif
