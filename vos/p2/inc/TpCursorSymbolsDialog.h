//////////////////////////////////////////////////////////////////////////////
// TpCursorSymbolsDialog.h: Dialog containing cursor symbol options.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPCURSORSYMBOLSDIALOG_H
#define TPCURSORSYMBOLSDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpDisplayer;

class TpCursorSymbolsDialog : public CustomDialog {

  protected:

    TpDisplayer *_displayer;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpCursorSymbolsDialog(const char *name, TpDisplayer *);

    virtual Widget createWorkArea(Widget);

};

#endif
