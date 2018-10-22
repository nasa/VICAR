//////////////////////////////////////////////////////////////////////////////
// TpQualFormatDialog.h: Dialog containing point file format values.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPQUALFORMATDIALOG_H
#define TPQUALFORMATDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;
class TpQualFormatCmd;

class TpQualFormatDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager;

    TpQualFormatCmd *_cmdGen, *_cmdPnt;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpQualFormatDialog(const char *name, TpMatchManager *);

    virtual Widget createWorkArea(Widget);
    virtual void post();

};

#endif
