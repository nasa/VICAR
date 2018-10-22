//////////////////////////////////////////////////////////////////////
// SiPrintDialog.h: This class creates a work area for print dialog.
//////////////////////////////////////////////////////////////////////
#ifndef SIPRINTDIALOG_H
#define SIPRINTDIALOG_H
#include "CustomDialog.h"
#include "HelpBrowser.h"

class Cmd;
class SiPrintCmdInterface;

class SiPrintDialog : public CustomDialog {

  private:

    Cmd *_cmd;
    SiPrintCmdInterface *_printCI;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    SiPrintDialog(const char *name, Cmd *cmd);

    virtual Widget createWorkArea(Widget parent);

    virtual void post();

    virtual const char *const className() { return "SiPrintDialog"; }
};
#endif

