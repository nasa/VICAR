//////////////////////////////////////////////////////////////////////
// SiSaveDialog.h: This class creates a work area for save dialog.
//////////////////////////////////////////////////////////////////////
#ifndef SISAVEDIALOG_H
#define SISAVEDIALOG_H
#include "CustomDialog.h"
#include "HelpBrowser.h"

class Cmd;
class SiSaveCmdInterface;

class SiSaveDialog : public CustomDialog {

  private:

    Cmd *_cmd;
    SiSaveCmdInterface *_saveCI;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    SiSaveDialog(const char *name, Cmd *cmd);

    virtual Widget createWorkArea(Widget parent);

    virtual void post();

    virtual const char *const className() { return "SiSaveDialog"; }
};
#endif

