//////////////////////////////////////////////
// PseudoDialog.h: Dialog box for pseudocolor tool
//////////////////////////////////////////////
#ifndef PSEUDODIALOG_H
#define PSEUDODIALOG_H
#include "MenuDialog.h"
#include "HelpBrowser.h"

class Cmd;
class CmdList;
class PseudoValue;
class PseudoMarks;

class PseudoDialog : public MenuDialog {

  private:

    static XtResource _resources[];

    Widget _iw, _form;
    Cmd *_cmd;
    Cmd *_modeCmd;
    Cmd *_setDeferredCmd;
    PseudoValue *_pseudoValue;
    PseudoMarks *_pseudoMarks;
    int _numPseudoFiles;
    char *_filename, *_dirUNIX, *_dirVMS;
    char *_fullFilename;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    PseudoDialog(const char *name, Cmd *, Cmd *, Widget);

    virtual Widget createWorkArea(Widget);
    virtual void createMenuPanes();

    virtual void post();
};
#endif
