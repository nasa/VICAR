//////////////////////////////////////////////////////////////////////
// StretchDialog.h: This class creates a work area for stretch dialog.
//////////////////////////////////////////////////////////////////////
#ifndef STRETCHDIALOG_H
#define STRETCHDIALOG_H
#include "MenuDialog.h"
#include "HelpBrowser.h"

class Cmd;
class Lut;

class StretchDialog : public MenuDialog {

  private:

    Cmd *_cmd;

    Lut *_lutR, *_lutG, *_lutB;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    StretchDialog(const char *name, Cmd *cmd, Lut *lutR, Lut *lutG, Lut *lutB);

    virtual Widget createWorkArea(Widget parent);
    virtual void createMenuPanes();

    virtual const char *const className() { return "StretchDialog"; }
};
#endif
