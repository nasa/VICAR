/////////////////////////////////////////////////////////////////
// StretchPercValuesDialog.h: Custom dialog for displaying  
// percent stretch upper and lower limits.
/////////////////////////////////////////////////////////////////
#ifndef STRETCHPERCVALUESDIALOG_H
#define STRETCHPERCVALUESDIALOG_H
#include "CustomDialog.h"
#include "HelpBrowser.h"

class Cmd;

class StretchPercValuesDialog : public CustomDialog {

  private:

    Cmd *_cmd;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    StretchPercValuesDialog(const char *name, Cmd *);

    virtual Widget createWorkArea(Widget);

};
#endif
