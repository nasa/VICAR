/////////////////////////////////////////////////////////////////
// StretchParmListDialog.h: Custom dialog for selecting 
// values for table, itable and alarm stretches.
/////////////////////////////////////////////////////////////////
#ifndef STRETCHPARMLISTDIALOG_H
#define STRETCHPARMLISTDIALOG_H
#include "CustomDialog.h"
#include "HelpBrowser.h"
#include "StretchValue.h" // For StretchType definition

class Cmd;

class StretchParmListDialog : public CustomDialog {

  private:

    StretchType _stretchType; 	// ITABLE or TABLE or ALARM
    int _defValue;

    Cmd *_cmd;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    StretchParmListDialog(const char *name, Cmd *, StretchType, int defValue=0);

    virtual Widget createWorkArea(Widget);

};
#endif
