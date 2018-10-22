//////////////////////////////////////////////////////////////
//PrefDialog.h: Include file to create the Preferences Dialog 
//              Box
//////////////////////////////////////////////////////////////
#ifndef PREFDIALOG_H
#define PREFDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class Cmd;
class CmdList;
class BasicImageView;

class PrefDialog: public CustomDialog {

  private:

    Cmd *_showSideBarCmd;
    Cmd *_showBorderCmd;
    Cmd *_showMenuBarCmd;
    Cmd *_showFullScreenCmd;

    Widget _imageViewWidget;
    BasicImageView *_imageView;
    int _CMMRedLevels,  _CMMGreenLevels, _CMMBlueLevels, _CMMGrayLevels;

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    PrefDialog(const char *name, Cmd *, Cmd *, Cmd *, Cmd *, Widget, 
	       BasicImageView *, int, int, int, int);

    virtual Widget createWorkArea(Widget);
};

#endif

