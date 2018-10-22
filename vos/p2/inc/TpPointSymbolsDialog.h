//////////////////////////////////////////////////////////////////////////////
// TpPointSymbolsDialog.h: Dialog containing point editor options.
//////////////////////////////////////////////////////////////////////////////
// vxp JPL
//////////////////////////////////////////////////////////////////////////////
#ifndef TPPOINTSYMBOLSDIALOG_H
#define TPPOINTSYMBOLSDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class TpMatchManager;
class TpSetPointSymbolCmd;
class TpSetPointSizeCmd;
class TpColorCodeCmd;

class TpPointSymbolsDialog : public CustomDialog {

  protected:

    TpMatchManager *_matchManager; 

    TpSetPointSymbolCmd *_symbolCmd[6];
    TpSetPointSizeCmd *_sizeCmd[6];
    TpColorCodeCmd *_colorCodeCmd[2];
    Cmd *_colorCmd;
    Cmd *_colorSelCmd;

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    TpPointSymbolsDialog(const char *name, TpMatchManager *);

    virtual Widget createWorkArea(Widget);

    virtual void post ();
};

#endif
