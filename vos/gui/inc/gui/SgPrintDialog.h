///////////////////////////////////////////////////////////////////
// SgPrintDialog.h: Creates a dialog for printing 
///////////////////////////////////////////////////////////////////
#ifndef SGPRINTDIALOG_H
#define SGPRINTDIALOG_H

#include "CustomDialog.h"
#include "HelpBrowser.h"

class SgPrintValue;
class Cmd;
class RadioCmdBox;

class SgPrintDialog: public CustomDialog {

  private:
  
    SgPrintValue    *_printValue;
    Cmd               *_printCmd;

    Cmd               *_toPrinterCmd;
    Cmd               *_toFileCmd;

    Cmd               *_filenameCmd;
    Cmd               *_printerCmd;

    Cmd               *_asVicarCmd;
    Cmd               *_asPsCmd;

    Cmd               *_paperA2Cmd;
    Cmd               *_paperA3Cmd;
    Cmd               *_paperA4Cmd;
    Cmd               *_paperLetterCmd;
    Cmd               *_paperLegalCmd;

    CmdList           *_radioListDest;
    RadioCmdBox       *_radioCmdBoxDest;

    CmdList           *_radioListFormat;
    RadioCmdBox       *_radioCmdBoxFormat;

    CmdList           *_radioListPaper;
    RadioCmdBox       *_radioCmdBoxPaper;

    virtual void apply();

  protected:

    virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

  public:

    SgPrintDialog(const char *name, Cmd *printCmd, SgPrintValue *printValue = NULL);
    ~SgPrintDialog();

    virtual Widget createWorkArea(Widget);

};

#endif

