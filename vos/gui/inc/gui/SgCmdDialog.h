////////////////////////////////////////////////////////////////
// SgCmdDialog.h: This is an abstract class that implements a 
// dialog that holds a command interface.  The subclasses must
// provide the *command interface*. The *command* itself is created
// outside the class and is passed in via the constructor.
////////////////////////////////////////////////////////////////

#ifndef SGCMDDIALOG_H
#define SGCMDDIALOG_H
#include "CustomDialog.h"
#include "HelpBrowser.h"

class Cmd;
class CmdInterface;

class SgCmdDialog : public CustomDialog {


 private:

   CmdInterface *_cmdInterface;
   Cmd *_cmd;

   virtual CmdInterface *createCmdInterface(Widget, Cmd *) = 0;
   virtual void setCmdIfDeferredExec();

   virtual void help(Widget w, XtPointer) { theHelpBrowser->run(w); }

 public:

   SgCmdDialog(const char *name, Cmd *cmd, ButtonState showOk,
	       ButtonState showApply, ButtonState showReset,
	       ButtonState showCancel, ButtonState showHelp);

   virtual ~SgCmdDialog();

   virtual void resetDialog(Cmd *);

   virtual Widget createWorkArea(Widget);
};
#endif
