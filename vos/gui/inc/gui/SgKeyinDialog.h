////////////////////////////////////////////////////////////////
// SgKeyinDialog.h: A dialog class used to take single string
//      input from the user
////////////////////////////////////////////////////////////////

#ifndef SGKEYINDIALOG_H
#define SGKEYINDIALOG_H

#include "SgCmdDialog.h"
#include "SgKeyinCmdInterface.h"

class SgKeyinDialog : public SgCmdDialog {

 protected:

   Cmd *_command;
   CmdInterface *_ci;

   virtual void setCmdIfDeferredExec() { }

   virtual CmdInterface *createCmdInterface(Widget, Cmd *);

 public:
 
   SgKeyinDialog(const char *, Cmd *);

   virtual ~SgKeyinDialog() { };

   virtual CmdInterface *getCmdInterface() { return _ci; }
};

#endif
