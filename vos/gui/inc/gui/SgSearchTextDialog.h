////////////////////////////////////////////////////////////////
// SgSearchTextDialog.h:  A dialog for SgSearchTextCmd objects;
// overrides CustomDialog::apply for immediate execution of a 
// single command (as opposed to the deferred list)
////////////////////////////////////////////////////////////////
 
#ifndef SGSEARCHTEXTDIALOG_H
#define SGSEARCHTEXTDIALOG_H
 
#include "SgCmdDialog.h"
#include "SgSearchTextCmdInterface.h"

class SgSearchTextDialog : public SgCmdDialog {
 
 protected:
   static String _defaults[];
   Cmd *_command;
   SgSearchTextCmdInterface *_ci;

   virtual void apply();         // executes a Cmd, not a CmdList

   virtual CmdInterface *createCmdInterface(Widget, Cmd *);

   virtual void setCmdIfDeferredExec() { }
 
 public:
 
   SgSearchTextDialog(const char *, Cmd *);
 
   virtual ~SgSearchTextDialog() { };
 
   virtual CmdInterface *getCmdInterface() { return _ci; }

};
 
#endif
