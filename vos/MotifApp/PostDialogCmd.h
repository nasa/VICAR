/////////////////////////////////////////////////////////////
// PostDialogCmd.h:  Command to post custom or menu dialog
/////////////////////////////////////////////////////////////
#ifndef POSTDIALOGCMD_H
#define POSTDIALOGCMD_H
#include "NoUndoCmd.h"
#include "CustomDialog.h"

class PostDialogCmd : public NoUndoCmd {

  private:

    CustomDialog *_dialog;

  protected:
    
    virtual void doit() 
	{ if (_dialog) _dialog->post(); }

  public:

    PostDialogCmd ( const char *name, int active, CustomDialog *dialog) 
		: NoUndoCmd(name, active) 
	{ _dialog = dialog; }

    virtual ~PostDialogCmd() { }

    virtual const char *const className () { return "PostDialogCmd"; }
};
#endif

