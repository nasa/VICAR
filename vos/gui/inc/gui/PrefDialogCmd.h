///////////////////////////////////////////////////////////////
// PrefDialogCmd.h: Include file to define class for displaying
//                  Preferences Dialog Box for XVICDISP widget
///////////////////////////////////////////////////////////////
#ifndef PREFDIALOGCMD_H
#define PREFDIALOGCMD_H
#include "Cmd.h"
#include "CustomDialog.h"

class BasicImageView;

class PrefDialogCmd : public NoUndoCmd {
  private:
    CustomDialog *_dialog;
  protected:
    
    virtual void doit() { _dialog->post(); };   

  public:
    
    PrefDialogCmd ( const char *name , int active, CustomDialog *dialog )
		: NoUndoCmd( name, active) { _dialog = dialog; };
    virtual const char *const className () { return "PrefDialogCmd"; }
};
#endif
