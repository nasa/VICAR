///////////////////////////////////////////////////////////////
// SpikeDialogCmd.h: Include file to define class for displaying
//                  Spike Dialog Box for XVICDISP widget
///////////////////////////////////////////////////////////////
#ifndef SPIKEDIALOGCMD_H
#define SPIKEDIALOGCMD_H
#include "Cmd.h"
#include "CustomDialog.h"
#include "NoUndoCmd.h"

class BasicImageView;

class SpikeDialogCmd : public NoUndoCmd {
  private:
    CustomDialog *_dialog;
  protected:
    
    virtual void doit() { _dialog->post(); };   

  public:
    
    SpikeDialogCmd ( const char *name , int active, CustomDialog *dialog ) : NoUndoCmd( name, active) { _dialog = dialog; };
    virtual const char *const className () { return "SpikeDialogCmd"; }
};
#endif
