////////////////////////////////////////////////////////////////
// PostPseudoCmd.h
////////////////////////////////////////////////////////////////
#include "Cmd.h"
#include "MenuDialog.h"

class PostPseudoCmd : public NoUndoCmd {

  protected:

    MenuDialog *_dialog;

  public:

    PostPseudoCmd (const char *name, int active, MenuDialog *dialog) 
		: NoUndoCmd(name, active) { _dialog = dialog; }

    virtual void doit() { _dialog->post(); }
};
