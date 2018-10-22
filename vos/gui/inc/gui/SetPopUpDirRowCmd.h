/////////////////////////////////////////////////////////////
// SetPopUpDirRowCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETPOPUPDIRROWCMD_H
#define SETPOPUPDIRROWCMD_H
#include "HistDefs.h"
#include "RadioCmd.h"

class HistBox;

class SetPopUpDirRowCmd : public RadioCmd {

  private:
    PopupDirectionType _oldPopupDirectionValue;
    MethodType _oldMethodValue;
    HistBox *_menuView;

  protected:
    virtual void doit();   

  public:
    SetPopUpDirRowCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetPopUpDirRowCmd"; }

};
#endif
