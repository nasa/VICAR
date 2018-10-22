/////////////////////////////////////////////////////////////
// SetPopUpDirColCmd.h: Example, dummy command class
/////////////////////////////////////////////////////////////
#ifndef SETPOPUPDIRCOLCMD_H
#define SETPOPUPDIRCOLCMD_H
#include "RadioCmd.h"
#include "HistDefs.h"
class HistBox;

class SetPopUpDirColCmd : public RadioCmd {

  private:
    PopupDirectionType _oldPopupDirectionValue;
    MethodType  _oldMethodValue;
    HistBox *_menuView;

  protected:
    virtual void doit();   

  public:
    SetPopUpDirColCmd ( const char *, int, HistBox *, CmdList * );
    virtual const char *const className () { return "SetPopUpDirColCmd"; }

};
#endif
