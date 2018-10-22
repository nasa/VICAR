/////////////////////////////////////////////////////////////
// SiHistSetRowCmd.h:  Arrange r,g,b histograms in row order.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETROWCMD_H
#define SiHistSETROWCMD_H
#include "RadioCmd.h"
#include "SiHistDefs.h"

class SiHistBox;

class SiHistSetRowCmd : public RadioCmd {

  private:

    PopupDirectionType _oldPopupDirectionValue;
    MethodType _oldMethodValue;
    SiHistBox *_box;

  protected:

    virtual void doit();   
    virtual void undoit();

  public:

    SiHistSetRowCmd ( const char *, int, SiHistBox *, CmdList * );

    virtual const char *const className () { return "SiHistSetRowCmd"; }
};
#endif
