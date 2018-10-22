/////////////////////////////////////////////////////////////
// SiHistSetColCmd.h:  Arrange r,g,b SiHistograms in column order.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETCOLCMD_H
#define SiHistSETCOLCMD_H
#include "RadioCmd.h"
#include "SiHistDefs.h"

class SiHistBox;

class SiHistSetColCmd : public RadioCmd {

  private:

    PopupDirectionType _oldPopupDirectionValue;
    MethodType _oldMethodValue;
    SiHistBox *_box;

  protected:

    virtual void doit();   
    virtual void undoit();

  public:

    SiHistSetColCmd ( const char *, int, SiHistBox *, CmdList * );
    virtual const char *const className () { return "SiHistSetColCmd"; }
};
#endif
