/////////////////////////////////////////////////////////////
// SiHistSetBlendCmd.h:  Display histogram in blend mode.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETBLENDCMD_H
#define SiHistSETBLENDCMD_H
#include "SiHistDefs.h"
#include "RadioCmd.h"

class SiHistBox;

class SiHistSetBlendCmd : public RadioCmd {

  private:

    MethodType _oldValue;
    SiHistBox *_box;

  protected:

    virtual void doit();   
    virtual void undoit();

  public:

    SiHistSetBlendCmd ( const char *, int, SiHistBox *, CmdList * );

    virtual const char *const className () { return "SiHistSetBlendCmd"; }
};
#endif
