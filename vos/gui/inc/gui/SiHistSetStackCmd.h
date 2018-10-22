/////////////////////////////////////////////////////////////
// SiHistSetStackCmd.h:  
/////////////////////////////////////////////////////////////
#ifndef SiHistSETSTACKCMD_H
#define SiHistSETSTACKCMD_H
#include "SiHistDefs.h"
#include "RadioCmd.h"

class SiHistBox;

class SiHistSetStackCmd : public RadioCmd {

  private:

    MethodType _oldValue;
    SiHistBox *_box;

  protected:

    virtual void doit();   
    virtual void undoit();

  public:    

    SiHistSetStackCmd ( const char *, int, SiHistBox *, CmdList * );

    virtual const char *const className () { return "SiHistSetStackCmd"; }

};
#endif
