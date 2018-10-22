/////////////////////////////////////////////////////////////
// SiHistSetVerGraphCmd.h: 
/////////////////////////////////////////////////////////////
#ifndef SiHistSETVERGRAPHCMD_H
#define SiHistSETVERGRAPHCMD_H
#include "SiHistDefs.h"
#include "RadioCmd.h"

class SiHistBox;

class SiHistSetVerGraphCmd : public RadioCmd {

  private:

    OrientType _oldValue;
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    SiHistSetVerGraphCmd ( const char *, int, SiHistBox *, CmdList * );

    virtual const char *const className () { return "SiHistSetVerGraphCmd"; }
};
#endif
