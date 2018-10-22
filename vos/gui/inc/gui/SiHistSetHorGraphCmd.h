/////////////////////////////////////////////////////////////
// SiHistSetHorGraphCmd.h:  Set horizontal orientation on hstogram 
// plot.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETHORGRAPHCMD_H
#define SiHistSETHORGRAPHCMD_H
#include "SiHistDefs.h"
#include "RadioCmd.h"

class SiHistBox;

class SiHistSetHorGraphCmd : public RadioCmd {

  private:

    OrientType _oldValue;
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:

    SiHistSetHorGraphCmd ( const char *, int, SiHistBox *, CmdList * );

    virtual const char *const className() { return "SiHistSetHorGraphCmd"; }
};
#endif
