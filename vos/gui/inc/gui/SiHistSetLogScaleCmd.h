/////////////////////////////////////////////////////////////
// SiHistSetLogScaleCmd.h: Displays histogram using logarithmic scale.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETLOGSCALECMD_H
#define SiHistSETLOGSCALECMD_H
#include <Xm/Xm.h>
#include "Cmd.h"

class SiHistBox;

class SiHistSetLogScaleCmd : public Cmd {

  private:

    Boolean _oldValue;
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:

    SiHistSetLogScaleCmd ( const char *, int, SiHistBox * );
    ~SiHistSetLogScaleCmd() { }

    virtual const char *const className () { return "SiHistLogScaleCmd"; }
};
#endif
