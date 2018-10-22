/////////////////////////////////////////////////////////////
// SiHistShowAxisCmd.h: Displays or hides histogram axis.
/////////////////////////////////////////////////////////////
#ifndef SiHistSHOWAXISCMD_H
#define SiHistSHOWAXISCMD_H
#include "Cmd.h"
#include <X11/Intrinsic.h>  // For definition of Boolean

class SiHistBox;

class SiHistShowAxisCmd : public Cmd {

  private:

    Boolean _oldValue;
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    SiHistShowAxisCmd ( const char *, int, SiHistBox * );

    virtual const char *const className () { return "SiHistShowAxisCmd"; }
};
#endif
