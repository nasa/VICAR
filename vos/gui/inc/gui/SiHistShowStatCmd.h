/////////////////////////////////////////////////////////////
// SiHistShowStatCmd.h: Show or hide histogram statistics.
/////////////////////////////////////////////////////////////
#ifndef SiHistSHOWSTATCMD_H
#define SiHistSHOWSTATCMD_H
#include "Cmd.h"

class SiHistBox;

class SiHistShowStatCmd : public Cmd {

  private:

    int _oldValue;        // Last valid command for Undo
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit(); 

  public:
    
    SiHistShowStatCmd ( const char *, int, SiHistBox * );
    ~SiHistShowStatCmd() { }

    virtual const char *const className () { return "SiHistShowStatCmd"; }
};
#endif
