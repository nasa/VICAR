/////////////////////////////////////////////////////////////
// SetAscAxisCmd.h:  Display histogram using ascending axis.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETASCAXISCMD_H
#define SiHistSETASCAXISCMD_H
#include "SiHistDefs.h"
#include "RadioCmd.h"

class SiHistBox;

class SiHistSetAscAxisCmd : public RadioCmd {

  private:

    VerAxisDirType _oldValue;
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit();

  public:
    
    SiHistSetAscAxisCmd ( const char *, int, SiHistBox *, CmdList * );
    ~SiHistSetAscAxisCmd() { }

    virtual const char *const className () { return "SiHistSetAscAxisCmd"; }
};
#endif
