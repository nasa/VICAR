/////////////////////////////////////////////////////////////
// SiHistSetDescAxisCmd.h:  Set descending orientation on histogram 
// axis.
/////////////////////////////////////////////////////////////
#ifndef SiHistSETDESCAXISCMD_H
#define SiHistSETDESCAXISCMD_H
#include "SiHistDefs.h"
#include "RadioCmd.h"

class SiHistBox;

class SiHistSetDescAxisCmd : public RadioCmd {

  private:

    VerAxisDirType _oldValue;
    SiHistBox *_box;

  protected:
    
    virtual void doit();   
    virtual void undoit();

  public:
    
    SiHistSetDescAxisCmd ( const char *, int, SiHistBox *, CmdList * );
    ~SiHistSetDescAxisCmd() { }

    virtual const char *const className () { return "SiHistSetDescAxisCmd"; }
};
#endif
