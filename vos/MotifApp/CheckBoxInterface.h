//////////////////////////////////////////////////////////////
// CheckBoxInterface.h: A check box button interface to a Cmd object
// The value is either TRUE or FALSE.
///////////////////////////////////////////////////////////////
#ifndef CHECKBOXINTERFACE_H
#define CHECKBOXINTERFACE_H
#include "CmdInterface.h"
#include "Cmd.h"

class CheckBoxInterface : public CmdInterface {

  private:

    void doConstruct( Widget );

  protected:

    virtual void executeCmd ( XtPointer );

  public:
    
    CheckBoxInterface ( Widget, Cmd * );
    CheckBoxInterface ( Widget, const char * );
    void setValue ( CmdValue );
};
#endif
