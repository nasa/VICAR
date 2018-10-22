//////////////////////////////////////////////////////////////
// OptionInterface.h: A push button interface to a Cmd object intended
// for use within an Option menu.  Note that the widget ID of the
// option menu itself must be provided so that setValue can tell
// the option menu what the new setting is (there appears to be no
// way to get the option menu given one of the pushbuttons!).
///////////////////////////////////////////////////////////////
#ifndef OPTIONINTERFACE
#define OPTIONINTERFACE
#include "CmdInterface.h"

class OptionInterface : public CmdInterface {
    
  private:

    void doConstruct ( Widget );

  protected:

    Widget _option;

    virtual void executeCmd ( XtPointer );

  public:
    
    OptionInterface ( Widget, Cmd *, Widget );
    OptionInterface ( Widget, const char *, Widget );

    virtual void setValue ( CmdValue );
};
#endif
