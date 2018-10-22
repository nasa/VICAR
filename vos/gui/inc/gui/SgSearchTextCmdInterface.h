//////////////////////////////////////////////////////////////
// SgSearchTextCmdInterface.h: Derived from CmdInterface,
// this class will fill in the SearchCmdValue structure and
// calls the Cmd with filled-in value.
//////////////////////////////////////////////////////////////
#ifndef SGSEARCHTEXTCMDINTERFACE
#define SGSEARCHTEXTCMDINTERFACE

#include "CmdInterface.h"
#include "SgSearchCmdValue.h"
#include "Cmd.h"

class SgSearchTextCmdInterface : public CmdInterface {

  protected:

    SgSearchCmdValue *_searchCmdValue;
  
    Widget _textLabel;                    // label (for the text field)
    Widget _textField;                    // takes user input

    Widget _checkBox;                     // toggles case-sensitivity

    char *_prev_sought_text; 

    virtual void executeCmd(XtPointer = NULL);
                                          // fills in our specialized
                                          // CmdValue and calls runCmd()
   public:
    
     SgSearchTextCmdInterface(Widget, Cmd *);
     virtual ~SgSearchTextCmdInterface() { }
     
     virtual void setValue(CmdValue);    // update the interface
     virtual char *getText();            // fetches & returns widget contents
     virtual int  getCaseSens();         // fetches & returns case sens state
     virtual void triggerCmd();          // allows other entities to execute
                                         // the Cmd
};
#endif
