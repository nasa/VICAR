//////////////////////////////////////////////////////////////
// SgIntKeyinInterface.h: An integer keyin interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef SGINTKEINTERFACE
#define SGINTKEINTERFACE
#include "CmdInterface.h"

class KeyinView;

class SgIntKeyinInterface : public CmdInterface {

  protected:

    KeyinView *_keyin;
    
    virtual void executeCmd(XtPointer);
    virtual void setValue(CmdValue);

  public:

    SgIntKeyinInterface ( Widget, Cmd * );
    virtual ~SgIntKeyinInterface();
};
#endif
