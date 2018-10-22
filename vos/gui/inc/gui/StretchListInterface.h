//////////////////////////////////////////////////////////////
// StretchListInterface.h: Fills list with values 
//////////////////////////////////////////////////////////////
#ifndef STRETCHLISTINTERFACE_H
#define STRETCHLISTINTERFACE_H
#include "CmdInterface.h"
#include "StretchValue.h" // For StretchType definition

class ListControl;

class StretchListInterface : public CmdInterface {

  protected:
    
    int _defValue;
    ListControl *_control;
    StretchType _stretchType;

  public:
    
    StretchListInterface ( Widget, Cmd *, StretchType, int defValue=0);
    
    virtual void setValue(CmdValue);
    
    void runIt();

};
#endif
