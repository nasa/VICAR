//////////////////////////////////////////////////////////////
// StretchPercValuesIf.h: Shows current percent stretch limits.
//////////////////////////////////////////////////////////////
#ifndef STRETCHPERCVALUESIF_H
#define STRETCHPERCVALUESIF_H
#include "CmdInterface.h"

class KeyinView;

class StretchPercValuesIf : public CmdInterface {

  private:

    KeyinView *_lPercValueRed, *_lPercValueGrn, *_lPercValueBlu;
    KeyinView *_hPercValueRed, *_hPercValueGrn, *_hPercValueBlu;

  public:
    
    StretchPercValuesIf(Widget, Cmd *);

    virtual void setValue(CmdValue);

};
#endif
