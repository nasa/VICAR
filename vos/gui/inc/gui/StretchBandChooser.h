//////////////////////////////////////////////////////////////
// StretchRadioBtn.h: A component class to choose which band will be 
// stretched.  The choices are Red, Green, Blue, All.  Only one option
// can be chosen.
/////////////////////////////////////////////////////////////
#ifndef STRETCHBANDCHOOSER_H
#define STRETCHBANDCHOOSER_H
#include "CmdInterface.h"
#include "StretchValue.h"

class StretchCmdInterface;

class StretchBandChooser : public CmdInterface {

  private:

    static void valueChangedCallback(Widget, XtPointer, XtPointer);
    
  protected:
    
    Widget _all, _red, _grn, _blu;
    
  public:
    
    StretchBandChooser(Widget, Cmd *);
    virtual ~StretchBandChooser() { }
    
    virtual void valueChanged();

    virtual void setValue(CmdValue);

    virtual const char *const className() { return "StretchBandChooser"; }
};
#endif

