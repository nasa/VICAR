/////////////////////////////////////////////////////////////
// LookupTableCmd.h: Include file for radio command class
//                  LookupTableCmd.
/////////////////////////////////////////////////////////////
#ifndef LOOKUPTABLECMD_H
#define LOOKUPTABLECMD_H
#include "RadioCmd.h"
#include "MainWindow.h"
#include <stdint.h>

class ColorMapModes;

class LookupTableCmd : public RadioCmd {
  private:
    Widget _imageWidget;
    unsigned char _LUTMode;
    ColorMapModes *_colorMapModes;
  protected:
    
    virtual void doit();   

  public:
    
    LookupTableCmd ( const char *, int, unsigned char, Widget, ColorMapModes *,
								CmdList *);
    void setValue (Boolean setVal) { _value = (CmdValue)((uintptr_t) setVal); newValue(); }
    virtual const char *const className () { return "LookupTableCmd"; }
};
#endif
