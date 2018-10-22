/////////////////////////////////////////////////////////////
// ColorMapCmd.h: Include file for radio command class
//                  ColorMapCmd.
/////////////////////////////////////////////////////////////
#ifndef COLORMAPCMD_H
#define COLORMAPCMD_H
#include "RadioCmd.h"
#include "MainWindow.h"
#include <stdint.h>

class ColorMapModes;

class ColorMapCmd : public RadioCmd {
  private:
    Widget _imageWidget;
    unsigned char _colorMapValue;
    ColorMapModes *_colorMapModes;
  protected:
    
    virtual void doit();   

  public:
    
    ColorMapCmd ( const char *, int, unsigned char, Widget, ColorMapModes *,
							CmdList * );
    void setValue (Boolean setVal) { _value = (CmdValue) ((uintptr_t) setVal); newValue(); }
    unsigned char getColorMapMode() { return _colorMapValue; }
    virtual const char *const className () { return "ColorMapCmd"; }
};
#endif
