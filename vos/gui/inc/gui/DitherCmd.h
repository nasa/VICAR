/////////////////////////////////////////////////////////////
// DitherCmd.h: Include file for radio command class
//                  DitherCmd.
/////////////////////////////////////////////////////////////
#ifndef DITHERCMD_H
#define DITHERCMD_H
#include "RadioCmd.h"
#include "MainWindow.h"
#include <stdint.h>

class ColorMapModes;

class DitherCmd : public RadioCmd {
  private:
    Widget _imageWidget;
    unsigned char _ditherMode;
    ColorMapModes * _colorMapModes;
  protected:
    
    virtual void doit();   

  public:
    
    DitherCmd ( const char *, int, unsigned char, Widget, ColorMapModes * ,
								CmdList *);
    void setValue (Boolean setVal) { _value = (CmdValue)((uintptr_t) setVal); newValue(); }
    unsigned char getDitherMode() { return _ditherMode;  }
    virtual const char *const className () { return "DitherCmd"; }
};
#endif
