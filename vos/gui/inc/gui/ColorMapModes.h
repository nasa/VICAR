/////////////////////////////////////////////////////////////
// ColorMapModes.h: Include file for class
//                  ColorMapModes.
/////////////////////////////////////////////////////////////
#ifndef COLORMAPMODES_H
#define COLORMAPMODES_H
#include "MainWindow.h"

class CmdList;
class Cmd;

class ColorMapModes {
  private:
    Widget  _imageWidget;
    CmdList *_ditherRadList;
    CmdList *_colorMapRadList;
    CmdList *_stretchPolicyRadList;
    Cmd     **_ditherCmds;
    Cmd     **_colorMapCmds;
    Cmd     **_stretchPolicyCmds;
    unsigned char _colorMapPolicy, _ditherMode, _stretchPolicy;
    int _redLevels, _greenLevels, _blueLevels, _grayLevels;
    int _allocRedLevels, _allocGreenLevels, _allocBlueLevels, _allocGrayLevels;

    static void modeChangedCallback(Widget, XtPointer, XtPointer);

  protected:
    
    void modeChanged(XtPointer);

  public:
    
    ColorMapModes ( Widget, int, int, int, int );
    void SetColorMapModes();
    void SetColorMapButtons();
    void SetStretchPolicy( unsigned char value ) { _stretchPolicy = value; }
    void addRadLists( CmdList *, CmdList *, CmdList *);
    virtual const char *const className () { return "ColorMapModes"; }
    virtual ~ColorMapModes() {}
};
#endif






