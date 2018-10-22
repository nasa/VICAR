////////////////////////////////////////////////////////////////////
// LutWindow.h: LUT Main Window
////////////////////////////////////////////////////////////////////
#ifndef LUTWINDOW_H
#define LUTWINDOW_H
#include "MenuWindow.h"

class LutBox;
class Lut;

class LutWindow : public MainWindow {

  private:

    LutBox *_lutBox;
  
  protected:
    
    Lut *_lutR, *_lutG, *_lutB;

    Widget _form;
    
  public:
    
    Widget createWorkArea ( Widget );
    
    LutWindow ( const char *, Lut*, Lut*, Lut* );
};
#endif
