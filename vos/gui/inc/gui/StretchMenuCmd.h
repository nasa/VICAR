/////////////////////////////////////////////////////////////
// StretchMenuCmd.h: Include file to handle the STRETCH command
//                button from the Image widget.
/////////////////////////////////////////////////////////////
#ifndef STRETCHMENUCMD_H
#define STRETCHMENUCMD_H
#include "NoUndoCmd.h"

class MainWindow;
class Lut;

class StretchMenuCmd : public NoUndoCmd {

  private:

     int _created;
     MainWindow *_lutWindow;
     Lut *_lutR, *_lutG, *_lutB;

  protected:
    
    virtual void doit();

  public:
    
    StretchMenuCmd ( char*, int, Lut *, Lut *, Lut *);
    virtual const char *const className () { return "StretchMenuCmd"; }
};
#endif
