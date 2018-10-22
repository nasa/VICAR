/////////////////////////////////////////////////////////////
// LutMenuCmd.h: LUT command class for displaying the current
//               lookup table.
/////////////////////////////////////////////////////////////
#ifndef LUTMENUCMD_H
#define LUTMENUCMD_H
#include "NoUndoCmd.h"

class MainWindow;
class Lut;

class LutMenuCmd : public NoUndoCmd {

  private:

     int _created;
     MainWindow *_lutWindow;
     Lut *_lutR, *_lutG, *_lutB;

  protected:
    
    virtual void doit();

  public:
    
    LutMenuCmd ( const char *, int, Lut*, Lut*, Lut*);

    virtual const char *const className () { return "LutMenuCmd"; }
};
#endif
