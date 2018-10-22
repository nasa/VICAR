///////////////////////////////////////////////////////////
// PseudoCmd.h: Command class to execute the PSEUDOCOLOR
//                command.
//////////////////////////////////////////////////////////
#ifndef PSEUDOCMD_H
#define PSEUDOCMD_H
#include "NoUndoCmd.h"

class ImageWindow;
class Lut;

class PseudoCmd : public NoUndoCmd {

  protected:

    int _created;

    Lut *_lutR, *_lutG, *_lutB;

  public:

    PseudoCmd(const char *, int, Lut *, Lut *, Lut * );

    virtual void doit();

    virtual void freeValue(CmdValue);
};
#endif
