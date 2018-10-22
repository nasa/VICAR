///////////////////////////////////////////////////////////////
//
//   ClearLabelCmd.h: 
//
//   This is a class derived from NoUndoCmd.
//   It clears image label display.
//
///////////////////////////////////////////////////////////////
#ifndef CLEARLABELCMD_H
#define CLEARLABELCMD_H
#include "NoUndoCmd.h"

class TextDisplayModel;

class ClearLabelCmd : public NoUndoCmd {

  protected:

    TextDisplayModel *_textM;

    virtual void doit();

  public:

    ClearLabelCmd( const char *, int, TextDisplayModel *textM );

    virtual const char *const className() { return ("ClearLabelCmd"); }

};
#endif
