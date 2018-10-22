/////////////////////////////////////////////////////////////////
// SgSaveTextWidgetCmd.h - writes the contents of a Motif
// text widget to a file
////////////////////////////////////////////////////////////////

#ifndef SGSAVETEXTWIDGETCMD_H
#define SGSAVETEXTWIDGETCMD_H

#include "NoUndoCmd.h"
#include <Xm/Xm.h>

class Cmd;

class SgSaveTextWidgetCmd: public NoUndoCmd {

  protected:

    Widget _textWidget;

  public:

    SgSaveTextWidgetCmd( const char *, int, Widget );
    virtual void doit();
    virtual const char *const className() { return ("SgSaveTextWidgetCmd"); }
    virtual void freeValue(CmdValue value) 
                                      { if (value) delete [] (char *)value; }
};

#endif
