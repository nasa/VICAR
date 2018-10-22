///////////////////////////////////////////////////////////////////////////////
// TpSetCursorSymbolCmd.h: This command class allow user to set cursor symbol 
// to one of the predefined shapes.
///////////////////////////////////////////////////////////////////////////////
#ifndef TPSETCURSORSYMBOLCMD_H
#define TPSETCURSORSYMBOLCMD_H
#include "RadioCmd.h"
#include <UIComponent.h>

class TpDisplayer;

class TpSetCursorSymbolCmd : public RadioCmd {

  protected:

    TpDisplayer *_displayer;
    String _cursor;

    void doit();
    void undoit() { }

  public:

    TpSetCursorSymbolCmd(const char *name, int active, CmdList *, TpDisplayer *);
    virtual ~TpSetCursorSymbolCmd() { }

    String getCursor() { return _cursor; }

    virtual const char *const className () { return "TpSetCursorSymbolCmd"; }
};

#endif
