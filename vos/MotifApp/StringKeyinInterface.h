////////////////////////////////////////////////////////////////
// StringKeyinInterface.h: A TextField interface to a command that takes
// a dynamically allocated string (freed via XtFree()) as its value.
////////////////////////////////////////////////////////////////
#ifndef STRINGKEYININTERFACE_H
#define STRINGKEYININTERFACE_H

#include "CmdInterface.h"

class StringKeyinInterface : public CmdInterface {
  protected:
    char *_oldValue;		// Avoid repeat execs if string doesn't change

    virtual void copyOldValue(char *string);

    virtual void executeCmd(XtPointer = NULL);

  public:

    StringKeyinInterface(Widget, Cmd *);

    virtual void setValue(CmdValue);

    virtual void setValueAndRun(CmdValue);

};
#endif
