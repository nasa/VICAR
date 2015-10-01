////////////////////////////////////////////////////////
// PrintWidgetTreeCmd.h: Debugging command used to print out the
// names and classes of a widget and all its ancestors.
////////////////////////////////////////////////////////
#ifndef PRINTWIDGETTREECMD_H
#define PRINTWIDGETTREECMD_H
#include "NoUndoCmd.h"
#include <Xm/Xm.h>

class PrintWidgetTreeCmd : public NoUndoCmd {

  private:

  protected:

    Widget _baseW;		// Base widget to use for XmTrackingEvent()

    virtual void doit();

    int    _post_to_dialog;    // if true post a popup

  public:

    PrintWidgetTreeCmd(const char*, int, Widget);
    PrintWidgetTreeCmd(const char*, int, int, Widget);
    virtual const char *const className () { return "PrintWidgetTreeCmd"; }
};
#endif
