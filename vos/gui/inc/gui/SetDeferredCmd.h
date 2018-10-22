/////////////////////////////////////////////////////////////////
// SetDeferredCmd.cc: Switches between immediate and
// deferred execution modes.  This command in intended to
// work with CheckBox widget.  If the CheckBox's value is
// True then deferred execution is set, else immediate
// execution mode is set
////////////////////////////////////////////////////////////////
#ifndef SETDEFERREDCMD_H
#define SETDEFERREDCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>

class SetDeferredCmd : public Cmd {

  protected:

    CmdInterface *_interface;
    CmdList 	 *_applyCmdList;

    Widget *_applyButton;

    virtual void doit();
    virtual void undoit();

  public:

    SetDeferredCmd(const char *name, int active, CmdInterface *, CmdList *,
								Widget*);

    virtual const char *const className() { return ("SetDeferredCmd"); }

};
#endif
