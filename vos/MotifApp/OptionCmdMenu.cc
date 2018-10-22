//////////////////////////////////////////////////////////////
// OptionCmdMenu.h: An option menu component that hooks to a
// CmdList or MenuCmdList.  All commands in the list should be
// RadioCmd's in order for things to work correctly.  If a
// MenuCmdList is used, separators may also be used, along with
// setting mnemonics and accelerators, although the RadioCmd's
// must be added using addOption() rather than addRadioButton().
// If a straight CmdList is used, then all items of the list must
// be RadioCmd's and mnemonics or accelerators must be set via the
// resource file.  The CmdList form is convenient because it allows
// the use of the already-existing CmdList used to specify the
// mutual-exclude of the RadioCmd's.
///////////////////////////////////////////////////////////////
#include "OptionCmdMenu.h"
#include "MenuCmdList.h"
#include "OptionInterface.h"
#include <Xm/RowColumn.h>

// To set deferred execution using the MenuCmdList constructor, the defer
// list must be specified in addOption() on the MenuCmdList.

OptionCmdMenu::OptionCmdMenu ( Widget parent, const char *name,
			MenuCmdList *menuCmdList )
		: UIComponent(name)
{
    Widget menu;
    Arg args[5];
    int n = 0;

    menu = XmCreatePulldownMenu (parent, (char *)"option_pane", NULL, 0);

    XtSetArg(args[n], XmNsubMenuId, menu); n++;

    _w = XmCreateOptionMenu(parent, (char *)name, args, n);
    installDestroyHandler();
    
    menuCmdList->createMenuPane(menu, _w);
}

OptionCmdMenu::OptionCmdMenu ( Widget parent, const char *name,
			CmdList *cmdList, CmdList *deferList )
		: UIComponent(name)
{
    Widget menu;
    Arg args[5];
    int n = 0;
    CmdInterface *ci;

    menu = XmCreatePulldownMenu (parent, (char *)"option_pane", NULL, 0);

    XtSetArg(args[n], XmNsubMenuId, menu); n++;

    _w = XmCreateOptionMenu(parent, (char *)name, args, n);
    installDestroyHandler();
    
    for (int i=0; i < cmdList->size(); i++) {
        ci = new OptionInterface(menu, (*cmdList)[i], _w);
        ci->manage();
        if (deferList)
            ci->setDeferredExec(deferList);
    }
}

