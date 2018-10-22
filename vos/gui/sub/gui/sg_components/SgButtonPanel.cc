///////////////////////////////////////////////////////////////////////////////
// SgButtonPanel.h: This is a row-column container that user can stack with
// commands, each represented by a command interface.
//////////////////////////////////////////////////////////////////////////////
#include "SgButtonPanel.h"
#include "ButtonInterface.h"
#include "OptionCmdMenu.h"
#include <Xm/RowColumn.h>

SgButtonPanel::SgButtonPanel(Widget parent, const char *name)
    : UIComponent(name)
{
    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent, 
			  XmNpacking, XmPACK_TIGHT,
			  NULL);
    installDestroyHandler();
}

void SgButtonPanel::addCommands(Cmd *cmd)
{
    ButtonInterface *bi = new ButtonInterface(_w, cmd);
    bi->manage();
}

void SgButtonPanel::addOptionMenu(CmdList *cmdList)
{
    UIComponent *optionMenu = new OptionCmdMenu(_w, "option", cmdList);
    optionMenu->manage();
}
