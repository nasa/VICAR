///////////////////////////////////////////////////////////////////////////////
// SgButtonPanel.h: This is a row-column container that user can stack with 
// commands, each represented by a command interface.
//////////////////////////////////////////////////////////////////////////////
#ifndef SGBUTTONPANEL_H
#define SGBUTTONPANEL_H
#include "UIComponent.h"

class Cmd;
class CmdList;

class SgButtonPanel : public UIComponent {

  public:

    SgButtonPanel(Widget parent, const char *name);
    virtual ~SgButtonPanel() { }

    void addCommands(Cmd *);
    void addOptionMenu(CmdList *);

    virtual const char * const className() { return ("SgButtonPanel"); }

};
#endif
