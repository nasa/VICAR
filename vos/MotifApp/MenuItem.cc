////////////////////////////////////////////////////////////
// MenuItem.cc: A collection of small classes that maintain info about
// how a menu item is presented, and create the items
////////////////////////////////////////////////////////////

#include <Xm/Xm.h>
#include <Xm/Separator.h>
#include "MenuItem.h"
#include "Cmd.h"
#include "ButtonInterface.h"
#include "CheckBoxInterface.h"
#include "RadioButtonInterface.h"
#include "CascadeInterface.h"
#include "OptionInterface.h"

// Set the mnemonic and accelerator on the widget
// Note that these should rarely be used.  It is much better to set
// mnemonics, accelerators, and labels in the resource file so that
// they can be internationalized.

void MenuItem::setMneAcc ( Widget w )
{
    XmString str;
    if (_mnemonic)
        XtVaSetValues(w, XmNmnemonic, _mnemonic, NULL);

    if (_accelerator) {
#if (XmVERSION==1 && XmREVISON<=1)
        str = XmStringCreateSimple(_accel_text);
#else					// Motif 1.2 or later
        str = XmStringCreateLocalized(_accel_text);
#endif
        XtVaSetValues(w, XmNaccelerator, _accelerator,
			 XmNacceleratorText, str,
			 NULL);
        XmStringFree(str);
    }
}

void MenuItemButton::createItem ( Widget parent, Cmd *command, Widget )
{
    CmdInterface *ci = new ButtonInterface(parent, command);
    setMneAcc(ci->baseWidget());
    ci->manage();
}

void MenuItemCheckBox::createItem ( Widget parent, Cmd *command, Widget )
{
    CmdInterface *ci = new CheckBoxInterface(parent, command);
    setMneAcc(ci->baseWidget());
    ci->manage();
}

void MenuItemOption::createItem ( Widget parent, Cmd *command, Widget option )
{
    CmdInterface *ci = new OptionInterface(parent, command, option);
    setMneAcc(ci->baseWidget());
    ci->manage();
    if (_deferList)
        ci->setDeferredExec(_deferList);
}

void MenuItemRadioButton::createItem ( Widget parent, Cmd *command, Widget )
{
    CmdInterface *ci = new RadioButtonInterface(parent, command);
    setMneAcc(ci->baseWidget());
    ci->manage();
}

void MenuItemSubmenu::createItem ( Widget parent, Cmd *command, Widget )
{
    CmdInterface *ci = new CascadeInterface(parent, command);
    setMneAcc(ci->baseWidget());
    ci->manage();
}

void MenuItemSeparator::createItem ( Widget parent, Cmd *, Widget )
{
    XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, parent, NULL);
}

