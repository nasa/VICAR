//////////////////////////////////////////////////////////////
// PopupMenu.h: A class to assist in creating Popup menus.
// The popup should be constructed with a MenuCmdList.  Then,
// call popup->attachPopup(w) to attach the popup to widget "w"
// via an event handler on button 3.  This is done in a separate
// routine instead of in the constructor in order to 1) allow
// other events to pop up the menu (although that would
// violate the Motif Style Guide, baseWidget() will return
// the widget for the menu), 2) to possibly allow attachment
// to a different widget than the parent (although I don't
// know if that's legal or not), and 3) to allow control
// over when the menu is enabled and disabled (detachPopup()
// will remove the event hander).
// Note that Cmd's for Popup menus cannot be deferred as that
// does not make sense for the way popups are used (it could be
// done if needed, however - it's a matter of not having a clean 
// interface to do so).
///////////////////////////////////////////////////////////////
#include "PopupMenu.h"
#include "MenuCmdList.h"
#include <Xm/RowColumn.h>

PopupMenu::PopupMenu ( Widget parent, const char *name, MenuCmdList *list )
		: UIComponent(name)
{

    _w = XmCreatePopupMenu (parent, (char *)name, NULL, 0);
    installDestroyHandler();
    
    list->createMenuPane(_w);
}

void PopupMenu::attachPopup ( Widget popfrom )
{
    XtAddEventHandler(popfrom, ButtonPressMask, False,
		PopupMenu::popupEventHandler, (XtPointer) this);
}

// The widget ID is needed here in case it's legal to attach the popup
// to more than one widget.  If not, popfrom could be saved in attachPopup()
// and used here instead.

void PopupMenu::detachPopup( Widget popfrom )
{
    XtRemoveEventHandler(popfrom, ButtonPressMask, False,
		PopupMenu::popupEventHandler, (XtPointer) this);
}

// Might as well do all the work here....

void PopupMenu::popupEventHandler ( Widget, XtPointer clientData,
				XEvent *event, Boolean *)
{
    PopupMenu *popup = (PopupMenu *) clientData;
    XButtonPressedEvent *bevent = (XButtonPressedEvent *)event;

    if (event->type != ButtonPress)
        return;

    if (bevent->button != 3)
        return;

    XmMenuPosition(popup->_w, bevent);
    XtManageChild(popup->_w);
}

