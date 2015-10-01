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

#ifndef POPUPMENU
#define POPUPMENU
#include "UIComponent.h"

class MenuCmdList;

class PopupMenu : public UIComponent {

  protected:

    static void popupEventHandler ( Widget, XtPointer, XEvent *, Boolean * );

  public:
    
    PopupMenu ( Widget, const char *name, MenuCmdList * );

    virtual void attachPopup ( Widget );
    virtual void detachPopup ( Widget );

    Widget menu() { return _w; }
};
#endif
