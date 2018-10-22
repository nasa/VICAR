//////////////////////////////////////////////////////////
// MenuDialog.h: Add a menubar to the features of CustomDialog
//////////////////////////////////////////////////////////
#ifndef MENUDIALOG_H
#define MENUDIALOG_H

#include "CustomDialog.h"

class MenuBar;

class MenuDialog : public CustomDialog {

  protected:
    
    MenuBar *_menuBar;

    virtual void createMenuPanes()=0;
    virtual Widget createBody( Widget );

  public:
    
    MenuDialog(const char *, ButtonState showOk = Default,
			     ButtonState showApply = Visible,
			     ButtonState showReset = Invisible,
			     ButtonState showCancel = Visible,
			     ButtonState showHelp = Visible );

    virtual ~MenuDialog();
    
};
#endif
