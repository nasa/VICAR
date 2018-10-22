/////////////////////////////////////////////////////////////////
// MenuCustomDialog.h: A base class for custom dialogs with MENU BAR.
/////////////////////////////////////////////////////////////////
#include "MenuDialog.h"
#include "MenuBar.h"
#include <Xm/MainW.h>
#include <Xm/PanedW.h>

MenuDialog::MenuDialog( const char *name,
		ButtonState showOk, ButtonState showApply,
		ButtonState showReset,
		ButtonState showCancel, ButtonState showHelp )
	: CustomDialog ( name, showOk, showApply, 
		showReset, showCancel, showHelp )
{
    _menuBar = NULL;
}

MenuDialog::~MenuDialog()
{
    delete _menuBar;
}

Widget MenuDialog::createBody(Widget parent)
{

    Widget main = XtCreateWidget ("mainWindow",
		xmMainWindowWidgetClass, parent,
		NULL, 0 );

    Widget pane = CustomDialog::createBody(main);

    XtVaSetValues ( main,
		XmNworkWindow, pane,
		NULL );

    _menuBar = new MenuBar ( main, (char *)"menubar" );
    XtVaSetValues ( main,
		XmNmenuBar, _menuBar->baseWidget(),
		NULL );
    createMenuPanes();

    _menuBar->manage();
    XtManageChild(main);

    return main;
}

