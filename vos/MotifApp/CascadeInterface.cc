//////////////////////////////////////////////////////////////
// CascadeInterface.cc: A cascade button interface that hooks to a
// MenuCmdList object.  The Cmd object *MUST* be a MenuCmdList;
// the compiler does not check this!
///////////////////////////////////////////////////////////////
#include "CascadeInterface.h"
#include "MenuCmdList.h"
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>

CascadeInterface::CascadeInterface ( Widget parent, 
				  Cmd *cmd ) : CmdInterface ( cmd )
{
    MenuCmdList *list = (MenuCmdList *)cmd;
    Widget menu;

    menu = XmCreatePulldownMenu (parent, _name, NULL, 0);

    _w = XtVaCreateWidget ( _name, 
			   xmCascadeButtonWidgetClass,
			   parent,
			   XmNsubMenuId, menu,
			   NULL );
    installDestroyHandler();
    
    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
    
    if ( _active )
	activate();     
    else
	deactivate();   

    list->createMenuPane(menu);
}

