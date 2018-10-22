///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//         This example code is from the book:
//
//           Object-Oriented Programming with C++ and OSF/Motif
//         by
//           Douglas Young
//           Prentice Hall, 1992
//           ISBN 0-13-630252-1	
//
//         Copyright 1991 by Prentice Hall
//         All Rights Reserved
//
//  Permission to use, copy, modify, and distribute this software for 
//  any purpose except publication and without fee is hereby granted, provided 
//  that the above copyright notice appear in all copies of the software.
///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////
// MenuBar.C: A menu bar whose panes support items
//            that execute Cmd's
//////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////
// MODIFIED TO SUPPORT SUBMENUS - not described in Book
///////////////////////////////////////////////////////////


#include "MenuBar.h"
#include "Cmd.h"
#include "CmdList.h"
#include "ButtonInterface.h"
#include "SeparatorInterface.h"
#include "CascadeInterface.h"
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>

MenuBar::MenuBar ( Widget parent, const char *name ) : UIComponent ( name )
{
    // Base widget is a Motif menu bar widget
    
    _w = XmCreateMenuBar ( parent, _name, NULL, 0 );
    
    installDestroyHandler();
}

void MenuBar::addCommands ( CmdList *list, int help_menu )
					    // help_menu = FALSE
{
    createPulldown ( _w, list, help_menu );
}

void MenuBar::addCommands ( MenuCmdList *list, int help_menu )
						// help_menu = FALSE
{
    CmdInterface *ci;
    ci = new CascadeInterface( _w, (Cmd *)list );
    if (help_menu)
        XtVaSetValues ( _w, XmNmenuHelpWidget, ci->baseWidget(), NULL );
    ci->manage();
}


void MenuBar::createPulldown ( Widget parent, CmdList *list, int help_menu )
							  // help_menu = FALSE
{
    int    i;
    Widget pulldown, cascade;
    
    // Create a pulldown menu pane for this list of commands
    
    pulldown = XmCreatePulldownMenu ( parent, (char *)list->name(), NULL, 0 );
    
    // Each entry in the menu bar must have a cascade button
    // from which the user can pull down the pane
    
    cascade = XtVaCreateWidget ( list->name(), 
				xmCascadeButtonWidgetClass,
				parent, 
				XmNsubMenuId, pulldown, 
				NULL );
    XtManageChild ( cascade );
    
    if (help_menu)
        XtVaSetValues ( parent, XmNmenuHelpWidget, cascade, NULL );

    // Loop through the cmdList, creating a menu 
    // entry for each command. 
    
    for ( i = 0; i < list->size(); i++)
    {
	if(!strcmp((*list)[i]->className(), "CmdList"))
	{
	    createPulldown( pulldown, (CmdList*) (*list)[i], FALSE);
	}
        else if (!strcmp((*list)[i]->className(), "Separator"))
        {
            SeparatorInterface *si;
            si = new SeparatorInterface ( pulldown, (*list)[i] );
            si->manage();
        }
	else
	{
	    CmdInterface *ci;
	    ci  = new ButtonInterface ( pulldown, (*list)[i] );
	    ci->manage();
	}
    }
}
