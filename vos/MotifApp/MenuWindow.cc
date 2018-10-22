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
// MenuWindow.C
//////////////////////////////////////////////////////////

#include "MenuWindow.h"
#include "MenuBar.h"

MenuWindow::MenuWindow ( const char *name )  : MainWindow ( name ) 
{
    _menuBar = NULL;
}

void MenuWindow::initialize()
{
    
    // Call base class to create XmMainWindow widget
    // and set up the work area.
    
    MainWindow::initialize();
    
    // Specify the base widget of a MenuBar object 
    // the XmMainWindow widget's menu bar.
    
    _menuBar = new MenuBar ( _main, (char *)"menubar" );
    
    XtVaSetValues ( _main, 
		   XmNmenuBar, _menuBar->baseWidget(),
		   NULL);
    
    // Call derived class hook to add panes to the menu
    
    createMenuPanes();
    
    _menuBar->manage();
}

MenuWindow::~MenuWindow()
{
    delete _menuBar;
}
