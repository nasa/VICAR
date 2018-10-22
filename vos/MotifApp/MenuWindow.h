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


//////////////////////////////////////////////////////////////
// MenuWindow.C: Add a menubar to the features of MainWindow
//////////////////////////////////////////////////////////////

#ifndef MENUWINDOW_H
#define MENUWINDOW_H

#include "MainWindow.h"
#include "MenuBar.h"

class MenuWindow : public MainWindow {
    
  protected:
    
    MenuBar *_menuBar;
    
    virtual void createMenuPanes() = 0;  // Defined by derived
                                         // classes to specify the
                                         // contents of the menu

#ifndef CPLUSPLUS2_1
    virtual Widget createWorkArea ( Widget ) = 0;
#endif

  public:
    
    MenuWindow ( const char *name ); 
    virtual ~MenuWindow();

    virtual void hideMenuBar()
	{ if (_menuBar) _menuBar->unmanage(); }
    virtual void showMenuBar()
	{ if (_menuBar) _menuBar->manage(); }
    virtual Boolean isMenuBarVisible()
	{ if (_menuBar) return _menuBar->isManaged(); else return False; }

    virtual void initialize();           // Called by Application
};
#endif

