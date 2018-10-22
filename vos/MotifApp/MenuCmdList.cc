////////////////////////////////////////////////////////////
// MenuCmdList.cc: Maintain a list of Cmd objects and info for each
// describing how they fit in a menu
////////////////////////////////////////////////////////////

#include "MenuCmdList.h"
#include "MenuItem.h"
#include "SeparatorCmd.h"

MenuCmdList::MenuCmdList() : CmdList()
{
   _menuItems = 0;
}

MenuCmdList::MenuCmdList(const char *name ) : CmdList(name)
{
   _menuItems = 0;
}

MenuCmdList::~MenuCmdList()
{
    int i;

    // Free the individual structures

    for ( i = 0; i < size(); i++)
        delete _menuItems[i];

    // free the list itself
    
    delete []_menuItems;
}

void MenuCmdList::addItem ( Cmd *cmd, MenuItem *item )
{
    int i;
    MenuItem **newList;

    // Add the Command to the list first

    CmdList::add(cmd);

    // Now add the MenuItem to the list as well

    // Allocate a list large enough for one more element - _numElements has
    // already been incremented!
    
    newList = new MenuItem*[size()];
    
    // Copy the contents of the previous list to
    // the new list
    
    for( i = 0; i < size()-1; i++)
	newList[i] = _menuItems[i];
    
    // Free the old list
    
    delete []_menuItems;
    
    // Make the new list the current list
    
    _menuItems = newList;
    
    // Add the item to the list
    
    _menuItems[size()-1] = item;
}

void MenuCmdList::add ( Cmd *command, CmdValue )	// Default is a button
{
    addButton(command);
}

void MenuCmdList::addButton ( Cmd *command,
		char mnemonic, char *accel, char *accel_text )
		//   = 0             = 0          = 0
{
    MenuItem *item = new MenuItemButton;
    item->_mnemonic = mnemonic;
    item->_accelerator = accel;
    item->_accel_text = accel_text;

    addItem(command, item);
}

void MenuCmdList::addCheckBox ( Cmd *command,
		char mnemonic, char *accel, char *accel_text )
		//   = 0             = 0          = 0
{
    MenuItem *item = new MenuItemCheckBox;
    item->_mnemonic = mnemonic;
    item->_accelerator = accel;
    item->_accel_text = accel_text;

    addItem(command, item);
}

void MenuCmdList::addOption ( Cmd *command, CmdList *deferList,
						// = 0
		char mnemonic, char *accel, char *accel_text )
		//   = 0             = 0          = 0
{
    MenuItemOption *item = new MenuItemOption;
    item->_mnemonic = mnemonic;
    item->_accelerator = accel;
    item->_accel_text = accel_text;
    item->_deferList = deferList;

    addItem(command, item);
}

void MenuCmdList::addRadioButton ( Cmd *command,
		char mnemonic, char *accel, char *accel_text )
		//   = 0             = 0          = 0
{
    MenuItem *item = new MenuItemRadioButton;
    item->_mnemonic = mnemonic;
    item->_accelerator = accel;
    item->_accel_text = accel_text;

    addItem(command, item);
}

void MenuCmdList::addSubmenu ( Cmd *command,
		char mnemonic, char *accel, char *accel_text )
		//   = 0             = 0          = 0
{
    MenuItem *item = new MenuItemSubmenu;
    item->_mnemonic = mnemonic;
    item->_accelerator = accel;
    item->_accel_text = accel_text;

    addItem(command, item);
}

void MenuCmdList::addSeparator()
{
    MenuItem *item = new MenuItemSeparator;
    item->_mnemonic = 0;
    item->_accelerator = 0;
    item->_accel_text = 0;

    addItem(theSeparatorCmd, item);
}

// Create the menu items in the given menu child

void MenuCmdList::createMenuPane ( Widget menu, Widget option )
						// option = NULL
{
    int i;

    for (i=0; i<size(); i++) {
        _menuItems[i]->createItem ( menu, contents()[i], option );
    }
}

