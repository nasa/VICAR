////////////////////////////////////////////////////////////
// MenuCmdList.h: Maintain a list of Cmd objects and info for each
// describing how they fit in a menu
////////////////////////////////////////////////////////////

#ifndef MENUCMDLIST_H
#define MENUCMDLIST_H

#include "CmdList.h"
#include <Xm/Xm.h>

class MenuItem;

class MenuCmdList : public CmdList {
    
  private:
    
    MenuItem **_menuItems;    // The list of attributes for menu item display

  protected:

    void addItem ( Cmd *, MenuItem * );

  public:
 
    MenuCmdList();    
    MenuCmdList(const char *);      // Construct an empty list
    virtual ~MenuCmdList();  // Destroys list, but not objects in list
    
    virtual void add( Cmd *, CmdValue=NULL ); // Add a single Cmd object to list
    
    // Functions to add specific menu types

    virtual void addButton ( Cmd *,
		char mnemonic = 0, char *accel = 0, char *accel_text = 0 );
    virtual void addCheckBox ( Cmd *,
		char mnemonic = 0, char *accel = 0, char *accel_text = 0 );
    virtual void addOption ( Cmd *, CmdList * = 0,
		char mnemonic = 0, char *accel = 0, char *accel_text = 0 );
    virtual void addRadioButton ( Cmd *,
		char mnemonic = 0, char *accel = 0, char *accel_text = 0 );
    virtual void addSubmenu ( Cmd *,
		char mnemonic = 0, char *accel = 0, char *accel_text = 0 );
    virtual void addSeparator();	// args are meaningless here

    // Function to actually build the menu pane

    virtual void createMenuPane ( Widget menu, Widget option=NULL );

    virtual const char *const className () { return "MenuCmdList"; }
};

#endif

