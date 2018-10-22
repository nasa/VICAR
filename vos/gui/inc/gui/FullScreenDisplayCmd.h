/////////////////////////////////////////////////////////////
// FullScreenDisplayCmd.h: Turn off all decorations and make the
// image cover the entire screen.
/////////////////////////////////////////////////////////////
#ifndef FULLSCREENDISPLAYCMD_H
#define FULLSCREENDISPLAYCMD_H
#include "Cmd.h"
#include <X11/Intrinsic.h>		// for Dimension and Boolean only

class ImageDisplayView;

class FullScreenDisplayCmd : public Cmd {
 private:
   Cmd *_borderCmd;
   Cmd *_sideBarCmd;
   Cmd *_menuBarCmd;
   ImageDisplayView *_imageView;
   Widget _shell;

 protected:

   // The saved values are for use with Undo

   CmdValue _borderCmdSaveValue;
   CmdValue _sideBarCmdSaveValue;
   CmdValue _menuBarCmdSaveValue;
   Dimension _saveViewWidth, _saveViewHeight;
   Position _saveX, _saveY;
   Boolean _saveStoredValues;

   // The stored values are for executing with False.  We want to set it
   // back to what it was before the command was executed with True, regardless
   // of how many times it was executed with True.  So, undo isn't sufficient
   // and we must store the values separately.

   CmdValue _borderCmdStoreValue;
   CmdValue _sideBarCmdStoreValue;
   CmdValue _menuBarCmdStoreValue;
   Dimension _storeViewWidth, _storeViewHeight;
   Position _storeX, _storeY;
   Boolean _storedValues;	// True if we've stored something significant

   virtual void doit();   
   virtual void undoit(); 

 public:

   FullScreenDisplayCmd(const char *, int, Cmd *, Cmd *, Cmd *,
							ImageDisplayView *);
   virtual const char *const className () { return "FullScreenDisplayCmd"; }
};
#endif

