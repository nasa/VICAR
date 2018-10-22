///////////////////////////////////////////////////////////
// FullScreenDisplayCmd.cc: Turn off all decorations and make the
// image cover the entire screen.
///////////////////////////////////////////////////////////
#include "FullScreenDisplayCmd.h"
#include "ImageDisplayView.h"

FullScreenDisplayCmd::FullScreenDisplayCmd(const char *name, int active,
		Cmd *borderCmd, Cmd *sideBarCmd, Cmd *menuBarCmd,
		ImageDisplayView *imageView)
	: Cmd(name, active)
{
   // Save the Command's for use later

   _borderCmd = borderCmd;
   _sideBarCmd = sideBarCmd;
   _menuBarCmd = menuBarCmd;
   _imageView = imageView;

   _shell = _imageView->baseWidget();
   if (_shell && !XtIsShell(_shell)) {
      do {
         _shell = XtParent(_shell);
      } while (_shell && !XtIsShell(_shell));
   }

   _borderCmdStoreValue = _borderCmd->getValue();
   _sideBarCmdStoreValue = _sideBarCmd->getValue();
   _menuBarCmdStoreValue = _menuBarCmd->getValue();
   _imageView->getViewSize(_storeViewWidth, _storeViewHeight);
   if (_shell)
      XtVaGetValues(_shell, XmNx, &_storeX, XmNy, &_storeY, NULL);
   _storedValues = False;
}

void FullScreenDisplayCmd::doit()
{
   // Save the current state so we can restore it for Undo

   _borderCmdSaveValue = _borderCmd->getValue();
   _sideBarCmdSaveValue = _sideBarCmd->getValue();
   _menuBarCmdSaveValue = _menuBarCmd->getValue();
   _imageView->getViewSize(_saveViewWidth, _saveViewHeight);
   if (_shell)
      XtVaGetValues(_shell, XmNx, &_saveX, XmNy, &_saveY, NULL);
   _saveStoredValues = _storedValues;

   // Execute the three Commands to turn on or off all decorations

   if (_value) {

      // Save the current state for when we turn this mode off (note: this
      // is different from undo!)

      if (!_storedValues) {
         _borderCmdStoreValue = _borderCmd->getValue();
         _sideBarCmdStoreValue = _sideBarCmd->getValue();
         _menuBarCmdStoreValue = _menuBarCmd->getValue();
         _imageView->getViewSize(_storeViewWidth, _storeViewHeight);
         if (_shell)
            XtVaGetValues(_shell, XmNx, &_storeX, XmNy, &_storeY, NULL);
         _storedValues = True;
      }

      // Turn everything off

      _borderCmd->execute((CmdValue) False);
      _sideBarCmd->execute((CmdValue) False);
      _menuBarCmd->execute((CmdValue) False);

      // Now set the display's view size and shell position

      _imageView->setViewSize(
		XDisplayWidth(XtDisplay(_imageView->getWidget()),
			    XDefaultScreen(XtDisplay(_imageView->getWidget()))),
		XDisplayHeight(XtDisplay(_imageView->getWidget()),
			    XDefaultScreen(XtDisplay(_imageView->getWidget()))),
		False);				// No scrollbars
      if (_shell)
         XtVaSetValues(_shell, XmNx, 0, XmNy, 0, NULL);
   }

   else {
      _borderCmd->execute(_borderCmdStoreValue);
      _sideBarCmd->execute(_sideBarCmdStoreValue);
      _menuBarCmd->execute(_menuBarCmdStoreValue);

      // Set scrollbars if needed
      _imageView->setViewSize(_storeViewWidth, _storeViewHeight, True);
      if (_shell)
         XtVaSetValues(_shell, XmNx, _storeX, XmNy, _storeY, NULL);

      _storedValues = False;
   }
}      

void FullScreenDisplayCmd::undoit()
{

   // Undo the command to the last state

   _borderCmd->execute(_borderCmdSaveValue);
   _sideBarCmd->execute(_sideBarCmdSaveValue);
   _menuBarCmd->execute(_menuBarCmdSaveValue);

   // Set scrollbars if needed
   _imageView->setViewSize(_saveViewWidth, _saveViewHeight, True);
   if (_shell)
      XtVaSetValues(_shell, XmNx, _saveX, XmNy, _saveY, NULL);

   _storedValues = _saveStoredValues;

   if (!_borderCmdSaveValue && !_sideBarCmdSaveValue && !_menuBarCmdSaveValue)
      _value = (CmdValue) True;
   else
      _value = (CmdValue) False;
   newValue();

}

