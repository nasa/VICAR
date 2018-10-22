/////////////////////////////////////////////////////////////////
// ZoomCmdSet.cc - creates and maintains all the Zoom commands
////////////////////////////////////////////////////////////////
#include "ZoomCmdSet.h"
#include "BasicImageView.h"
#include "MenuCmdList.h"
#include "CmdList.h"
#include "ZoomCmd.h"
#include "ZoomFitCmd.h"
#include "ZoomConstantCmd.h"
#include "ZoomSpecialCmd.h"
#include "ZoomSpecialCmdList.h"

ZoomCmdSet::ZoomCmdSet(BasicImageView *imageView)
{
   Cmd *cmd;

   _imageView = imageView;

   _menuCmdList = new MenuCmdList("Zoom");
   _radioList = new CmdList;

   _zoomCmd = new ZoomCmd("Zoom", True, imageView);

   // Commands related to the special zoom

   _zoomSpecialCmdList = new ZoomSpecialCmdList("ZoomSpecialCmdList");

   _zoomSpecialCmd = new ZoomSpecialCmd("Special", True, _radioList,
				(CmdValue)False, _zoomSpecialCmdList);
   _menuCmdList->addRadioButton(_zoomSpecialCmd);

   // Normal (constant) zoom commands

   _zoomFitCmd = new ZoomFitCmd("Zoom To Fit", True, _radioList,(CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(_zoomFitCmd);
   cmd = new ZoomConstantCmd("8 x", True, 8, 1, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("4 x", True, 4, 1, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("3 x", True, 3, 1, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("2 x", True, 2, 1, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("None",True, 1, 1, _radioList, (CmdValue)True,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/2", True, 1, 2, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/3", True, 1, 3, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/4", True, 1, 4, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/5", True, 1, 5, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/6", True, 1, 6, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/8", True, 1, 8, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/10",True, 1,10, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);
   cmd = new ZoomConstantCmd("1/16",True, 1,16, _radioList, (CmdValue)False,
					_zoomSpecialCmd, imageView);
   _menuCmdList->addRadioButton(cmd);

}

