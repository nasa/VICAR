////////////////////////////////////////////////////////////////////////
//PrefDialog.cc: Source file for creating Preferences Dialog Box
////////////////////////////////////////////////////////////////////////
#include "PrefDialog.h"
#include "XvicBasicImage.h"
#include "CheckBoxInterface.h"
#include "SeparatorInterface.h"
#include "SeparatorCmd.h"
#include "Cmd.h"
#include "CmdList.h"
#include "OptionCmdMenu.h"
#include "DitherCmd.h"
#include "ColorMapCmd.h"
#include "ColorMapModes.h"
#include "LookupTableCmd.h"
#include <Xm/RowColumn.h>
#include "xvmaininc.h"		// for UNUSED


PrefDialog::PrefDialog(const char *name, Cmd *sideBar , Cmd *border,
	Cmd * menuBar, 
	Cmd * fullScreen, Widget imageWidget, BasicImageView *imageView,
	int redLevel, int greenLevel, int blueLevel, int grayLevel)
	: CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{

   _showSideBarCmd = sideBar;
   _showBorderCmd = border;
   _showMenuBarCmd = menuBar;
   _showFullScreenCmd = fullScreen;
   _imageViewWidget = imageWidget;
   _imageView = imageView;
   _CMMRedLevels = redLevel;
   _CMMGreenLevels = greenLevel;
   _CMMBlueLevels = blueLevel;
   _CMMGrayLevels = grayLevel;
}

Widget PrefDialog::createWorkArea(Widget parent)
{
   Widget rc = XtVaCreateWidget("PrefDialog", xmRowColumnWidgetClass, parent,
				XmNorientation, XmVERTICAL, NULL);

//  MAKE COMMANDS FOR THE Preference Dialog
//  Get a pointer to the ColorMapModes class to be used by the Option Menu
//  commands

    ColorMapModes *colorMapModes = new ColorMapModes( _imageViewWidget, 
       _CMMRedLevels, _CMMGreenLevels, _CMMBlueLevels, _CMMGrayLevels);

// MAKE THE COMMAND LIST FOR THE OPTION MENU Dither Mode

    CmdList *ditherList = new CmdList();

    Cmd *UNUSED(ditherNoneCmd) = new DitherCmd( "None", TRUE, XvicNONE, 
			   _imageViewWidget, colorMapModes, ditherList );
    Cmd *UNUSED(ditherOrderedCmd) = new DitherCmd( "Ordered", TRUE, XvicORDERED,
			   _imageViewWidget, colorMapModes, ditherList );
    Cmd *UNUSED(ditherKagelsCmd) = new DitherCmd( "Kagels", TRUE, XvicKAGELS,
			   _imageViewWidget, colorMapModes, ditherList );

// MAKE THE COMMAND LIST FOR THE OPTION MENU ColorMap Policy

    CmdList *colorMapList = new CmdList();

    Cmd *UNUSED(colorMapAllCmd) = new ColorMapCmd( "Full", TRUE, XvicFULL, 
			       _imageViewWidget, colorMapModes, colorMapList );
    Cmd *UNUSED(colorMapHalfCmd) = new ColorMapCmd( "Half", TRUE, XvicHALF,
			       _imageViewWidget, colorMapModes, colorMapList );
    Cmd *UNUSED(colorMapAllocateCmd) = new ColorMapCmd( "Allocate", TRUE, XvicALLOC,
			       _imageViewWidget, colorMapModes, colorMapList );

// MAKE THE COMMAND LIST FOR THE OPTION MENU LOOKUP Table/Stretch Policy

    CmdList *LUTList = new CmdList();

    Cmd *UNUSED(hardwareLUTCmd) = new LookupTableCmd( "H/W Lookup Table", TRUE,
					      XvicUSE_HW, _imageViewWidget,
					      colorMapModes, LUTList);

    Cmd *UNUSED(softwareLUTCmd) = new LookupTableCmd( "S/W Lookup Table", TRUE,
					      XvicUSE_SW, _imageViewWidget,
					      colorMapModes, LUTList);

    colorMapModes->addRadLists( ditherList, colorMapList, LUTList );
    colorMapModes->SetColorMapButtons();



// Create CheckBoxes and Option Menues for dialog

   CmdInterface *ci1 = new CheckBoxInterface(rc, _showFullScreenCmd);
   CmdInterface *ci2 = new CheckBoxInterface(rc, _showBorderCmd);
   CmdInterface *ci3 = new CheckBoxInterface(rc, _showMenuBarCmd);
   CmdInterface *ci4 = new CheckBoxInterface(rc, _showSideBarCmd);
   OptionCmdMenu *ci5 = new OptionCmdMenu(rc, "Dither Mode", ditherList, 
					  _applyCmdList);
   OptionCmdMenu *ci6 = new OptionCmdMenu(rc, "ColorMap Mode", colorMapList, 
					  _applyCmdList);
   OptionCmdMenu *ci7 = new OptionCmdMenu(rc, "LUT Mode", LUTList, 
					  _applyCmdList);

   //  Manage Commands

   ci1->manage();
   ci2->manage();
   ci3->manage();
   ci4->manage();
   ci5->manage();
   ci6->manage();
   ci7->manage();

   //  Set commands to defer command execution until "Apply" button is pressed

   ci1->setDeferredExec(_applyCmdList);
   ci2->setDeferredExec(_applyCmdList);
   ci3->setDeferredExec(_applyCmdList);
   ci4->setDeferredExec(_applyCmdList);

   return rc;
}






