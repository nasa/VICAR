//////////////////////////////////////////////////////////////////////
// StretchDialog.cc: This class creates a work area for stretch dialog.
//////////////////////////////////////////////////////////////////////
#include "StretchDialog.h"
#include "StretchCmdInterface.h"
#include "StretchBandChooser.h"
#include "PostSingleFileDialogCmd.h"
#include "LoadLutFileCmd.h"
#include "SaveLutFileCmd.h"
#include "StretchPercValuesDialog.h"
#include "PostDialogCmd.h"
#include "UndoCmd.h"
#include "HelpOnContextCmd.h"
#include "HelpSelfCmd.h"
#include "Lut.h"
#include "Cmd.h"
#include "MenuCmdList.h"
#include "MenuBar.h"
#include <Xm/RowColumn.h>

StretchDialog::StretchDialog(const char *name, Cmd *cmd, 
			     Lut *lutR, Lut *lutG, Lut *lutB)
    : MenuDialog(name, Default, Invisible, Invisible, Invisible, Visible)
{
    _cmd = cmd;

    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;
}

Widget StretchDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("StretchDialogRC", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 NULL);
    CmdInterface *ci = new StretchCmdInterface(rc, _cmd);
    CmdInterface *bandChooser = new StretchBandChooser(rc, _cmd);

    ci->manage();
    bandChooser->manage();

    return rc;
}

void StretchDialog::createMenuPanes()
{
   MenuCmdList *cmdList;
 
   ////////
   // CREATE FILE PULLDOWN
   ////////
 
   cmdList = new MenuCmdList("File");
 
   Cmd *loadFileCmd = new LoadLutFileCmd("loadFile", True, _cmd);
   Cmd *loadWinFileCmd = new PostSingleFileDialogCmd("load", True, 
						     loadFileCmd);
   cmdList->add(loadWinFileCmd);

   Cmd *saveFileCmd = new SaveLutFileCmd ("saveFile", True, 
					  _lutR, _lutG, _lutB);
   Cmd *saveWinFileCmd = new PostSingleFileDialogCmd ("save", True, 
						      saveFileCmd);
   cmdList->add(saveWinFileCmd);
 
   _menuBar->addCommands(cmdList);
   delete cmdList;
 
   ////////
   // CREATE OPTIONS PULLDOWN
   ////////

   cmdList = new MenuCmdList("Options");

   cmdList->addButton(theUndoCmd);

   // Show percent stretch limits
   
   CustomDialog *percValuesDialog;
   percValuesDialog = new StretchPercValuesDialog("percValuesDialog", _cmd);
   Cmd *postPercValuesCmd;
   postPercValuesCmd = new PostDialogCmd("postPercValuesDialog",
					 True, percValuesDialog );
   cmdList->add(postPercValuesCmd);

   _menuBar->addCommands(cmdList);
   delete cmdList;

   ////////
   // Create Help menu
   ////////
 
        Cmd *helpOnContextCmd = new HelpOnContextCmd("On Context", True, 
						     _menuBar->baseWidget());
        Cmd *helpOnHelpCmd = new HelpSelfCmd("On Help", True,
                        _menuBar->baseWidget(), "*Help*On Help");
        Cmd *helpOnWindowCmd = new HelpSelfCmd("On Window", True,
                        _menuBar->baseWidget(), "*Help*On Window");
        Cmd *helpOnKeysCmd = new HelpSelfCmd("On Keys", True,
                        _menuBar->baseWidget(), "*Help*On Keys");
        Cmd *helpOnVersionCmd = new HelpSelfCmd("On Version", True,
                        _menuBar->baseWidget(), "*Help*On Version");
 
        cmdList = new MenuCmdList("Help");
 
        cmdList->addButton(helpOnContextCmd);
        cmdList->addButton(helpOnHelpCmd);
        cmdList->addButton(helpOnWindowCmd);
        cmdList->addButton(helpOnKeysCmd);
        cmdList->addButton(helpOnVersionCmd);
 
        _menuBar->addCommands(cmdList, True);
	delete cmdList;
}
