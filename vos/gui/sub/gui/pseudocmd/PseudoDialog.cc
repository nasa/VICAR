//////////////////////////////////////////////////////////////////////
// PseudoDialog.cc: Dialog box for Pseudocolor Tool
//////////////////////////////////////////////////////////////////////
#include "PseudoDialog.h"
#include "MenuBar.h"
#include "PostSingleFileDialogCmd.h"
#include "LoadPseudoFileCmd.h"
#include "SavePseudoFileCmd.h"
#include "SetDeferredCmd.h"
#include "ClearMarksCmd.h"
#include "PseudoValue.h"
#include "PseudoMarks.h"
#include "MenuCmdList.h"
#include "PseudoCmdInterface.h"
#include "Application.h"
#include "XvicImage.h"
#include "zvproto.h"
#include <string.h>
#include <Xm/Form.h>
#include <stdio.h>

XtResource PseudoDialog::_resources[] = {
 {
   (char *)"numPseudoFiles",
   (char *)"NumPseudoFiles",
   XmRInt,
   sizeof ( int ),
   XtOffset ( PseudoDialog *, _numPseudoFiles ),
   XmRString,
   ( XtPointer ) "7",
 },
 {
   (char *)"filename",
   (char *)"Filename",
   XmRString,
   sizeof ( String ),
   XtOffset ( PseudoDialog *, _filename ),
   XmRString,
   ( XtPointer ) "",
 },
 {
   (char *)"dirUNIX",
   (char *)"DirUNIX",
   XmRString,
   sizeof ( String ),
   XtOffset ( PseudoDialog *, _dirUNIX ),
   XmRString,
   ( XtPointer ) "",
 },
 {
   (char *)"dirVMS",
   (char *)"DirVMS",
   XmRString,
   sizeof ( String ),
   XtOffset ( PseudoDialog *, _dirVMS ),
   XmRString,
   ( XtPointer ) "",
 },
};

PseudoDialog::PseudoDialog(const char *name, Cmd *cmd, Cmd *modeCmd, Widget iw)
        : MenuDialog(name, Default, Visible, Invisible, Visible, Visible)
{
   _iw = iw;
   _cmd = cmd;
   _modeCmd = modeCmd;
   _pseudoValue = NULL;
   _pseudoMarks = NULL;
}

Widget PseudoDialog::createWorkArea(Widget parent)
{
   // Make sure we're using pseudocolor tables
   _modeCmd->execute ( (CmdValue)True );

   _form = XtVaCreateWidget("workArea",
                xmFormWidgetClass, parent,
                NULL );

   XtGetSubresources(_form, (XtPointer)this, "pseudoFiles", "PseudoFiles",
                        _resources, XtNumber(_resources), NULL, 0);

   // Model for LUTs
   _pseudoValue = new PseudoValue;
   // Model for tick marks
   _pseudoMarks = new PseudoMarks;
   CmdInterface *cmdInterface = new PseudoCmdInterface(_form, _cmd, _pseudoValue, _pseudoMarks, _iw);

   // Set the command that switches between deferred and immediate execution
   _setDeferredCmd = new SetDeferredCmd ( "SetDeferredCmd", True, 
					cmdInterface, _applyCmdList, &_apply );

   XtVaSetValues( cmdInterface->baseWidget(), 
                XmNtopAttachment,    XmATTACH_FORM,
                XmNrightAttachment,  XmATTACH_FORM,
                XmNleftAttachment,   XmATTACH_FORM,
                XmNbottomAttachment, XmATTACH_FORM,
                NULL );

   cmdInterface->manage();

   return _form;
}

void PseudoDialog::createMenuPanes()
{
   MenuCmdList *cmdList;

   ////////
   // CREATE FILE PULLDOWN
   ////////

   cmdList = new MenuCmdList("File");
   
   Cmd *loadFileCmd = new LoadPseudoFileCmd ("loadFile", True, _pseudoValue);
   Cmd *loadWinFileCmd = new PostSingleFileDialogCmd ("loadWin", True, loadFileCmd);
   cmdList->add ( loadWinFileCmd );

   Cmd *saveFileCmd = new SavePseudoFileCmd ("saveFile", True, _pseudoValue);
   Cmd *saveWinFileCmd = new PostSingleFileDialogCmd ("saveWin", True, saveFileCmd);
   cmdList->add ( saveWinFileCmd );

   _menuBar->addCommands ( cmdList );
   delete cmdList;

   ////////
   // CREATE OPTIONS PULLDOWN
   ////////

   cmdList = new MenuCmdList("Options");

   cmdList->addCheckBox ( _modeCmd );

   cmdList->addCheckBox ( _setDeferredCmd );

   Cmd *clearMarksCmd = new ClearMarksCmd ( "ClearMarksCmd", True, _pseudoMarks );
   cmdList->add ( clearMarksCmd );

   cmdList->addSeparator();

   Cmd *loadDefFileCmd[10];
   char buf[16][10];
   for (int n = 0; n < _numPseudoFiles; n++) {
      sprintf(buf[n], "file%d", n+1);
      XtGetSubresources(_form, (XtPointer)this, buf[n], "File",
                        _resources, XtNumber(_resources), NULL, 0);
      char *fullFilename = new char[80];
      #if UNIX_OS
      	strcpy(fullFilename, _dirUNIX);
      	strcat(fullFilename, _filename);
      #else
	strcpy(fullFilename, _dirVMS);
        strcat(fullFilename, _filename);	
      #endif
      char *xlateFilename = new char[80];
      zvfilename(fullFilename, xlateFilename, 0);
      loadDefFileCmd[n] = new LoadPseudoFileCmd (buf[n], True, _pseudoValue, xlateFilename);
      cmdList->add ( loadDefFileCmd[n] );
   }

   _menuBar->addCommands ( cmdList );
   delete cmdList;

   loadDefFileCmd[0]->execute();

   // Add two marks at 0 and 255
   int r,g,b;
   _pseudoValue->getRGBDn(0, &r, &g, &b);
   _pseudoMarks->addMark(0, 0, r, g, b);

   _pseudoValue->getRGBDn(255, &r, &g, &b);
   _pseudoMarks->addMark(255, 0, r, g, b);
}

void PseudoDialog::post()
{
    // Make sure we're using pseudocolor tables

   _modeCmd->execute((CmdValue)True);

    MenuDialog::post();
}
