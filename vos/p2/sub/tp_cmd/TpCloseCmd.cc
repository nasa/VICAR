////////////////////////////////////////////////////////////
// TpCloseCmd.h: Select the point file then call the quit command 
// so that the application may exit.
////////////////////////////////////////////////////////////
#include "TpCloseCmd.h"
#include "TpMatchManager.h"
#include "YesNoDialog.h"
#include "PostSingleFileDialogCmd.h"
#include "TpSavePointFileAsCmd.h"
#include "TpApplication.h"
#include <stdlib.h>

TpCloseCmd::TpCloseCmd(const char *name, int active, TpMatchManager *mm, 
		       Cmd *quitCmd) 
	: NoUndoCmd(name, active)
{
    _matchManager = mm;
    _quitCmd = quitCmd;
}

void TpCloseCmd::saveCallback(void *clientData)
{
    TpCloseCmd *obj = (TpCloseCmd *)clientData;
    obj->save();
}

void TpCloseCmd::save()
{
    char *fileName = _matchManager->getIbisFileName();
    if (fileName && strlen(fileName)) {
	_matchManager->writePointsIbis();
	_quitCmd->execute();
    }
    else {
	Cmd *saveAsFileCmd = new TpSavePointFileAsCmd("saveAs", True, 
						      _matchManager, _quitCmd);
	Cmd *postCmd = new PostSingleFileDialogCmd("SaveAs", True,
						   saveAsFileCmd);
	postCmd->execute();
    }
}

void TpCloseCmd::noSaveCallback(void *clientData)
{
    TpCloseCmd *obj = (TpCloseCmd *)clientData;
    obj->noSave();
}

void TpCloseCmd::noSave()
{
    _matchManager->closePointsIbis();
    //exit(theTpApplication->getExitStatus());
    _quitCmd->execute();
}

void TpCloseCmd::doit()
{
    char *fileName = _matchManager->getIbisFileName();
    Boolean noName = False;
    if (!fileName) {
	fileName = sdup("None");
	noName = True;
    }
    char *msg = new char [33 + strlen(fileName)];
    sprintf (msg, "Do you want to save changes to %s?", fileName);
    if (noName) delete [] fileName;
    
    // Create a two-way dialog.  There is no need to delete
    // it afterwards since it destroys itself when user makes
    // a selection

    new YesNoDialog("save", (void *)this,
		    msg,
		    &TpCloseCmd::saveCallback,
		    &TpCloseCmd::noSaveCallback);

    delete [] msg;
}       
