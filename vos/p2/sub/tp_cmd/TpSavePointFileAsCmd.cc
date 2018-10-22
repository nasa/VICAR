/////////////////////////////////////////////////////////////
// TpSavePointFileAsCmd.cc: Saves all points in IBIS-2 file.
/////////////////////////////////////////////////////////////
#include "TpSavePointFileAsCmd.h"
#include "TpMatchManager.h"
#include "QuestionDialogManager.h"
#include <stdio.h>
#ifndef __VMS
  #include <sys/types.h>
  #include <sys/stat.h>
#endif

#define TP_POINTS_SAVE_AS_QUESTION "The file already exists.\nOverwrite?"

TpSavePointFileAsCmd::TpSavePointFileAsCmd(const char *name, int active, 
			       TpMatchManager *matchManager, Cmd *quitCmd)
	: NoUndoCmd(name, active)
{
    _matchManager = matchManager;
    _quitCmd = quitCmd;
}

void TpSavePointFileAsCmd::okCallback(void *clientData)
{
    TpSavePointFileAsCmd *obj = (TpSavePointFileAsCmd *)clientData;
    obj->NoUndoCmd::execute(obj->_tmpValue);
}

void TpSavePointFileAsCmd::execute(CmdValue new_value)
{
    _tmpValue = new_value;
#ifdef __VMS
    // VMS creates new versions so there's no need to ask
    NoUndoCmd::execute(new_value);
#else
    struct stat file_info;
    if (stat((char *)new_value, &file_info) == 0) {  // File exists
	theQuestionDialogManager->post(TP_POINTS_SAVE_AS_QUESTION,
				       (void *) this,
				       &TpSavePointFileAsCmd::okCallback);
    }
    else
	NoUndoCmd::execute(new_value);
#endif
}

void TpSavePointFileAsCmd::doit()
{
    _matchManager->writePointsIbis((char *)_value);
    if (_quitCmd) _quitCmd->execute();
}
