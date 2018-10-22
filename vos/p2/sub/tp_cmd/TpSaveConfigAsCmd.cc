/////////////////////////////////////////////////////////////
// TpSaveConfigAsCmd.cc: Saves all points in IBIS-2 file.
/////////////////////////////////////////////////////////////
#include "TpSaveConfigAsCmd.h"
#include "TpWindow.h"
#include "QuestionDialogManager.h"
#include <stdio.h>
#ifndef __VMS
  #include <sys/types.h>
  #include <sys/stat.h>
#endif

#define TP_CONFIG_SAVE_AS_QUESTION "The file already exists.\nOverwrite?"

TpSaveConfigAsCmd::TpSaveConfigAsCmd(const char *name, int active, TpWindow *window)
    : NoUndoCmd(name, active)
{
    _window = window;
}

void TpSaveConfigAsCmd::okCallback(void *clientData)
{
    TpSaveConfigAsCmd *obj = (TpSaveConfigAsCmd *)clientData;
    obj->NoUndoCmd::execute(obj->_tmpValue);
}

void TpSaveConfigAsCmd::execute(CmdValue new_value)
{
    _tmpValue = new_value;
#ifdef __VMS
    // VMS creates new versions so there's no need to ask
    NoUndoCmd::execute(new_value);
#else
    struct stat file_info;
    if (stat((char *)new_value, &file_info) == 0) {  // File exists
	theQuestionDialogManager->post(TP_CONFIG_SAVE_AS_QUESTION,
				       (void *) this,
				       &TpSaveConfigAsCmd::okCallback);
    }
    else
	NoUndoCmd::execute(new_value);
#endif
}

void TpSaveConfigAsCmd::doit()
{
    _window->saveConfig((char *)_value);
}
