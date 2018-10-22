////////////////////////////////////////////////////////////
// TpLoadConfigCmd.cc: Load config file.
////////////////////////////////////////////////////////////
#include "TpLoadConfigCmd.h"
#include "TpWindow.h"

TpLoadConfigCmd::TpLoadConfigCmd(const char *name, int active, 
				 TpWindow *window) 
    : NoUndoCmd(name, active)
{
    _window = window;
}

void TpLoadConfigCmd::doit()
{
    _window->loadConfig((char *)_value);
}       

