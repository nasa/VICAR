/////////////////////////////////////////////////////////////////
// StretchParmListDialog.cc: Custom dialog for selecting 
// values for table, itable and alarm stretches.
/////////////////////////////////////////////////////////////////
#include "StretchParmListDialog.h"
#include "StretchListInterface.h"
#include "StretchCmd.h"
#include <iostream>
using namespace std;

StretchParmListDialog::StretchParmListDialog(const char *name, 
		Cmd *cmd, StretchType stretchType, int defValue)
                                                          // = 0
    : CustomDialog(name, Default, Invisible, Invisible, Invisible, Visible)
{
    if ((stretchType != TABLE) &&
	(stretchType != ITABLE) && 
	(stretchType != ALARM))
	cerr << "Wrong values passed to the constructor or memory error\n";
    else
	_stretchType = stretchType;

    _defValue = defValue;
    _cmd = cmd;
}

Widget StretchParmListDialog::createWorkArea(Widget parent)
{
    CmdInterface *ci = new StretchListInterface(parent, _cmd, 
						_stretchType, _defValue);
    
    return ci->baseWidget();
}

