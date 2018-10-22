/////////////////////////////////////////////////////////////////
// StretchPercValuesDialog.cc: Custom dialog for displaying  
// values for percent stretch.
/////////////////////////////////////////////////////////////////
#include "StretchPercValuesDialog.h"
#include "StretchPercValuesIf.h"
#include "Cmd.h"
#include <iostream>
using namespace std;

StretchPercValuesDialog::StretchPercValuesDialog(const char *name, Cmd *cmd)
    : CustomDialog(name, Default, Invisible, Invisible, Invisible, Visible)
{
    _cmd = cmd;
}

Widget StretchPercValuesDialog::createWorkArea(Widget parent)
{
    CmdInterface *ci = new StretchPercValuesIf(parent, _cmd);
    return ci->baseWidget();
}

