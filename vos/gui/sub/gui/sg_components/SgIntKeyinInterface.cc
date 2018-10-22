//////////////////////////////////////////////////////////////
// SgIntKeyinInterface.cc: An integer keyin interface to a Cmd object.
// Command should expect integer as a value.
///////////////////////////////////////////////////////////////
#include "SgIntKeyinInterface.h"
#include "KeyinView.h"
#include "Cmd.h"
#include "stdlib.h"
#include "stdio.h"
#include <stdint.h>
#include <Xm/RowColumn.h>

SgIntKeyinInterface::SgIntKeyinInterface ( Widget parent,
		Cmd *cmd ) : CmdInterface ( cmd )
{
    _w = XtCreateWidget(_name, xmRowColumnWidgetClass, parent, 
			     NULL, 0 );
    installDestroyHandler();

    _keyin = new KeyinView(_w, "KeyinView");
    _keyin->manage();
    _keyin->installCallback(&CmdInterface::executeCmdCallback, 
			(XtPointer)this);

    setValue(_cmd->getValue());
}

SgIntKeyinInterface::~SgIntKeyinInterface()
{
    delete _keyin;
}

void SgIntKeyinInterface::executeCmd(XtPointer)
{
    char *strValue = _keyin->getFieldValue();
    int intValue = atoi(strValue);
    XtFree(strValue);

    runCmd((CmdValue) (uintptr_t) intValue);
}

void SgIntKeyinInterface::setValue(CmdValue value)
{
    char strValue[20];
    int intValue = (int) (uintptr_t)value;

    sprintf(strValue, "%d", intValue);

    _keyin->setFieldValue(strValue);
}
