////////////////////////////////////////////////////////
// SgKeyinCmdInterface:  Command interface to get string value for cmd.
// Initial values come from Cmd object.  This class is not specific
// to the sage client library and can be reused if needed.
////////////////////////////////////////////////////////
#include "SgKeyinCmdInterface.h"
#include "KeyinView.h"
#include "Cmd.h"
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <stdio.h>

String SgKeyinCmdInterface::_defaults [ ] = {
    (char *)"*label.labelString:	Input:",
     NULL,
};

//////////////////////////////////////////////////////////////
// Constructor
//////////////////////////////////////////////////////////////
SgKeyinCmdInterface::SgKeyinCmdInterface(Widget parent, Cmd *cmd)
			: CmdInterface(cmd)
{
    char *value;
    value = (char *)_cmd->getValue();

    setDefaultResources ( parent, _defaults );

    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent,
		XmNorientation, XmVERTICAL,
		NULL);
    installDestroyHandler();

    _keyinView = new KeyinView(_w, "nameView");
    if (value)
	_keyinView->setFieldValue(value);

    XtAddCallback ( _keyinView->getField(), XmNactivateCallback, 
				&CmdInterface::executeCmdCallback,
				(XtPointer)this );

    _keyinView->manage();
}

//////////////////////////////////////////////////////////////
// Destructor
//////////////////////////////////////////////////////////////
SgKeyinCmdInterface::~SgKeyinCmdInterface()
{
    delete _keyinView;	// Widgets handled by BasicComponent dtor
}

//////////////////////////////////////////////////////////////
// Set the value for the I/F.  
//////////////////////////////////////////////////////////////

void SgKeyinCmdInterface::setValue(CmdValue val)
{
    char *value = (char *)val;

    CmdInterface::setValue(value);	// Removes cmd from deferred list

    if (value) {
	_keyinView->setFieldValue(value);
    }
    else {
	_keyinView->setFieldValue((char *)"");
    }
}


//////////////////////////////////////////////////////////////
// Execute command after creating dynamic value.
//////////////////////////////////////////////////////////////

void SgKeyinCmdInterface::executeCmd(XtPointer)
{
    char *string;

    string = strdup(_keyinView->getFieldValue());
    if (!string || !strlen(string)) return;
   
    // Delete leading blanks

    while (string[0] == ' ')
	for (int i = 0; i < (int)strlen(string); i++ )
           string[i] = string[i+1];

    // Delete trailing blanks

    while ((strlen(string) > 0) && (string[strlen(string)-1] == ' '))
	string[strlen(string)-1] = '\0';

    runCmd((CmdValue)string);
}
