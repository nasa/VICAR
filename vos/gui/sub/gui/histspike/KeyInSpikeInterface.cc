//////////////////////////////////////////////////////////////
// KeyInSpikeInterface.C: An KeyIn interface to a Cmd object
///////////////////////////////////////////////////////////////
#include "KeyInSpikeInterface.h"
#include "Cmd.h"
#include "KeyinView.h"
#include "stdlib.h"
#include "stdio.h"
#include <iostream>
#include <stdint.h>
using namespace std;
#include <Xm/RowColumn.h>

KeyInSpikeInterface::KeyInSpikeInterface ( Widget parent,
				  Cmd *cmd ) : CmdInterface ( cmd )
{
  _w = XtCreateManagedWidget(_name, xmRowColumnWidgetClass, parent, 
			     NULL, 0 );
  installDestroyHandler();

  _spike = addOneSubView(_w, "SpikeValue:");
  setValue(_cmd->getValue());
}

KeyInSpikeInterface::~KeyInSpikeInterface ()
{
delete _spike;
}

KeyinView *KeyInSpikeInterface::addOneSubView (Widget parent, const char *name)
{
  KeyinView *view = new KeyinView(parent, name);
  view->manage();
  view->installCallback(&CmdInterface::executeCmdCallback, (XtPointer)this);
  return view;
}

void KeyInSpikeInterface::executeCmd(XtPointer)
{
  char *_strValue;
  int  _intValue;
  
  _strValue = _spike->getFieldValue();
  _intValue = atoi(_strValue);
  XtFree(_strValue);

  runCmd((CmdValue) (uintptr_t) _intValue);
}

void KeyInSpikeInterface::setValue(CmdValue value)
{
  char _strValue[20];
  int  _intValue = (int) (uintptr_t)value;
  
  sprintf(_strValue, "%d", _intValue);

  _spike->setFieldValue(_strValue);
}

