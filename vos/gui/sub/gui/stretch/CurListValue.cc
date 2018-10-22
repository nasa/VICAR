///////////////////////////////////////////////////////
// CurListValue.cc: Current Selection on the List
////////////////////////////////////////////////////////
#include "CurListValue.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream>
using namespace std;

CurListValue::CurListValue (Widget parent, const char *name, int defValue)
		: KeyinView (parent, name)
{
	if (defValue)
		setValue(defValue);
	installCallback();
}

void CurListValue::setValue ( int newValue )
{
	_value = newValue;
	char buf[5];
	sprintf(buf, "%d", newValue);
	setFieldValue(buf);
}

int CurListValue::getValue ()
{
	return _value;
}

void CurListValue::update ( XtPointer )
{
	if (!strcmp("", getFieldValue()))
		_value = _defValue;
	else
		_value = atoi(getFieldValue());
}
