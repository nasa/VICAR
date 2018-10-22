///////////////////////////////////////////////////////////
// StretchListCmd.cc: Command class to set stretch value for 
// TABLE, ITABLE, or ALARM stretches.
//////////////////////////////////////////////////////////
#include "StretchListCmd.h"
#include "StretchCmdInterface.h"
#include "TableValue.h"
#include <iostream>
using namespace std;
#include <stdio.h>

StretchListCmd::StretchListCmd ( const char *name, int active, 
	StretchCmdInterface *stretchCmdInterface, 
	StretchType stretchType ) 
    : NoUndoCmd ( name, active )
{
    _stretchCmdInterface = stretchCmdInterface;
    _stretchType = stretchType;
}

void StretchListCmd::doit()
{
    // Change table's "in" and "out" values

    TableValue *tableValue = (TableValue*)_value;
    switch (_stretchType) {
      case ITABLE:
	_stretchCmdInterface->setITable(tableValue->inTable,
					tableValue->outTable, 
					tableValue->count);
	break;

      case TABLE: 
	_stretchCmdInterface->setTable(tableValue->inTable,
                                        tableValue->outTable,
                                        tableValue->count);
	break;

      case ALARM:
	_stretchCmdInterface->setAlarmTable(tableValue->inTable,
					    tableValue->count);
	break;

      default:
	cerr << "StretchListCmd::doit(): Memory error or wrong type is set\n";
    }

   _stretchCmdInterface->stretchIt();
}

void StretchListCmd::freeValue(CmdValue value)
{
    if (value)
	delete (TableValue *)value;
}
