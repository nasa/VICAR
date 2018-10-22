//////////////////////////////////////////////////////////////////////////////
// TpQualFormatCmd.cc: Command that sets qualifier format.
//////////////////////////////////////////////////////////////////////////////
#include "TpQualFormatCmd.h"
#include "TpQualFormatValue.h"
#include "TpQualGroupMgr.h"
#include <stdlib.h>
#include <stdio.h>

TpQualFormatCmd::TpQualFormatCmd(const char *name, int active, 
				 TpQualGroupMgr *qualGroupMgr)
    : Cmd(name, active)
{
    _qualGroupMgr = qualGroupMgr;

    collectValue();
}

void TpQualFormatCmd::collectValue()
{
    int n = _qualGroupMgr->getNumQuals();
    TpQualInfo *info = NULL;
    if (n > 0) info = new TpQualInfo[n];
    for (int i = 0; i < n; i++) {
        info[i]._qualName = _qualGroupMgr->getQualName(i);
        info[i]._qualType = _qualGroupMgr->getType(i);
        info[i]._qualUnit = _qualGroupMgr->getQualUnit(i);
    }
    TpQualFormatValue *value = new TpQualFormatValue(n, info);
    execute((CmdValue)value);
}

void TpQualFormatCmd::doit()
{
    TpQualFormatValue *value = (TpQualFormatValue *)_value;

    int i;
    if (value->_numQuals > _qualGroupMgr->getNumQuals()) {
	for (i = _qualGroupMgr->getNumQuals(); i < value->_numQuals; i++)
	    _qualGroupMgr->incNumQuals(value->_info[i]._qualType);
    }
    else if (value->_numQuals < _qualGroupMgr->getNumQuals()) {
	for (i = value->_numQuals; i < _qualGroupMgr->getNumQuals(); i++)
	    _qualGroupMgr->decNumQuals();
    }
    for (i = 0; i < value->_numQuals; i++) {
	_qualGroupMgr->setQualName(i, value->_info[i]._qualName);
	_qualGroupMgr->setQualUnit(i, value->_info[i]._qualUnit);
    }
}
