///////////////////////////////////////////////////////////////////
// TpQualFormatValue.cc: TpPointFileCmd's command value.
///////////////////////////////////////////////////////////////////
#include "TpQualFormatValue.h"
#include "TpDefs.h"
#include <stdlib.h>
#include <stdio.h>

TpQualFormatValue::TpQualFormatValue()
{
    _numQuals = 0;
    _info = NULL;
}

TpQualFormatValue::TpQualFormatValue(int numQuals, TpQualInfo *info)
{
    _numQuals = numQuals;
    _info = info;
}

TpQualFormatValue::TpQualFormatValue(const TpQualFormatValue &val)
{
    _numQuals = val._numQuals;

    int i;

    _info = new TpQualInfo[_numQuals];
    for (i = 0; i < _numQuals; i++) {
	_info[i]._qualName = sdup(val._info[i]._qualName);
	_info[i]._qualType = val._info[i]._qualType;
	_info[i]._qualUnit = sdup(val._info[i]._qualUnit);
    }
}

TpQualFormatValue &TpQualFormatValue::operator=(TpQualFormatValue &val)
{
    if (this == &val)
        return *this;           // assignment to self
 
    _numQuals = val._numQuals;

    int i;
 
    _info = new TpQualInfo[_numQuals];
    for (i = 0; i < _numQuals; i++) {
	_info[i]._qualName = sdup(val._info[i]._qualName);
        _info[i]._qualType = val._info[i]._qualType;
        _info[i]._qualUnit = sdup(val._info[i]._qualUnit);
    }
 
    return *this;
}

void TpQualFormatValue::setNumQuals(int n)
{
    int i;
    if (n > _numQuals) {
	for (i = _numQuals; i < n; i++)
	    addQualInfo();
    }

    if (n < _numQuals) {
        for (i = n; i < _numQuals; i++)
            deleteQualInfo();
    }
}

void TpQualFormatValue::addQualInfo(const char *name, TpQualType type,
							const char *unit)
{
    TpQualInfo *newInfo;
    newInfo = new TpQualInfo[_numQuals + 1];
    for (int i = 0; i < _numQuals; i++) {
	newInfo[i]._qualName = sdup(_info[i]._qualName);
        newInfo[i]._qualType = _info[i]._qualType;
        newInfo[i]._qualUnit = sdup(_info[i]._qualUnit);
    }
    newInfo[_numQuals]._qualName = sdup(name);
    newInfo[_numQuals]._qualType = type;
    newInfo[_numQuals]._qualUnit = sdup(unit);
    
    if (_info) delete [] _info;
    _info = newInfo;

    _numQuals++;
}

void TpQualFormatValue::deleteQualInfo()
{
    TpQualInfo *newInfo;
    newInfo = new TpQualInfo[_numQuals - 1];
    for (int i = 0; i < _numQuals - 1; i++) {
        newInfo[i]._qualName = sdup(_info[i]._qualName);
        newInfo[i]._qualType = _info[i]._qualType;
        newInfo[i]._qualUnit = sdup(_info[i]._qualUnit);
    }
 
    if (_info) delete [] _info;
    _info = newInfo;

    _numQuals--;
}
