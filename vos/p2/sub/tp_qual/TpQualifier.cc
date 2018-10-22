///////////////////////////////////////////////////////////////
// TpQualifier.cc:
///////////////////////////////////////////////////////////////
#include "TpQualifier.h"
#include "TpDefs.h"
#include <sstream>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

TpQualifier::TpQualifier(TpQualType type) 
    : _type(type) 
{ 
    switch (_type) {
    case TpFull:
        _value.f = -1;
        break;
    case TpReal:
        _value.r = -1.0;
        break;
    case TpText:
        _value.t = sdup("NONE");
        break;
    default:
        assert(0);
    }
}

TpQualifier::~TpQualifier()
{
    if (_type == TpText)
	if (_value.t) 
	    delete [] _value.t;
}

void TpQualifier::setValue(char *value) 
{
    switch (_type) {
    case TpFull:
	_value.f = atoi(value);
	break;
    case TpReal:
	_value.r = atof(value);
	break;
    case TpText:
	if (_value.t) delete [] _value.t;
	_value.t = sdup(value);
	break;
    default:
	assert(0);
    }
}

void TpQualifier::getValue(int &value) const 
{ 
    (_type == TpFull) ? (value = _value.f) : (value = 0); 
}

void TpQualifier::getValue(float &value) const
{
    (_type == TpReal) ? (value = _value.r) : (value = 0.0); 
}

void TpQualifier::getValue(char *&value) const
{
    if (_type == TpText)
	sprintf(value, "%s", _value.t);
    else
	sprintf(value, "%s", "");
}

std::ostream &operator<<(std::ostream &ostr, const TpQualifier &q) 
{
    switch (q._type) {
    case TpFull:
	ostr << q._value.f;
	break;
    case TpReal:
	ostr << q._value.r;
	break;
    case TpText:
	ostr << q._value.t;
	break;
    default:
	assert(0);
    }

    return ostr;
}

char *TpQualifier::valueToString() const
{
    std::ostringstream outStream;
    switch (_type) {
    case TpFull:
	outStream << _value.f << '\0';
	break;
    case TpReal:
	outStream << _value.r << '\0';
	break;
    case TpText:
	outStream << _value.t << '\0';
	break;
    default:
	assert(0);
    }

    return (sdup(outStream.str().c_str()));
}

