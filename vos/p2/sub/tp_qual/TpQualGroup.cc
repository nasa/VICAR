///////////////////////////////////////////////////////////////////////////////
// TpQualGroup.cc:
///////////////////////////////////////////////////////////////////////////////
#include "TpQualGroup.h"
#include "TpQualGroupMgr.h"
#include <sstream>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

TpQualGroup::TpQualGroup(TpQualGroupMgr *mgr)
{
    _mgr = mgr;

    _numQuals = 0;
 
    _qualFormat = NULL;
    _qualifiers = NULL;

    _mgr->addGroup(this);
}

TpQualGroup::TpQualGroup(TpQualGroupMgr *mgr, int numQuals, 
			 TpQualType *qualFormat)
{
    _mgr = mgr;
    _numQuals = numQuals;

    int i;

    _qualFormat = new TpQualType [_numQuals];
    for (i = 0; i < _numQuals; i++) {
	_qualFormat[i] = qualFormat[i];
    }

    _qualifiers = new TpQualifier *[_numQuals];
    for (i = 0; i < _numQuals; i++) {
	_qualifiers[i] = new TpQualifier(_qualFormat[i]);
    }

    _mgr->addGroup(this);
}

TpQualGroup::~TpQualGroup()
{
    if (_qualFormat) delete [] _qualFormat;
    if (_qualifiers) delete [] _qualifiers;
    _numQuals = 0;

    _mgr->deleteGroup(this);
}
 
void TpQualGroup::incNumQuals(TpQualType type)
{
    int i;

    TpQualType *newQualFormat;
    newQualFormat = new TpQualType[_numQuals + 1];
    for (i = 0; i < _numQuals; i++)
	newQualFormat[i] = _qualFormat[i];
    newQualFormat[_numQuals] = type;
    if (_qualFormat) delete [] _qualFormat;
    _qualFormat = newQualFormat;
    
    TpQualifier **newQualifiers;
    newQualifiers = new TpQualifier *[_numQuals + 1];
    for (i = 0; i < _numQuals; i++)
	newQualifiers[i] = _qualifiers[i];
    newQualifiers[_numQuals] = new TpQualifier(_qualFormat[_numQuals]);
    if (_qualifiers) delete [] _qualifiers;
    _qualifiers = newQualifiers;

    _numQuals++;
}

void TpQualGroup::decNumQuals()
{
    int i;
    TpQualType *newQualFormat;
    newQualFormat = new TpQualType[_numQuals - 1];
    for (i = 0; i < _numQuals-1; i++)
        newQualFormat[i] = _qualFormat[i];
    if (_qualFormat) delete [] _qualFormat;
    _qualFormat = newQualFormat;
 
    TpQualifier **newQualifiers;
    newQualifiers = new TpQualifier *[_numQuals - 1];
    for (i = 0; i < _numQuals-1; i++)
        newQualifiers[i] = _qualifiers[i];
    if (_qualifiers) delete [] _qualifiers;
    _qualifiers = newQualifiers;
 
    _numQuals--;

}

void TpQualGroup::deleteAllQuals()
{
    if (_qualFormat) delete [] _qualFormat;
    if (_qualifiers) delete [] _qualifiers;

    _qualFormat = NULL;
    _qualifiers = NULL;

    _numQuals = 0;
}

void TpQualGroup::getValue(int n, int &value) const
{
    _qualifiers[n]->getValue(value);
}
 
void TpQualGroup::getValue(int n, float &value) const
{
    _qualifiers[n]->getValue(value);
}
 
void TpQualGroup::getValue(int n, char *&value) const
{
    _qualifiers[n]->getValue(value);
}

char *TpQualGroup::valueToString(int n) const
{
    return (_qualifiers[n]->valueToString());
}

float *TpQualGroup::getRealQuals()
{
    float *a;
    int length = 0;
    int i;
    for (i = 0; i < _numQuals; i++) {
	if (_qualFormat[i] == TpReal)
	    length++;
    }
    if (length == 0) return NULL;
    a = new float[length];
    length = 0;
    for (i = 0; i < _numQuals; i++) {
	if (_qualFormat[i] == TpReal)
	    _qualifiers[i]->getValue(a[length++]);
    }
    return a;
}

int *TpQualGroup::getFullQuals()
{
    int *a;
    int length = 0;
    int i;
    for (i = 0; i < _numQuals; i++) {
        if (_qualFormat[i] == TpFull)
            length++;
    }
    if (length == 0) return NULL;
    a = new int[length];
    length = 0;
    for (i = 0; i < _numQuals; i++) {
        if (_qualFormat[i] == TpFull)
            _qualifiers[i]->getValue(a[length++]);
    }
    return a;
}

char *TpQualGroup::getTextQuals()
{
    char *a;
    int length = 0;
    int i;
    for (i = 0; i < _numQuals; i++) {
        if (_qualFormat[i] == TpText)
            length++;
    }
    if (length == 0) return NULL;
    a = new char[length * 257];
    length = 0;
    for (i = 0; i < _numQuals; i++) {
        if (_qualFormat[i] == TpText) {
	    char *p = a + (length * 257);
            _qualifiers[i]->getValue(p);
	    length++;
	}
    }
    return a;
}

void TpQualGroup::setValue(int n, char *value)
{
    _qualifiers[n]->setValue(value);
}

void TpQualGroup::setValue(int n, int value)
{
    std::ostringstream outStream;
    outStream << value << '\0';
    _qualifiers[n]->setValue((char *)outStream.str().c_str());
}

void TpQualGroup::setValue(int n, float value)
{
    std::ostringstream outStream;
    outStream << value << '\0';
    _qualifiers[n]->setValue((char *)outStream.str().c_str());
}

Boolean TpQualGroup::isEqual(int n, char *value)
{
    if (_qualifiers[n]->getType() == TpText) {
	char *vt = new char[257];
	_qualifiers[n]->getValue(vt);
	if (strcmp(vt, value)) {
	    delete [] vt;
	    return False;
	}
	else {
	    delete [] vt;
	    return True;
	}
    }
    else if (_qualifiers[n]->getType() == TpFull) {
	int vf;
	_qualifiers[n]->getValue(vf);
	std::ostringstream vfs;
	vfs << vf << '\0';
	if (strcmp(vfs.str().c_str(), value))
            return False;
        else
            return True;
    }
    else if (_qualifiers[n]->getType() == TpReal) {
        int vr;
        _qualifiers[n]->getValue(vr);
	std::ostringstream vrs;
	vrs << vr << '\0';
        if (strcmp(vrs.str().c_str(), value))
            return False;
        else
            return True;
    }
    else {
	assert(0);
    }
    return False; // To avoid warning on certain compilers
}

std::ostream &operator<<(std::ostream &ostr, const TpQualGroup &qg)
{
    int i;
    for (i = 0; i < qg._numQuals-1; i++) {
	ostr << *(qg._qualifiers[i]);
	ostr << "; ";
    }
    if (qg._numQuals > 0)
	ostr << *(qg._qualifiers[qg._numQuals-1]);

    return ostr;
}

TpQualType TpQualGroup::getValueType(char *s)
{
    if (!s || (strlen(s) == 0))
        return TpText;
 
    // Special case - if string is in ""
 
    if (s[0] == '"')
        return TpText;
 
    // Special case - if "0" was typed
 
    if (!strcmp(s, "0")) {
        return TpFull;
    }
 
    // Check if entered value is an integer
 
    int valueInt = atoi(s);
    if ((valueInt != 0) && (!strchr(s, '.'))) {
        return TpFull;
    }
 
    // Check if entered value is a real number
 
    float valueFloat;
    valueFloat = atof(s);
    if (valueFloat != 0.0) {
        return TpReal;
    }
 
    // If it is not integer or real, it must be text then
 
    return TpText;
}
