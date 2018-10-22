//////////////////////////////////////////////////////////////////////////////
// TpQualGroupMgr.h:
//////////////////////////////////////////////////////////////////////////////
#include "TpQualGroupMgr.h"
#include "TpDefs.h"
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

//!!!! Comment out next line before delivery!
//#define DEBUG
#ifdef DEBUG
#define DPR(x) printf x
#else
#define DPR(x)
#endif

TpQualGroupMgr::TpQualGroupMgr()
{
    _numGroups = 0;
    _groups = NULL;

    _numQuals = 0;
    _qualFormat = NULL;

    _names = NULL;
    _units = NULL;
}

TpQualGroupMgr::TpQualGroupMgr(int numQuals, TpQualType *qualFormat)
{
    _numGroups = 0;
    _groups = NULL;

    _numQuals = numQuals;

    if (_numQuals == 0) {
	_qualFormat = NULL;
	_names = NULL;
	_units = NULL;
    }
    else {
	int i;
	
	_qualFormat = new TpQualType [_numQuals];
	_names = new char *[_numQuals];
	_units = new char *[_numQuals];
	for (i = 0; i < _numQuals; i++) {
	    _qualFormat[i] = qualFormat[i];
	    _names[i] = sdup("");
	    _units[i] = sdup("");
	}
    }

#ifdef DEBUG
    std::cout << (*this) << std::endl;
#endif

}

TpQualGroupMgr::~TpQualGroupMgr()
{
    int i;
    for (i = 0; i < _numGroups; i++)
	delete _groups[i];
    if (_groups)
	delete [] _groups;

    if (_qualFormat) 
	delete [] _qualFormat;

    for (i = 0; i < _numQuals; i++) {
	delete [] _names[i];
	delete [] _units[i];
    }

    if (_names) delete [] _names;
    if (_units) delete [] _units;
}

void TpQualGroupMgr::addGroup(TpQualGroup *group)
{
    int i;

    for (i = 0; i < _numQuals; i++)
	group->incNumQuals(_qualFormat[i]);

    TpQualGroup **newGroups;
    newGroups = new TpQualGroup *[_numGroups + 1];
    for (i = 0; i < _numGroups; i++)
        newGroups[i] = _groups[i];
    newGroups[_numGroups] = group;
    if (_groups) delete [] _groups;
    _groups = newGroups;
    _numGroups++;
}

void TpQualGroupMgr::deleteGroup(TpQualGroup *group)
{
    int i;

    TpQualGroup **newGroups;
    newGroups = new TpQualGroup *[_numGroups - 1];
    int index = 0;
    for (i = 0; i < _numGroups; i++) {
	if (_groups[i] != group) 
	    newGroups[index++] = _groups[i];
    }
    assert (index < _numGroups);
    if (_groups) delete [] _groups;
    _groups = newGroups;
    
    _numGroups--;
}
 
void TpQualGroupMgr::incNumQuals(TpQualType type)
{
    int i;
 
    TpQualType *newQualFormat;
    newQualFormat = new TpQualType[_numQuals + 1];
    for (i = 0; i < _numQuals; i++)
        newQualFormat[i] = _qualFormat[i];
    newQualFormat[_numQuals] = type;
    if (_qualFormat) delete [] _qualFormat;
    _qualFormat = newQualFormat;

    char **newNames;
    newNames = new char *[_numQuals + 1];
    for (i = 0; i < _numQuals; i++)
        newNames[i] = _names[i];
    newNames[_numQuals] = sdup("");
    if (_names) delete [] _names;
    _names = newNames;

    char **newUnits;
    newUnits = new char *[_numQuals + 1];
    for (i = 0; i < _numQuals; i++)
        newUnits[i] = _units[i];
    newUnits[_numQuals] = sdup("");
    if (_units) delete [] _units;
    _units = newUnits;

    _numQuals++;

    for (i = 0; i < _numGroups; i++)
	_groups[i]->incNumQuals(type);

#ifdef DEBUG
    std::cout << (*this) << std::endl;
#endif
}

void TpQualGroupMgr::decNumQuals()
{
    int i;
    TpQualType *newQualFormat;
    newQualFormat = new TpQualType[_numQuals - 1];
    for (i = 0; i < _numQuals-1; i++)
        newQualFormat[i] = _qualFormat[i];
    if (_qualFormat) delete [] _qualFormat;
    _qualFormat = newQualFormat;

    char **newNames;
    newNames = new char *[_numQuals - 1];
    for (i = 0; i < _numQuals-1; i++)
        newNames[i] = _names[i];
    if (_names) delete [] _names;
    _names = newNames;

    char **newUnits;
    newUnits = new char *[_numQuals - 1];
    for (i = 0; i < _numQuals-1; i++)
        newUnits[i] = _units[i];
    if (_units) delete [] _units;
    _units = newUnits;

    _numQuals--;

    for (i = 0; i < _numGroups; i++)
        _groups[i]->decNumQuals();

#ifdef DEBUG
    cout << this;
#endif

}

void TpQualGroupMgr::deleteAllQuals()
{
    int i;

    if (_groups != NULL) {
	for (i = 0; i < _numGroups; i++)
	    _groups[i]->deleteAllQuals();
	
	for (i = 0; i < _numGroups; i++)
	    delete _groups[i];
	
	delete []_groups;
	_groups = NULL;
	_numGroups = 0;
    }

    for (i = 0; i < _numQuals; i++) {
        delete [] _names[i];
        delete [] _units[i];
    }
    if (_names) delete [] _names;
    if (_units) delete [] _units;
    _names = _units = NULL;

    if (_qualFormat) delete [] _qualFormat;
    _qualFormat = NULL;

    _numQuals = 0;
}

void TpQualGroupMgr::setFormat(int numQuals, char (*newFormat)[6])
{
    deleteAllQuals();
    for (int i = 0; i < numQuals; i++) {
	if (!strcmp(newFormat[i], FULL))
	    incNumQuals(TpFull);
	else if (!strcmp(newFormat[i], REAL))
	    incNumQuals(TpReal);
	else
	    incNumQuals(TpText);
    }
}

int TpQualGroupMgr::getNumFullQuals() const
{
    int counter = 0;
    for (int i = 0; i < _numQuals; i++) {
	if (_qualFormat[i] == TpFull)
	    counter++;
    }
    return counter;
}

int TpQualGroupMgr::getNumRealQuals() const
{
    int counter = 0;
    for (int i = 0; i < _numQuals; i++) {
        if (_qualFormat[i] == TpReal)
            counter++;
    }
    return counter;
}

int TpQualGroupMgr::getNumTextQuals() const
{
    int counter = 0;
    for (int i = 0; i < _numQuals; i++) {
        if (_qualFormat[i] == TpText)
            counter++;
    }
    return counter;
}

void TpQualGroupMgr::setValue(int group, int n, char *value)
{
    _groups[group]->setValue(n, value);
}

void TpQualGroupMgr::setValue(TpQualGroup *group, int n, char *value)
{
    for (int i = 0; i < _numGroups; i++)
	if (_groups[i] == group)
	    _groups[i]->setValue(n, value);
}

Boolean TpQualGroupMgr::isUnique(TpQualGroup *group, int n, char *value)
{
    for (int i = 0; i < _numGroups; i++)
	if (_groups[i] != group)
	    if (_groups[i]->isEqual(n, value))
		return False;
    return True;
}

// Return minimum qualifier value or -1 if the format is text
int TpQualGroupMgr::getMinValue(int n, int &value)
{
    if (n >= _numQuals) return (-1);

    if (_qualFormat[n] == TpText)
	return (-1);

    if (_qualFormat[n] == TpFull) {
	int valueF, minValueF;
	minValueF = 9999;
	for (int i = 0; i < _numGroups; i++) {
	    _groups[i]->getValue(n, valueF);
	    if (valueF < minValueF)
		minValueF = valueF;
	}
	value = minValueF;
	return 0;
    }

    if (_qualFormat[n] == TpReal) {
        float valueR, minValueR;
        minValueR = 9999.0;
        for (int i = 0; i < _numGroups; i++) {
            _groups[i]->getValue(n, valueR);
            if (valueR < minValueR)
                minValueR = valueR;
        }
	value = (int)minValueR;
	return 0;
    }
    
    assert(0); // sanity check
    return -1; // to avoid warning
}

// Return maximum qualifier value or -1 if the format is text
int TpQualGroupMgr::getMaxValue(int n, int &value)
{
    if (n >= _numQuals) return (-1);

    if (_qualFormat[n] == TpText)
        return (-1);
    
    if (_qualFormat[n] == TpFull) {
        int valueF, maxValueF;
        maxValueF = -9999;
        for (int i = 0; i < _numGroups; i++) {
            _groups[i]->getValue(n, valueF);
            if (valueF > maxValueF)
                maxValueF = valueF;
        }
        value = maxValueF;
        return 0;
    }
 
    if (_qualFormat[n] == TpReal) {
        float valueR, maxValueR;
        maxValueR = -9999.0;
        for (int i = 0; i < _numGroups; i++) {
            _groups[i]->getValue(n, valueR);
            if (valueR > maxValueR)
                maxValueR = valueR;
        }
        value = (int)maxValueR;
        return 0;
    }

    assert(0); // sanity check
    return -1; // to avoid warning
}

char *TpQualGroupMgr::getQualName(int n) const
{
    return (sdup(_names[n]));
}

char *TpQualGroupMgr::getQualUnit(int n) const
{
    return (sdup(_units[n]));
}

void TpQualGroupMgr::setQualName(int i, char *name)
{
    DPR(("Setting qual #%d name to %s\n", i, name));
    delete [] _names[i];
    if (name && (strlen(name) > 0))
	_names[i] = sdup(name);
    else 
	_names[i] = sdup("");
}
 
void TpQualGroupMgr::setQualUnit(int i, char *unit)
{
    DPR(("Setting qual #%d unit to %s\n", i, unit));
    delete [] _units[i];
    _units[i] = sdup(unit);
}

std::ostream &operator<<(std::ostream &ostr, const TpQualGroupMgr &m)
{
    for (int i = 0; i < m._numGroups; i++) 
	ostr << *(m._groups[i]) << std::endl;

    return ostr;
}

