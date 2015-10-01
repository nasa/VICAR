$!****************************************************************************
$!
$! Build proc for MIPL module tp_qual
$! VPACK Version 1.9, Monday, December 07, 2009, 16:39:34
$!
$! Execute by entering:		$ @tp_qual
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module tp_qual ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to tp_qual.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("tp_qual.imake") .nes. ""
$   then
$      vimake tp_qual
$      purge tp_qual.bld
$   else
$      if F$SEARCH("tp_qual.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tp_qual
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tp_qual.bld "STD"
$   else
$      @tp_qual.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tp_qual.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tp_qual.com -mixed -
	-s TpQualifier.cc TpQualGroup.cc TpQualGroupMgr.cc -
	   TpQualFormatDialog.cc TpQualFormatCmd.cc TpQualFormatValue.cc -
	   TpQualFormatCmdInterface.cc TpQualFormatView.cc -
	   TpQualFormatSingleView.cc TpAddIdAsGenQualInterface.cc -
	   TpAddCorrParmAsPntQualInterface.cc -
	-i tp_qual.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create TpQualifier.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualGroup.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualGroupMgr.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualFormatDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////////////////
// TpQualFormatDialog.h: Dialog containing qualifier format values.
////////////////////////////////////////////////////////////////////////////
#include "TpQualFormatDialog.h"
#include "TpQualFormatCmd.h"
#include "TpSetMatchIdOffsetCmd.h"
#include "TpSetMatchIdNextCmd.h"
#include "TpQualFormatCmdInterface.h"
#include "TpAddIdAsGenQualInterface.h"
#include "TpAddCorrParmAsPntQualInterface.h"
#include "TpMatchManager.h"
#include "StringKeyinInterface.h"
#include "TpDefs.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <stdio.h>

TpQualFormatDialog::TpQualFormatDialog(const char *name, TpMatchManager *mm)
	: CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{
    _matchManager = mm;
    _cmdGen = NULL;
    _cmdPnt = NULL;
}

Widget TpQualFormatDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("workArea", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 XmNnumColumns, 1,
				 XmNpacking, XmPACK_TIGHT,
				 NULL);

    XtVaCreateManagedWidget("qualFormatLabel", 
			    xmLabelWidgetClass, rc, 
			    NULL);

    /////////////////////////////////////////////////////////////////////////
    // General Qualifier Part
    /////////////////////////////////////////////////////////////////////////

    _cmdGen = new TpQualFormatCmd("genQualFormatCmd", True, 
				  _matchManager->getGenQualMgr());
    Widget rc1 = XtVaCreateManagedWidget("rcGenQualExtra", 
			xmRowColumnWidgetClass, rc,
			XmNorientation, XmHORIZONTAL, 
			XmNnumColumns, 1, 
			XmNpacking, XmPACK_TIGHT,
			NULL); 
    CmdInterface *idAsGen = new TpAddIdAsGenQualInterface(rc1, _cmdGen);
    idAsGen->manage();

    XtVaCreateManagedWidget("offsetLabel", xmLabelWidgetClass, rc1, NULL);
    char buf[16];
    sprintf(buf, "%d", _matchManager->getStartId());
    Cmd *offsetCmd = new TpSetMatchIdOffsetCmd("setMatchIdOffset",
			True, (CmdValue)buf, _matchManager);
    CmdInterface *setMatchIdOffsetCmdIf = new StringKeyinInterface(rc1, offsetCmd);
    offsetCmd->newValue();
    setMatchIdOffsetCmdIf->manage();

    XtVaCreateManagedWidget("nextLabel", xmLabelWidgetClass, rc1, NULL);
    sprintf(buf, "%d", _matchManager->getStartId());
    Cmd *nextCmd = new TpSetMatchIdNextCmd("setMatchIdNext",
                        True, (CmdValue)buf, _matchManager);
    CmdInterface *setMatchIdNextCmdIf = new StringKeyinInterface(rc1, nextCmd);
    nextCmd->newValue();
    setMatchIdNextCmdIf->manage();

    CmdInterface *ciGen = new TpQualFormatCmdInterface(rc, _cmdGen);
    ciGen->setDeferredExec(_applyCmdList);
    ciGen->manage();

    /////////////////////////////////////////////////////////////////////////
    // Point Qualifier Part
    /////////////////////////////////////////////////////////////////////////

    _cmdPnt = new TpQualFormatCmd("pntQualFormatCmd", True,
                                      _matchManager->getPointQualMgr());
    Widget rc2 = XtVaCreateManagedWidget("rcPntQualExtra",
			xmRowColumnWidgetClass, rc,
			XmNorientation, XmHORIZONTAL,
			XmNnumColumns, 1,
			XmNpacking, XmPACK_TIGHT,
			NULL);
    CmdInterface *corParAsPnt = new TpAddCorrParmAsPntQualInterface(rc2, _cmdPnt);
    corParAsPnt->manage();

    CmdInterface *ciPnt = new TpQualFormatCmdInterface(rc, _cmdPnt);
    ciPnt->setDeferredExec(_applyCmdList);
    ciPnt->manage();
     
    return rc;
}

void TpQualFormatDialog::post()
{
    if (_cmdGen) _cmdGen->collectValue();
    if (_cmdPnt) _cmdPnt->collectValue();

    CustomDialog::post();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualFormatCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualFormatValue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualFormatCmdInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpQualFormatCmdInterface.cc:  
//////////////////////////////////////////////////////////////////////////////
#include "TpQualFormatCmdInterface.h"
#include "TpQualFormatView.h"
#include "TpQualFormatValue.h"
#include "TpDefs.h"
#include "KeyinView.h"
#include "Cmd.h"
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <Xm/TextF.h>
#include <stdlib.h>
#include <stdio.h>

TpQualFormatCmdInterface::TpQualFormatCmdInterface(Widget parent, Cmd *cmd) 
	: CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name, xmFrameWidgetClass, parent, NULL);
    installDestroyHandler();

    XtVaCreateManagedWidget("qualFormatFrameLabel",
                            xmLabelGadgetClass, _w,
                            XmNchildType, XmFRAME_TITLE_CHILD,
                            XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
                            NULL);

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
 
    if (_active)
        activate();
    else
        deactivate();

    Widget form = XtVaCreateManagedWidget("formQualFormat", 
					  xmFormWidgetClass, _w,
					  NULL);

    _numQualsView = new KeyinView(form, "numQualsView");
    _numQualsView->installCallback(
	&TpQualFormatCmdInterface::setNumQualsCallback, (XtPointer)this);
    XtVaSetValues(_numQualsView->baseWidget(), 
		  XmNtopAttachment, XmATTACH_FORM, 
		  XmNleftAttachment, XmATTACH_FORM, 
		  XmNrightAttachment, XmATTACH_NONE,
		  XmNbottomAttachment, XmATTACH_NONE,
		  NULL);
    _numQualsView->manage();

    _qualView = new TpQualFormatView(form, "qualFormatView", this);
    XtVaSetValues(_qualView->baseWidget(),
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, _numQualsView->baseWidget(),
		  XmNleftAttachment, XmATTACH_FORM, 
		  XmNrightAttachment, XmATTACH_FORM, 
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
    _qualView->manage();

    setValue(_cmd->getValue());
}

void TpQualFormatCmdInterface::addQualCallback(Widget, 
		XtPointer clientData, XtPointer)
{
    TpQualFormatCmdInterface *obj;
    obj = (TpQualFormatCmdInterface *)clientData;
    if (obj != NULL)
        obj->addQual();
}

void TpQualFormatCmdInterface::deleteQualCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpQualFormatCmdInterface *obj;
    obj = (TpQualFormatCmdInterface *)clientData;
    if (obj != NULL)
        obj->deleteQual();
}

void TpQualFormatCmdInterface::setNumQualsCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpQualFormatCmdInterface *obj;
    obj = (TpQualFormatCmdInterface *)clientData;
    if (obj != NULL)
        obj->setNumQuals();
}

void TpQualFormatCmdInterface::addQual()
{
    
}

void TpQualFormatCmdInterface::setNumQuals()
{
    int n = atoi(_numQualsView->getFieldValue());
    //_value = new TpQualFormatValue(*((TpQualFormatValue *)(_cmd->getValue())));
    _value->setNumQuals(n);
    _qualView->setQuals(_value);
    runCmd((CmdValue)_value);
}

void TpQualFormatCmdInterface::deleteQual()
{
    
}

void TpQualFormatCmdInterface::setName(char *name, int n)
{
    _value->_info[n]._qualName = sdup(name);
    runCmd((CmdValue)_value);
}

void TpQualFormatCmdInterface::setUnit(char *unit, int n)
{
    _value->_info[n]._qualUnit = sdup(unit);
    runCmd((CmdValue)_value);
}

void TpQualFormatCmdInterface::setType(TpQualType t, int n)
{
    _value->_info[n]._qualType = t;
    runCmd((CmdValue)_value);
}

void TpQualFormatCmdInterface::setValue(CmdValue v)
{
    _value = new TpQualFormatValue(*((TpQualFormatValue *)v));

    char buf[16];
    sprintf(buf, "%d", _value->getNumQuals());
    _numQualsView->setFieldValue(buf);

    _qualView->setQuals(_value);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualFormatView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpQualFormatView.cc
//////////////////////////////////////////////////////////////////////////////
#include "TpQualFormatView.h"
#include "TpQualFormatSingleView.h"
#include "TpQualFormatValue.h"
#include "TpQualFormatCmdInterface.h"
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <stdio.h>

TpQualFormatView::TpQualFormatView(Widget parent, const char *name, 
				   TpQualFormatCmdInterface *ci)
    : UIComponent(name)
{
    _ci = ci;

    _w = XtVaCreateWidget(_name, xmFormWidgetClass, parent, 
			  NULL);
    installDestroyHandler();

    Widget title = XtVaCreateManagedWidget("title", 
					   xmFormWidgetClass, _w,
					   XmNtopAttachment, XmATTACH_FORM,
					   XmNleftAttachment, XmATTACH_FORM, 
					   XmNrightAttachment, XmATTACH_FORM,
					   XmNbottomAttachment, XmATTACH_NONE,
					   NULL);
    Widget titleName = XtVaCreateManagedWidget("titleName",
					 xmLabelWidgetClass, title, 
					 XmNtopAttachment, XmATTACH_FORM,
					 XmNbottomAttachment, XmATTACH_FORM,
					 XmNleftAttachment, XmATTACH_POSITION,
					 XmNleftPosition, 0,
					 NULL);
    Widget titleType = XtVaCreateManagedWidget("titleType",
                                         xmLabelWidgetClass, title, 
					 XmNtopAttachment, XmATTACH_FORM,
                                         XmNbottomAttachment, XmATTACH_FORM,
                                         XmNleftAttachment, XmATTACH_POSITION,
                                         XmNleftPosition, 33,
                                         NULL);
    Widget titleUnit = XtVaCreateManagedWidget("titleUnit",
                                         xmLabelWidgetClass, title, 
					 XmNtopAttachment, XmATTACH_FORM,
                                         XmNbottomAttachment, XmATTACH_FORM,
                                         XmNleftAttachment, XmATTACH_POSITION,
                                         XmNleftPosition, 66,
                                         NULL);

    _formatView = XtVaCreateManagedWidget("formatView", 
					  xmRowColumnWidgetClass, _w,
					  XmNtopAttachment, XmATTACH_WIDGET,
					  XmNtopWidget, title,
					  XmNleftAttachment, XmATTACH_FORM,
					  XmNrightAttachment, XmATTACH_FORM, 
					  XmNbottomAttachment, XmATTACH_FORM,
					  XmNorientation, XmVERTICAL,
					  XmNnumColumns, 1,
					  XmNpacking, XmPACK_TIGHT,
					  NULL);
    _qualInfos = new SL_List<TpQualFormatSingleView *>;
}

void TpQualFormatView::setQuals(TpQualFormatValue *value)
{
    int i;
    int diff = value->getNumQuals() - _qualInfos->get_length();
    if (diff > 0) {
	for (i = _qualInfos->get_length(); i < value->getNumQuals(); i++) {
	    TpQualFormatSingleView *singleView;
	    singleView = new TpQualFormatSingleView(_formatView, 
						    "formatSingleView",
						    this, i);
	    singleView->manage();
	    _qualInfos->add(singleView);
	}
    }
    if (diff < 0) {
	for (i = 0; i < (-1) * diff; i++) {
	    TpQualFormatSingleView *view = _qualInfos->remove_first();
	    view->unmanage();
	    delete view;
	}
    }

    SL_ListWatch<TpQualFormatSingleView *> w;
    _qualInfos->init_scan(&w);
    TpQualFormatSingleView *view;
    i = 0;
    while((view = _qualInfos->next()) != NULL) {
	view->setNumber(i);
	view->setName(value->_info[i]._qualName, False);
	view->setUnit(value->_info[i]._qualUnit, False);
	view->setType(value->_info[i]._qualType);
	i++;
    }
}

void TpQualFormatView::setName(char *name, int n)
{
    _ci->setName(name, n);
}

void TpQualFormatView::setUnit(char *unit, int n)
{
    _ci->setUnit(unit, n);
}

void TpQualFormatView::setType(TpQualType t, int n)
{
    _ci->setType(t, n);
}

#if defined(vms) || defined(__VMS)
#pragma define_template SL_List<TpQualFormatSingleView *>
#pragma define_template SL_ListWatch<TpQualFormatSingleView *>
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpQualFormatSingleView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpQualFormatSingleView.cc
//////////////////////////////////////////////////////////////////////////////
#include "TpQualFormatSingleView.h"
#include "TpQualFormatView.h"
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include <stdio.h>

TpQualFormatSingleView::TpQualFormatSingleView(Widget parent, const char *name, 
				   TpQualFormatView *view, int n)
    : UIComponent(name)
{
    _view = view;
    _n = n;
    
    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent, 
			  XmNorientation, XmHORIZONTAL,
			  XmNnumColumns, 1,
			  NULL);
    installDestroyHandler();

    _wname = XtVaCreateManagedWidget("name", xmTextFieldWidgetClass, _w, NULL);
    XtAddCallback(_wname, XmNlosingFocusCallback, 
		  &TpQualFormatSingleView::setNameCallback,
		  (XtPointer)this);
    XtAddCallback(_wname, XmNactivateCallback, 
		  &TpQualFormatSingleView::setNameCallback,
                  (XtPointer)this);

    Widget radioBox = XtVaCreateManagedWidget("qualFormatRadioBox", 
		xmRowColumnWidgetClass, _w,
                XmNorientation, XmHORIZONTAL,
                XmNradioBehavior, True,
                XmNradioAlwaysOne, True,
                NULL );
    _wreal = XtVaCreateManagedWidget("real", 
                xmToggleButtonWidgetClass, radioBox,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, True,
                NULL);
    _wfull = XtVaCreateManagedWidget("full",
                xmToggleButtonWidgetClass, radioBox,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);
    _wtext = XtVaCreateManagedWidget("text",
                xmToggleButtonWidgetClass, radioBox,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);

    XtAddCallback(_wreal, XmNvalueChangedCallback, 
		  &TpQualFormatSingleView::setTypeCallback, 
		  (XtPointer)this);
    XtAddCallback(_wfull, XmNvalueChangedCallback, 
                  &TpQualFormatSingleView::setTypeCallback,
                  (XtPointer)this);
    XtAddCallback(_wtext, XmNvalueChangedCallback, 
                  &TpQualFormatSingleView::setTypeCallback,
                  (XtPointer)this);

    _wunit = XtVaCreateManagedWidget("unit", xmTextFieldWidgetClass, _w, NULL);
    XtAddCallback(_wunit, XmNlosingFocusCallback,
                  &TpQualFormatSingleView::setUnitCallback,
                  (XtPointer)this);
    XtAddCallback(_wunit, XmNactivateCallback,
                  &TpQualFormatSingleView::setUnitCallback,
                  (XtPointer)this);
}

void TpQualFormatSingleView::setNameCallback(Widget w, 
					     XtPointer clientData, XtPointer)
{
    TpQualFormatSingleView *obj;
    obj = (TpQualFormatSingleView *)clientData;
    if (obj != NULL)
	obj->setName(XmTextFieldGetString(w));
}

void TpQualFormatSingleView::setUnitCallback(Widget w, 
                                             XtPointer clientData, XtPointer)
{
    TpQualFormatSingleView *obj;
    obj = (TpQualFormatSingleView *)clientData;
    if (obj != NULL)
        obj->setUnit(XmTextFieldGetString(w));
}

void TpQualFormatSingleView::setTypeCallback(Widget,
					     XtPointer clientData, XtPointer)
{
    TpQualFormatSingleView *obj;
    obj = (TpQualFormatSingleView *)clientData;
    if (obj != NULL)
	obj->setType();
}

void TpQualFormatSingleView::setName(char *name, Boolean doUpdate)
{
    XmTextFieldSetString(_wname, name);
    if (doUpdate)
	_view->setName(name, _n);
}

void TpQualFormatSingleView::setUnit(char *unit, Boolean doUpdate)
{
    XmTextFieldSetString(_wunit, unit);
    if (doUpdate)
	_view->setUnit(unit, _n);
}

// Process value that came from outside (no view update necessary)

void TpQualFormatSingleView::setType(TpQualType type)
{
    if (type == TpReal)
	XmToggleButtonSetState(_wreal, True, False);
    else
	XmToggleButtonSetState(_wreal, False, False);

    if (type == TpFull)
        XmToggleButtonSetState(_wfull, True, False);
    else
        XmToggleButtonSetState(_wfull, False, False);

    if (type == TpText)
        XmToggleButtonSetState(_wtext, True, False);
    else
        XmToggleButtonSetState(_wtext, False, False);
}

// Process user input

void TpQualFormatSingleView::setType()
{
    if (XmToggleButtonGetState(_wreal)) {
	_view->setType(TpReal, _n);
    }
    if (XmToggleButtonGetState(_wfull)) {
        _view->setType(TpFull, _n);
    }
    if (XmToggleButtonGetState(_wtext)) {
        _view->setType(TpText, _n);
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpAddIdAsGenQualInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpAddIdAsGenQualInterface.cc:  
//////////////////////////////////////////////////////////////////////////////
#include "TpAddIdAsGenQualInterface.h"
#include "TpQualFormatView.h"
#include "TpQualFormatValue.h"
#include "TpDefs.h"
#include "Cmd.h"
#include <Xm/PushB.h>
#include <stdlib.h>
#include <stdio.h>

TpAddIdAsGenQualInterface::TpAddIdAsGenQualInterface(Widget parent, Cmd *cmd) 
	: CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name, xmPushButtonWidgetClass, parent, NULL);
    installDestroyHandler();

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
 
    if (_active)
        activate();
    else
        deactivate();

    XtAddCallback(_w, XmNactivateCallback,
	&TpAddIdAsGenQualInterface::addIdAsGenQualCallback, (XtPointer)this);

   _value = new TpQualFormatValue(*((TpQualFormatValue *)_cmd->getValue())); 
}

void TpAddIdAsGenQualInterface::addIdAsGenQualCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpAddIdAsGenQualInterface *obj;
    obj = (TpAddIdAsGenQualInterface *)clientData;
    if (obj != NULL)
        obj->addIdAsGenQual();
}

void TpAddIdAsGenQualInterface::addIdAsGenQual()
{
    if (_value->getNumQuals() == 0) {
    	_value->addQualInfo("ID", TpFull, "None");
        runCmd((CmdValue)_value);
    }
}

void TpAddIdAsGenQualInterface::setValue(CmdValue v)
{
    _value = new TpQualFormatValue(*((TpQualFormatValue *)v));
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create TpAddCorrParmAsPntQualInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// TpAddCorrParmAsPntQualInterface.cc:  
//////////////////////////////////////////////////////////////////////////////
#include "TpAddCorrParmAsPntQualInterface.h"
#include "TpQualFormatView.h"
#include "TpQualFormatValue.h"
#include "TpDefs.h"
#include "Cmd.h"
#include <Xm/PushB.h>
#include <stdlib.h>
#include <stdio.h>

TpAddCorrParmAsPntQualInterface::TpAddCorrParmAsPntQualInterface(Widget parent, 
					Cmd *cmd) 
	: CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name, xmPushButtonWidgetClass, parent, NULL);
    installDestroyHandler();

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
 
    if (_active)
        activate();
    else
        deactivate();

    XtAddCallback(_w, XmNactivateCallback,
	&TpAddCorrParmAsPntQualInterface::addCorrParmAsPntQualCallback, 
	(XtPointer)this);

   _value = new TpQualFormatValue(*((TpQualFormatValue *)_cmd->getValue())); 
}

void TpAddCorrParmAsPntQualInterface::addCorrParmAsPntQualCallback(Widget,
                XtPointer clientData, XtPointer)
{
    TpAddCorrParmAsPntQualInterface *obj;
    obj = (TpAddCorrParmAsPntQualInterface *)clientData;
    if (obj != NULL)
        obj->addCorrParmAsPntQual();
}

void TpAddCorrParmAsPntQualInterface::addCorrParmAsPntQual()
{
    if (_value->getNumQuals() == 0) {
    	_value->addQualInfo("Corr_Parm", TpReal, "None");
        runCmd((CmdValue)_value);
    }
}

void TpAddCorrParmAsPntQualInterface::setValue(CmdValue v)
{
    _value = new TpQualFormatValue(*((TpQualFormatValue *)v));
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tp_qual.imake
#define SUBROUTINE tp_qual
#define MODULE_LIST \
 TpQualifier.cc TpQualGroup.cc TpQualGroupMgr.cc \
 TpQualFormatDialog.cc TpQualFormatCmd.cc TpQualFormatValue.cc
#define MODULE_LIST2 \
 TpQualFormatCmdInterface.cc TpQualFormatView.cc TpQualFormatSingleView.cc \
 TpAddIdAsGenQualInterface.cc TpAddCorrParmAsPntQualInterface.cc

#define P2_SUBLIB

#define MAIN_LANG_C_PLUS_PLUS
#define USES_C_PLUS_PLUS
#define CCC_TEMPLATES

#define LIB_P2SUB
#define LIB_GUISUB
#define LIB_MOTIFAPP
#define LIB_MOTIF
#define LIB_XMU

$ Return
$!#############################################################################
