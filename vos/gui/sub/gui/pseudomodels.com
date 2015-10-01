$!****************************************************************************
$!
$! Build proc for MIPL module pseudomodels
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:31
$!
$! Execute by entering:		$ @pseudomodels
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
$ write sys$output "*** module pseudomodels ***"
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
$ write sys$output "Invalid argument given to pseudomodels.com file -- ", primary
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
$   if F$SEARCH("pseudomodels.imake") .nes. ""
$   then
$      vimake pseudomodels
$      purge pseudomodels.bld
$   else
$      if F$SEARCH("pseudomodels.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pseudomodels
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pseudomodels.bld "STD"
$   else
$      @pseudomodels.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pseudomodels.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pseudomodels.com -mixed -
	-s PseudoMarks.cc PseudoValue.cc -
	-i pseudomodels.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create PseudoMarks.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////
// PseudoMarks.cc: model class for wedge marks
///////////////////////////////////////////////////
#include "PseudoMarks.h"
#include "ViewMacros.h"
#include "BasicWedgeOverlay.h"
#include "InterpolationChooser.h"
#include <stdlib.h>
#include <iostream>
using namespace std;

PseudoMarks::PseudoMarks ( )
{
        _numViews = 0;
	_numViews1 = 0;
        _views = NULL;
	_views1 = NULL;

	_numMarks = 0;
        _marks = NULL;
	_current = 0;
	_start = _end = _start1 = 0;
}

PseudoMarks::~PseudoMarks ( )
{
        delete [] _marks;
}

void PseudoMarks::attachView (BasicWedgeOverlay *view)
{
        AttachViewMacro(BasicWedgeOverlay, _views, _numViews, view);
        view->update ( this );
}

void PseudoMarks::attachView (InterpolationChooser *view)
{
	AttachViewMacro(InterpolationChooser, _views1, _numViews1, view);
	view->update ( this );
}

void PseudoMarks::detachView (BasicWedgeOverlay *view)
{
        DetachViewMacro(BasicWedgeOverlay, _views, _numViews, view);
}

void PseudoMarks::detachView (InterpolationChooser *view)
{
        DetachViewMacro(InterpolationChooser, _views1, _numViews1, view);
}

void PseudoMarks::updateViews( )
{
      int i;
      for (i=0; i<_numViews; i++)
              _views[i]->update ( this );
}

void PseudoMarks::updateViews1( )
{
      for (int i=0; i<_numViews1; i++) 
              if (_views1[i]) _views1[i]->update ( this );
}

int *PseudoMarks::getDnAsArray ( )
{
	if ((_marks == NULL) || (_numMarks == 0)) 
		return NULL;

	int *array = new int [_numMarks];
	for (int i = 0; i < _numMarks; i++) {
		array[i] = _marks[i].dn;
	}
	return array;
}

int PseudoMarks::addMark ( int value, int delta, 
	int red, int grn, int blu, InterpolationType type)
{
	if ( (value > 255) || (value < 0) )
	  return 0;

	int found = 0;

	if (_marks) 
	  for (int i = 0; i < _numMarks; i++) {
		if ( (abs(_marks[i].dn - value) <= delta) && (!found) ) {
			found = 1;
			if ( (_marks[i].dn != 0) && (_marks[i].dn != 255) )
				_marks[i].dn = value;
			else {		// we are at dn=0 or dn=255
				_marks[i].red = red;
				_marks[i].grn = grn;
				_marks[i].blu = blu;
			}
			setCurrent(i+1);	// make existing mark current
	    	}
		if (found) continue;
	  }

	if (found == 0) {	// add new mark
		Mark *newMarks;
		newMarks = new Mark [_numMarks + 1];
		for (int j = 0; j < _numMarks; j++)
			newMarks[j] = _marks[j]; // I hope all compilers will like it,
						 // otherwise use the line below
			//copyMark(_marks[j], newMarks[j]);
		delete _marks;
		_marks = newMarks;
		_marks[_numMarks].dn = value;
		_marks[_numMarks].type = type;
		_marks[_numMarks].red = red;
		_marks[_numMarks].grn = grn;
		_marks[_numMarks].blu = blu;
		_numMarks++;
		setCurrent(_numMarks);
	}

	markInterval(value);

	updateViews();

	return found;
}

// Move current mark to a new location
void PseudoMarks::moveMark ( int to )
{
	if ( (to > 255) || (to < 0) )
		deleteMark ();
	else 
		if ( (_marks[_current-1].dn == 0) || (_marks[_current-1].dn == 255) )
			addMark(to, 0, _marks[_current-1].red, _marks[_current-1].grn,
				_marks[_current-1].blu);
		else
			_marks[_current-1].dn = to;

	updateViews();
}

// DeleteMark function returns True if the current mark has been
// successfully deleted, otherwise returns False 
int PseudoMarks::deleteMark ( )	// deletes current mark
{
	int deleted = False;	// a flag to see if mark has been deleted

	Mark *newMarks;
        newMarks = new Mark [_numMarks - 1];
	int i = 0;
	for (int j = 0; j < _numMarks; j++)
	   if ( (j != _current-1) || (_marks[j].dn == 0) || (_marks[j].dn == 255) ) {
		newMarks[i] = _marks[j]; i++;
	   }
	   else deleted = True;
	if (deleted) {
           delete _marks;
           _marks = newMarks;
           _numMarks--;
	   markInterval(255);
	   updateViews();
	}

	return deleted;
}

// clearMarks function deletes all the marks except those
// with DN 0 and 255
void PseudoMarks::clearMarks()
{
	Mark *newMarks;
        newMarks = new Mark [2];	// for marks with dn = 0 and 255
	int i = 0;
	for (int j = 0; j < _numMarks; j++)
		if ( (_marks[j].dn == 0) || (_marks[j].dn == 255) ) {
			newMarks[i] = _marks[j]; 
			newMarks[i].type = NONE; 
			i++;
		}

	delete _marks;
	_marks = newMarks;
	_numMarks = 2;
	markInterval(255);
	updateViews();
}

void PseudoMarks::setCurrent ( int current )
{
	_current = current;
	updateViews(); 
	updateViews1();
}

// Mark new interval around the two nearest points
void PseudoMarks::markInterval ( int pos )
{
	int newDist;
	int i;
	// find the starting point
	int curDist = 256;		// some large number
	for (i = 0; i < _numMarks; i++) {
	   newDist = pos - _marks[i].dn;
	   if ( (newDist >= 0) && (newDist < curDist) ) {
		curDist = newDist;
		_start = i;
	   }
	}

	// find the ending point
	curDist = -256;              	// reset to small number
	for (i = 0; i < _numMarks; i++) {
	   newDist = pos - _marks[i].dn;
	   if ( (newDist < 0) && (newDist > curDist) ) {
		curDist = newDist;
		_end = _start1 = i;
	   }
	}
        // match the current mark with the beginning of the interval
	// No need to call updateViews() since setCurrent() will do that
        setCurrent (_start + 1);
}

// Expand or decrease current interval as a result of "drag" event
void PseudoMarks::resizeInterval ( int pos )
{
	int i;
	int newDist;
	int curDist;
	if (pos >= _marks[_start].dn)	{	// extend the interval to the next mark
	  curDist = -256;	// some small number
	  for (i = 0; i < _numMarks; i++) {
            newDist = pos - _marks[i].dn;
            if ( (newDist < 0) && (newDist > curDist) ) {
                curDist = newDist;
                _end = i;
            }
          }
	}
	else if (pos < _marks[_start].dn) { // extend the interval to the prev. mark
	  curDist = 256;	// some large number
	  for (i = 0; i < _numMarks; i++) {
            newDist = pos - _marks[i].dn;
            if ( (newDist >= 0) && (newDist < curDist) ) {
                curDist = newDist;
                _end = i;
            }
          }
	}
	else {
          curDist = -256;       // some small number
          for (i = 0; i < _numMarks; i++) {
            newDist = pos - _marks[i].dn;
            if ( (newDist < 0) && (newDist > curDist) ) {
                curDist = newDist;
                _end = i;
            }
          }
	}

	updateViews();
}

void PseudoMarks::setInterpolation (InterpolationType type)
{
	int i;
	for (i=0; i<_numMarks; i++)
	    if ( (_marks[i].dn >= _marks[_start].dn) && (_marks[i].dn < _marks[_end].dn) )
		_marks[i].type = type;
}

void PseudoMarks::copyMark (Mark from, Mark &to)
{
	to.dn   = from.dn;
	to.red  = from.red;
	to.grn  = from.grn;
	to.blu  = from.blu;
	to.type = from.type;
}

// Returns Mark number at specified DN value, or 
// returns 0 if no mark was found
int PseudoMarks::find ( int dn )
{
	int i;
	for (i=0; i<_numMarks; i++)
		if (_marks[i].dn == dn)
			return ++i;
	return 0;
}

int PseudoMarks::findNextDn (int dn)
{
	int nextDn = 255;
	int index = find(dn) - 1;
	int i;
	for (i=0; i<_numMarks; i++) {
		int dif = _marks[i].dn - _marks[index].dn;
		if ( (dif > 0) && (dif < (nextDn-_marks[index].dn)) )
			nextDn=_marks[i].dn;
	}
	return nextDn;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PseudoValue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// PseudoValue.cc: Contains three pseudocolor tables
////////////////////////////////////////////////////////
#include "PseudoValue.h"
#include "PseudoDefs.h"
#include "BasicWedgeOverlay.h"
#include "PseudoMarks.h"
#include "ViewMacros.h"
#include "ibisfile.h"
#include "file_no_path.h"
#include "zvproto.h"
#include <iostream>
using namespace std;
#include <stdio.h>
#include <ctype.h>

PseudoValue::PseudoValue ( char *filename)
{
        _numViews = 0;
        _views = NULL;
	_pseudoR = _defR = new int [256];
	_pseudoG = _defG = new int [256];
	_pseudoB = _defB = new int [256];

	if (!filename) return;

	if (!loadFile(filename)) {
		for (int i = 0; i < 256; i++) {
			_pseudoR[i] = _defR[i] = i;
			_pseudoG[i] = _defG[i] = i;
			_pseudoB[i] = _defB[i] = i;
		}
	}
}

PseudoValue::~PseudoValue()
{
	delete [] _pseudoR;
	delete [] _pseudoG;
	delete [] _pseudoB;
	delete [] _defR;
	delete [] _defG;
	delete [] _defB;
}

void PseudoValue::attachView (BasicWedgeOverlay *view)
{
        AttachViewMacro(BasicWedgeOverlay, _views, _numViews, view);
        view->update ( this );
}

void PseudoValue::detachView (BasicWedgeOverlay *view)
{
        DetachViewMacro(BasicWedgeOverlay, _views, _numViews, view);
}

void PseudoValue::updateViews( )
{
	for (int i=0; i<_numViews; i++)
	    _views[i]->update ( this );
}

PseudoValue::PseudoValue(PseudoValue &val)	// Copy constructor
{
	memcpy((void *)this, (void *)&val, sizeof(PseudoValue));
	   
	if (val._pseudoR) {
	   _pseudoR = new int[256];
	   for (int i=0; i<256; i++)
		_pseudoR[i] = val._pseudoR[i];
	}
	if (val._pseudoG) {
           _pseudoG = new int[256];
           for (int i=0; i<256; i++)
                _pseudoG[i] = val._pseudoG[i];
        }
	if (val._pseudoB) {
           _pseudoB = new int[256];
           for (int i=0; i<256; i++)
                _pseudoB[i] = val._pseudoB[i];
        }

        if (val._defR) {
           _defR = new int[256];
           for (int i=0; i<256; i++)
                _defR[i] = val._defR[i];
        }
        if (val._defG) {
           _defG = new int[256];
           for (int i=0; i<256; i++)
                _defG[i] = val._defG[i];
        }
        if (val._defB) {
           _defB = new int[256];
           for (int i=0; i<256; i++)
                _defB[i] = val._defB[i];
        }
}

PseudoValue &PseudoValue::operator=(PseudoValue &val)
{
	if (this == &val)
	   return *this;		// assignment to self

	if (_pseudoR)
	   delete []_pseudoR;
	if (_pseudoG)
           delete []_pseudoG;
	if (_pseudoB)
           delete []_pseudoB;

	memcpy((void *)this, (void *)&val, sizeof(PseudoValue));

        if (val._pseudoR) {
           _pseudoR = new int[256]; 
           for (int i=0; i<256; i++)
                _pseudoR[i] = val._pseudoR[i];
        }  
        if (val._pseudoG) { 
           _pseudoG = new int[256];
           for (int i=0; i<256; i++)
                _pseudoG[i] = val._pseudoG[i];
        }  
        if (val._pseudoB) { 
           _pseudoB = new int[256];
           for (int i=0; i<256; i++)
                _pseudoB[i] = val._pseudoB[i];
        }  

        if (val._defR) {
           _defR = new int[256];
           for (int i=0; i<256; i++)
                _defR[i] = val._defR[i];
        }
        if (val._defG) {
           _defG = new int[256];
           for (int i=0; i<256; i++)
                _defG[i] = val._defG[i];
        }
        if (val._defB) {
           _defB = new int[256];
           for (int i=0; i<256; i++)
                _defB[i] = val._defB[i];
        }

	return *this;
}

void PseudoValue::setRedAsArray ( int *array )
{
	int i;
	for (i=0; i<256; i++)
		_pseudoR[i] = array[i];
	updateViews();
}

void PseudoValue::setGrnAsArray ( int *array )
{
        int i;
        for (i=0; i<256; i++)
                _pseudoG[i] = array[i];
        updateViews();
}

void PseudoValue::setBluAsArray ( int *array )
{
        int i;
        for (i=0; i<256; i++)
                _pseudoB[i] = array[i];
        updateViews();
}

void PseudoValue::setDefRedAsArray ( int *array )
{
        int i;
        for (i=0; i<256; i++)
                _defR[i] = array[i];
        updateViews();
}

void PseudoValue::setDefGrnAsArray ( int *array )
{
        int i;
        for (i=0; i<256; i++)
                _defG[i] = array[i];
        updateViews();
}

void PseudoValue::setDefBluAsArray ( int *array )
{
        int i;
        for (i=0; i<256; i++)
                _defB[i] = array[i];
        updateViews();
}

void PseudoValue::setRedDn ( int index, int newDn )
{
        _pseudoR[index] = newDn;
        updateViews();
}

void PseudoValue::setGrnDn ( int index, int newDn )
{
        _pseudoG[index] = newDn;
        updateViews();
}

void PseudoValue::setBluDn ( int index, int newDn )
{
        _pseudoB[index] = newDn;
        updateViews();
}

void PseudoValue::setRGBDn ( int index, int red, int grn, int blu )
{
	_pseudoR[index] = red;
	_pseudoG[index] = grn;
	_pseudoB[index] = blu;
	updateViews();
}

int PseudoValue::getRGBDn ( int index, int *red, int *grn, int *blu)
{
	if ( (index < 0) || (index > 255) ) {
		*red = *grn = *blu = 0;
		return 0;
	}
	else {
		*red = _pseudoR[index];
		*grn = _pseudoG[index];
		*blu = _pseudoB[index];
		return 1;
	}
}

void PseudoValue::regenerate ( PseudoMarks* marks )
{
	// In order to regenerate values, we need at least two marks at 
	// 0 and 255 dn.
	if ( (marks->getNumMarks() < 2) || (marks->find(0) == 0) || 
		(marks->find(255) == 0) ) return;

	// The goal is to go from mark to mark (left to right) and set the 
	// pseudocolor values according to the mark's color value and interpolation
	int dn = 0;		// starting point
	int markNo;
	while (dn != 255) {	// ending point
		markNo = marks->find(dn);
		int nextDn = marks->findNextDn(dn);

		if ( marks->getInterpolation(markNo) == NONE) {
			_pseudoR[marks->getDn(markNo)] = marks->getRed(markNo);
			_pseudoG[marks->getDn(markNo)] = marks->getGrn(markNo);
			_pseudoB[marks->getDn(markNo)] = marks->getBlu(markNo);
			restoreDefault (marks->getDn(markNo), nextDn );
		}

		if ( marks->getInterpolation(markNo) == FLAT ) {
			_pseudoR[marks->getDn(markNo)] = marks->getRed(markNo);
                        _pseudoG[marks->getDn(markNo)] = marks->getGrn(markNo);
                        _pseudoB[marks->getDn(markNo)] = marks->getBlu(markNo);
			flat ( marks->getRed(markNo), marks->getGrn(markNo), 
				marks->getBlu(markNo), marks->getDn(markNo), nextDn );
		}

		if ( marks->getInterpolation(markNo) == LINEAR ) {
			_pseudoR[marks->getDn(markNo)] = marks->getRed(markNo);
                        _pseudoG[marks->getDn(markNo)] = marks->getGrn(markNo);
                        _pseudoB[marks->getDn(markNo)] = marks->getBlu(markNo);
			int nextMarkNo = marks->find(nextDn);
			linear (marks->getRed(markNo), marks->getRed(nextMarkNo),
				marks->getGrn(markNo), marks->getGrn(nextMarkNo), 
				marks->getBlu(markNo), marks->getBlu(nextMarkNo),
				marks->getDn(markNo), nextDn );
		}

		if ( marks->getInterpolation(markNo) == CUBS ) {
		// Not implemented yet
		}
		dn = nextDn;
	}
	// Process the last mark at 255
	markNo = marks->find(255);
	_pseudoR[255] = marks->getRed(markNo);
	_pseudoG[255] = marks->getGrn(markNo);
	_pseudoB[255] = marks->getBlu(markNo);

	updateViews();
}

///////////////////////////////////////////////////////////////////////////
// Restore default values between two points
///////////////////////////////////////////////////////////////////////////
void PseudoValue::restoreDefault ( int start, int end )
{
	for (int i = start+1; i < end; i++) {
		_pseudoR[i] = _defR[i];
		_pseudoG[i] = _defG[i];
		_pseudoB[i] = _defB[i];
	}
	updateViews();
}

///////////////////////////////////////////////////////////////////////////
// Do flat interpolation between two points
///////////////////////////////////////////////////////////////////////////
void PseudoValue::flat ( int valueR, int valueG, int valueB, int startDn, int endDn )
{
	for (int i = startDn+1; i < endDn; i++) {
		_pseudoR[i] = valueR;
		_pseudoG[i] = valueG;
		_pseudoB[i] = valueB;
	}
	updateViews();
}

///////////////////////////////////////////////////////////////////////////
// Do linear interpolation between two points
///////////////////////////////////////////////////////////////////////////
void PseudoValue::linear ( int startRedValue, int endRedValue, int startGrnValue, 
	int endGrnValue, int startBluValue, int endBluValue, int startDn, int endDn )
{
	for (int i = startDn+1; i < endDn; i++) {
              _pseudoR[i] = ( (i-startDn)*(endRedValue-startRedValue) +
                        startRedValue*(endDn-startDn) ) / (endDn-startDn);
              _pseudoG[i] = ( (i-startDn)*(endGrnValue-startGrnValue) +
                        startGrnValue*(endDn-startDn) ) / (endDn-startDn);
              _pseudoB[i] = ( (i-startDn)*(endBluValue-startBluValue) +
                        startBluValue*(endDn-startDn) ) / (endDn-startDn);
	}
	updateViews();
}

/////////////////////////////////////////////////////////////////////////////
// Load an IBIS file.  Return 1 on success.
////////////////////////////////////////////////////////////////////////////
int PseudoValue::loadFile(char *filename)
{
        const int SUCCESS = 1;
        const int FAILURE = 0;
	int unit, ibis, status, record, i;
	int lut[256][3];

	if (!filename || !strlen(filename)) return FAILURE;

	// open IBIS file for reading

	status = zvunit(&unit, (char *)"in_file",  1, "u_name", filename, NULL);

	status = IBISFileOpen(unit, &ibis, (char *)IMODE_READ, 0, 0, 0, 0);
	if(status!=1) return FAILURE;

	// Strip directory name from the filename
 
        char *stripFilename = strdup(filename);
        file_no_path(stripFilename);

	ICLGetLOOKUP_TABLE(ibis, (char *)"$MyLut", 0, (char *)"PSEUDOCOLOR",
					stripFilename);
	ICLGetRGB(ibis, (char *)"$MyRED", (char *)"$MyGRN", (char *)"$MyBLU",
						(char *)"$MyLut");

	status = IBISRecordOpen(ibis, &record,
		(char *)"$MyRED | $MyGRN | $MyBLU", 0, 0, (char *)IFMT_FULL);
	if (status != 1) return FAILURE;

	for (i=1; i<=256; i++) {
		status = IBISRecordRead(record, (char*)lut[i-1], i);
		if (status != 1) return FAILURE;
	}

	IBISFileClose(ibis, 0);

	for (i=0; i<256; i++) {
		_pseudoR[i] = _defR[i] = lut[i][0];
		_pseudoG[i] = _defG[i] = lut[i][1];
		_pseudoB[i] = _defB[i] = lut[i][2];
	}

	updateViews();

	delete [] stripFilename;

        return SUCCESS;
}

//////////////////////////////////////////////////////////////////////////
// Save pseudocolor tables in IBIS-formatted file
//////////////////////////////////////////////////////////////////////////
int PseudoValue::saveFile(char *filename)
{
	const int SUCCESS = 1;
        const int FAILURE = 0;
        int unit, i, ibis, status, record;
        int lutcols[3];
	int lut[256][3];

	if (!filename || !strlen(filename)) return FAILURE;

        for (i=0; i<3; i++) lutcols[i]=i+1;

	// Create ibis file

        status = zvunit(&unit, (char *)"out_file",  1, "u_name", filename,NULL);
        status = IBISFileOpen(unit, &ibis, (char *)IMODE_WRITE, 3,
                                        256, 0, (char *)IORG_COLUMN);
        if (status!=1) return FAILURE;

	// Strip directory name from the filename

	char *stripFilename = strdup(filename);
	file_no_path(stripFilename);

        status = IBISFileSet(ibis, (char *)IFILE_TYPE,
					(char *)"LOOKUP_TABLE", 0);
        if (status!=1) return FAILURE;

        status = ICLNewRGB(ibis, 1, 2, 3, 0);
        if (status < 0) return FAILURE;

        status = ICLNewLOOKUP_TABLE(ibis, lutcols, 3, 0, 0, 
				    (char *)"PSEUDOCOLOR", stripFilename);
        if (status < 0) return FAILURE;

	ICLGetLOOKUP_TABLE(ibis, (char *)"$MyLut", 0,
					(char *)"PSEUDOCOLOR", stripFilename);
        ICLGetRGB(ibis, (char *)"$MyRED", (char *)"$MyGRN", (char *)"$MyBLU",
						(char *)"$MyLut");

        status = IBISRecordOpen(ibis, &record,
		(char *)"$MyRED | $MyGRN | $MyBLU", 0, 0, (char *)IFMT_FULL);
        if (status != 1) return FAILURE;

	for (i=0; i<256; i++) {
		lut[i][0]=_pseudoR[i];
		lut[i][1]=_pseudoG[i];
		lut[i][2]=_pseudoB[i];
        }

        for (i=1; i<=256; i++) {
                status = IBISRecordWrite(record, (char*)lut[i-1], i);
                if (status != 1) return FAILURE;
        }

        // close up shop
        status = IBISFileClose( ibis, 0 );
        if (status != 1) return FAILURE;

	delete [] stripFilename;

	return SUCCESS;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pseudomodels.imake
#define SUBROUTINE pseudomodels
#define MODULE_LIST PseudoValue.cc PseudoMarks.cc 

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP
#define LIB_P2SUB
$ Return
$!#############################################################################
