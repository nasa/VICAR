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
