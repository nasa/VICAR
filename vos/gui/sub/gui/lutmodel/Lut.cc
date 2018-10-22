//////////////////////////////////////////////////
// Lut.cc: model class for LUT
///////////////////////////////////////////////////
#include "Lut.h"
#include "ViewMacros.h"
#include "LutView.h"
#include <iostream>
using namespace std;

Lut::Lut ( )
{
        _numViews = 0;
        _views = NULL;
        _lut = NULL;

	_lowerLimit = 0;
	_upperLimit = 255;
	ramp ( );
}

Lut::~Lut ( )
{
	if (_views) delete _views;
        delete [] _lut;
}

void Lut::attachView (LutView *view)
{
        AttachViewMacro(LutView, _views, _numViews, view);
        view->update ( );
}

void Lut::detachView (LutView *view)
{
        DetachViewMacro(LutView, _views, _numViews, view);
}

void Lut::updateViews( )
{
      int i;
      for (i=0; i<_numViews; i++)
              _views[i]->update ( );
}

int Lut::operator [] ( int i )
{
        return _lut[i];
}

void Lut::setAsArray (int *array)
{
	if (!_lut) _lut = new int [256];
	if (!_lut) cerr << "Memory allocation error\n";
	for (int i = 0; i < 256; i++)
		_lut[i] = array[i];

	updateViews();
}

void Lut::ramp ( )
{
	if (!_lut) _lut = new int [256];
	if (!_lut) cerr << "Allocation error\n";
	for (int i = 0; i < 256; i++)
		_lut[i] = i;

	updateViews();
}

void Lut::setDN ( int index, int newDN )
{
	_lut[index] = newDN;

	updateViews();
}
