////////////////////////////////////////////////////////////////
// Histogram.cc
////////////////////////////////////////////////////////////////
#include "Histogram.h"
#include "HistView.h"
#include "ViewMacros.h"
#include <assert.h>
#include <iostream>
using namespace std;
#include <stdio.h>
#include <math.h>

////////////////////////////////////////////////////////////////
// Constructors/Destructors
////////////////////////////////////////////////////////////////
Histogram::Histogram(double ll, double ul, double bs)
{
	_isIntRange = False;
	_useBinSize = True;
	_binSize = bs;
	ctor(ll, ul);
}

Histogram::Histogram(int ll, int ul, double bs)
{
	_isIntRange = True;
	_useBinSize = True;
	_binSize = bs;
	ctor(ll, ul + 1);
}

Histogram::Histogram()
{
	_isIntRange = True;
	_useBinSize = False;
	_numBins = 256;
	ctor(0, 256);
}

void Histogram::ctor(double ll, double ul)
{
	_lowerLimit = ll;
	_upperLimit = ul;

	if ( _upperLimit < _lowerLimit ) {
		cerr << "numBins: Upper limit can't be less than lower limit!";
		_lowerLimit = 0; _upperLimit = 256;
	}

	_numViews = 0;

	_views = NULL;
	_bin = NULL;

	allocate ();

	clear();
}

Histogram::~Histogram()
{
	if (_views) delete _views;
	delete [] _bin;
}

////////////////////////////////////////////////////////////////
// Views
////////////////////////////////////////////////////////////////
void Histogram::attachView (HistView *view)
{
	AttachViewMacro(HistView, _views, _numViews, view);
	view->update ( );
}

void Histogram::detachView (HistView *view)
{
	DetachViewMacro(HistView, _views, _numViews, view);
}

void Histogram::updateViews()
{
	int i;
	for (i=0; i<_numViews; i++)
		_views[i]->update ( );
}

////////////////////////////////////////////////////////////////
// Misc. manipulation
////////////////////////////////////////////////////////////////

void Histogram::allocate()
{
	if (_useBinSize)	// Calc numBins based on bin size
		_numBins = int((_upperLimit-_lowerLimit) / _binSize + 0.5);
	else			// Calc _binSize based on number of bins
		_binSize = (_upperLimit - _lowerLimit) / _numBins;

	if (_bin)
		delete [] _bin;
	_bin = new int[_numBins];
	if (!_bin) 
		cerr << "Allocation error\n";
}

void Histogram::clear()
{
	clear_noupdate();
	updateViews();
}

void Histogram::clear_noupdate()
{
        for  ( int ix=0; ix<_numBins; ix++ )
                _bin[ix]=0;
}

////////////////////////////////////////////////////////////////
// Bin value operations
////////////////////////////////////////////////////////////////

int Histogram::operator [] ( int b )
{
	if (b < 0)
		b = 0;
	if (b >= _numBins)
		b = _numBins-1;
	return _bin[b];
}

////////////////////////////////////////////////////////////////
// Functions that manipulate the histogram range (min/max/#bins/bin size)
////////////////////////////////////////////////////////////////
void Histogram::setBinSize (double bs)
{
        _binSize = bs;
	_useBinSize = True;
	allocate();
}

void Histogram::setNumBins(int num)
{
	_numBins = num;
	_useBinSize = False;
	allocate();
}

void Histogram::setLowerLimit (double limit)
{
	_lowerLimit = limit;
	allocate();
}

void Histogram::setUpperLimit (double limit)
{
        _upperLimit = limit;
	_isIntRange = False;
	allocate();
}

void Histogram::setUpperLimit (int limit)
{
        _upperLimit = limit + 1;
	_isIntRange = True;
	allocate();
}

void Histogram::setLimits (double minval, double maxval)
{
        _lowerLimit = minval;
        _upperLimit = maxval;
	_isIntRange = False;
	allocate();
}

void Histogram::setLimits (double minval, double maxval, double binSize)
{
        _lowerLimit = minval;
        _upperLimit = maxval;
	_isIntRange = False;
	_binSize = binSize;
	_useBinSize = True;
	allocate();
}

void Histogram::setLimits (double minval, double maxval, int numBins)
{
        _lowerLimit = minval;
        _upperLimit = maxval;
	_isIntRange = False;
	_numBins = numBins;
	_useBinSize = False;
	allocate();
}

void Histogram::setLimits (int minval, int maxval)
{
        _lowerLimit = minval;
        _upperLimit = maxval + 1;
	_isIntRange = True;
	allocate();
}

void Histogram::setLimits (int minval, int maxval, double binSize)
{
        _lowerLimit = minval;
        _upperLimit = maxval + 1;
	_isIntRange = True;
	_binSize = binSize;
	_useBinSize = True;
	allocate();
}

void Histogram::setLimits (int minval, int maxval, int numBins)
{
        _lowerLimit = minval;
        _upperLimit = maxval + 1;
	_isIntRange = True;
	_numBins = numBins;
	_useBinSize = False;
	allocate();
}

////////////////////////////////////////////////////////////////
// Functions on statistics calculation
////////////////////////////////////////////////////////////////

double Histogram::getMean()
{
	double rsum0, rsum1;
	rsum0 = 0;
	rsum1 = 0;
	double mean;

	int i;
	for (i=0; i<_numBins; i++)
	{
		rsum0 += (double)_bin[i];
		rsum1 += (double)_bin[i] * i;
	}
	if (rsum0 == 0.0)
		mean = 0.0;
	else
	{
		mean = rsum1 / rsum0;
	}

	return mean;
}

double Histogram::getStDev()
{
	double rsum0, rsum2;
	rsum0 = 0;
	rsum2 = 0;
	double stdev;
	double variance;

	for (int i=0; i<_numBins; i++)
	{
		rsum0 += (double)_bin[i];
		rsum2 += (double)_bin[i] * i * i;
	}
	if (rsum0 == 0.0)
		stdev = 0.0;
	else 
	{
		variance = (rsum2 / rsum0) - (getMean() * getMean());
		if (variance < 0.0)
			variance = 0.0;
		stdev = sqrt(variance);
	}
	return stdev;
}

int Histogram::getMaxValue()
{
	int maxv = 0;
	for (int i=0; i<_numBins; i++)
		maxv = max ( maxv, _bin[i]);

	return maxv;
}

////////////////////////////////////////////////////////////////
// Spike
////////////////////////////////////////////////////////////////
int Histogram::spike ( int n )
{
	int oldmax = -1;
	int maxval;
	int i, j;

	for (i=0; i<n; i++)               // de-spike the histogram
	{
		maxval = -1;
		for (j=0; j<_numBins; j++)
		{
			if ((_bin[j] > maxval) && (oldmax == -1 || _bin[j] < oldmax))
			maxval = _bin[j];
		}
		if (maxval >= 0)
			oldmax = maxval;
	}

	return oldmax;
}

////////////////////////////////////////////////////////////////
// Assignment
////////////////////////////////////////////////////////////////
Histogram &Histogram::operator=(Histogram &hist)
{
	if (this == &hist)
		return *this;		// assignment to self

	_binSize = hist._binSize;
	_lowerLimit = hist._lowerLimit;
	_upperLimit = hist._upperLimit;
	_numBins = hist._numBins;
	_useBinSize = hist._useBinSize;
	_isIntRange = hist._isIntRange;

	allocate();

	for (int i=0; i<_numBins; i++)
		_bin[i] = hist._bin[i];

	_numViews = 0;			// Don't transfer views
	_views = NULL;

	return *this;
}

