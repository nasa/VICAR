////////////////////////////////////////////////////////////////
// SiHistogram.cc: Histogram model class
////////////////////////////////////////////////////////////////
#include "SiHistogram.h"
#include "SiHistView.h"
#include "ViewMacros.h"
#include "ErrorManager.h"
#include <stdio.h>
#include <math.h>

////////////////////////////////////////////////////////////////
// Constructors/Destructors
////////////////////////////////////////////////////////////////
SiHistogram::SiHistogram ( double ll, double ul, double bs )
{
	_isIntRange = False;
	_useBinSize = True;
	_binSize = bs;
	ctor(ll, ul);
}

SiHistogram::SiHistogram ( int ll, int ul, double bs )
{
	_isIntRange = True;
	_useBinSize = True;
	_binSize = bs;
	ctor(ll, ul + 1);
}

SiHistogram::SiHistogram()
{
	_isIntRange = True;
	_useBinSize = False;
	_numBins = 256;
	ctor(0, 256);
}

void SiHistogram::ctor ( double ll, double ul )
{
	_lowerLimit = ll;
	_upperLimit = ul;

	if ( _upperLimit < _lowerLimit ) {
		theErrorManager->process ( Error, "histogram", 
			"Upper limit can't be less than lower limit!" );
		_lowerLimit = 0; _upperLimit = 256;
	}

	_numViews = 0;

	_views = NULL;
	_bin = NULL;

	allocate ();

	clear();
}

SiHistogram::~SiHistogram()
{
	if (_views) delete [] _views;
	delete [] _bin;
}

////////////////////////////////////////////////////////////////
// Views
////////////////////////////////////////////////////////////////
void SiHistogram::attachView ( SiHistView *view )
{
	AttachViewMacro(SiHistView, _views, _numViews, view);
	view->update();
}

void SiHistogram::detachView ( SiHistView *view )
{
	DetachViewMacro(SiHistView, _views, _numViews, view);
}

void SiHistogram::updateViews()
{
	for (int i=0; i<_numViews; i++)
		_views[i]->update ( );
}

////////////////////////////////////////////////////////////////
// Misc. manipulation
////////////////////////////////////////////////////////////////

void SiHistogram::allocate()
{
	if (_useBinSize)	// Calc numBins based on bin size
		_numBins = int ( (_upperLimit-_lowerLimit) / _binSize + 0.5 );
	else			// Calc _binSize based on number of bins
		_binSize = ( _upperLimit - _lowerLimit ) / _numBins;

	if (_bin)
		delete [] _bin;

	_bin = new int[_numBins];
	if ( !_bin ) 
		theErrorManager->process ( Error, "histogram", 
					   "Memory allocation error" );

	clear_noupdate();
}

void SiHistogram::clear()
{
	clear_noupdate();
	updateViews();
}

void SiHistogram::clear_noupdate()
{
        for  ( int i = 0; i < _numBins; i++ )
                _bin[i] = 0;
}

////////////////////////////////////////////////////////////////
// Overwrite [] operator and return the the bin count where the 
// parameter that is passed to the procedure belongs.  Note that 
// reference return creates lvalue.
////////////////////////////////////////////////////////////////

int &SiHistogram::operator[] ( int binNumber )
{
	if (binNumber < 0 || binNumber >= _numBins)
            binNumber = 0;
	return _bin[binNumber];
}

////////////////////////////////////////////////////////////////
// Functions that manipulate the histogram range (min/max/#bins/bin size)
////////////////////////////////////////////////////////////////
void SiHistogram::setBinSize (double bs)
{
        _binSize = bs;
	_useBinSize = True;
	allocate();
}

void SiHistogram::setNumBins(int num)
{
	_numBins = num;
	_useBinSize = False;
	allocate();
}

void SiHistogram::setLimits (double minval, double maxval)
{
        _lowerLimit = minval;
        _upperLimit = maxval;
	_isIntRange = False;
	allocate();
}

void SiHistogram::setLimits (double minval, double maxval, double binSize)
{
        _lowerLimit = minval;
        _upperLimit = maxval;
	_isIntRange = False;
	_binSize = binSize;
	_useBinSize = True;
	allocate();
}

void SiHistogram::setLimits (double minval, double maxval, int numBins)
{
        _lowerLimit = minval;
        _upperLimit = maxval;
	_isIntRange = False;
	_numBins = numBins;
	_useBinSize = False;
	allocate();
}

void SiHistogram::setLimits (int minval, int maxval)
{
        _lowerLimit = minval;
        _upperLimit = maxval + 1;
	_isIntRange = True;
	allocate();
}

void SiHistogram::setLimits (int minval, int maxval, double binSize)
{
        _lowerLimit = minval;
        _upperLimit = maxval + 1;
	_isIntRange = True;
	_binSize = binSize;
	_useBinSize = True;
	allocate();
}

void SiHistogram::setLimits (int minval, int maxval, int numBins)
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

////////////////////////////////////////////////////////////////
// Return histogram mean value
////////////////////////////////////////////////////////////////
double SiHistogram::getMean()
{
	double rsum0 = 0.0;
	double rsum1 = 0.0;
	double mean;

	for ( int i = 0; i < _numBins; i++ ) {
		double d = (double)_bin[i];
		rsum0 += d;
		rsum1 += d * i;
	}

	if (rsum0 == 0.0)
		mean = 0.0;
	else
		mean = rsum1 / rsum0;

	return mean;
}

////////////////////////////////////////////////////////////////
// Return histogram standard deviation value
////////////////////////////////////////////////////////////////
double SiHistogram::getStDev()
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

int SiHistogram::getMaxValue()
{
	int maxv = 0;
	for ( int i = 0; i < _numBins; i++)
		maxv = max ( maxv, _bin[i]);

	return maxv;
}

////////////////////////////////////////////////////////////////
// Spike.
////////////////////////////////////////////////////////////////
int SiHistogram::spike ( int n )
{
	int oldmax = -1;
	int i, j;

	for ( i = 0; i < n; i++ ) {	// de-spike the histogram
		int maxval = -1;
		for ( j = 0; j < _numBins; j++ ) {
			if ((_bin[j] > maxval) 
			    && (oldmax == -1 || _bin[j] < oldmax))
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
SiHistogram &SiHistogram::operator=(SiHistogram &hist)
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

	for (int i = 0; i < _numBins; i++)
		_bin[i] = hist._bin[i];

	_numViews = 0;			// Don't transfer views
	if (_views)
		delete [] _views;
	_views = NULL;

	return *this;
}

// Caller should free!!!
int *SiHistogram::getAsArray()
{
    int *a = new int [_numBins];
    for ( int i = 0; i < _numBins; i++ )
	a[i] = _bin[i];
    return a;
}
