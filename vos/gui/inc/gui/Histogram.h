//////////////////////////////////////////////////
// Histogram.h
// Note that of all the functions which change the histogram, only
// clear() calls updateViews automatically.  This is because it is
// likely that the entire histogram will be updated at once, which
// would result in many calls to updateViews.  It is therefore the
// caller's responsibility to call updateViews() when it is done
// modifying the histogram.  This includes changing the data range
// or the number of bins!
// The data range can be specified either for integral types
// (in which case it is [min..max] inclusive, e.g. 0..255), or for
// floating types (in which case it is [min..max) where max is just
// higher than the maximum value, e.g. 0..1.0).  Internally, the
// max is stored as max+1 (which makes it an exclusive range), but
// getUpperLimit() returns the value you sent in.
///////////////////////////////////////////////////
#ifndef HISTOGRAM_H
#define HISTOGRAM_H
#include <iostream>
#include <Xm/Xm.h>		// only for Boolean!

class HistView;

class Histogram	{

protected:

	double 	_binSize;
	double 	_lowerLimit; 
	double	_upperLimit;
	int	_numBins;
	Boolean	_useBinSize;	// True: binSize const.  False: _numBins const.
	Boolean _isIntRange;

 	int	*_bin; // Pointer to a hist array

	int _numViews;		// Number of dependent views
	HistView **_views;		// View objects that depend on this model

	void ctor(double ll, double ul);
	void allocate ();

public:

	Histogram ( double ll, double ul=256, double bs=1 );
	Histogram ( int ll, int ul=255, double bs=1 );
	Histogram();
	~Histogram ( );

	void attachView (HistView *);	// Add dependent view object
	void detachView (HistView *);   // Delete dependent view object

	void updateViews();	// Called whenever the model's data changes

	// Parameters for the model

	void setNumBins(int);	// setting one of #bins, binSize sets the other
	int numBins () { return _numBins; }

	void setBinSize(double);
 	double getBinSize() { return _binSize; };

	void setLowerLimit ( double );
	void setUpperLimit ( double );
	void setUpperLimit ( int );
	void setLimits ( double minval, double maxval);
	void setLimits ( double minval, double maxval, double binSize);
	void setLimits ( double minval, double maxval, int numBins);
	void setLimits ( int minval, int maxval);
	void setLimits ( int minval, int maxval, double binSize);
	void setLimits ( int minval, int maxval, int numBins);
	double getLowerLimit() { return _lowerLimit; }
	double getUpperLimit()
		{ return (_isIntRange ? _upperLimit-1 : _upperLimit); }
	double getUpperLimitBound() { return _upperLimit; }
	Boolean isIntRange() { return _isIntRange; }

	// Functions that allow to manipulate the model

	void clear ( );
	void clear_noupdate();

	int getBinNumber ( double value ) // get the "address"
	{
		int b;
		if ( value < _lowerLimit )
			b = 0;
		else if ( value > _upperLimit )
			b = _numBins-1;
		else {
			double tempValue;
			tempValue = value - _lowerLimit;
			b = int (tempValue / _binSize);
		}
		return b;
	};

	double getBinValue(int bin)	// get domain value for bin #
		{ return (bin * _binSize) + _lowerLimit; }

	int getBin ( int b )
	{
		if ( b < 0 )
			b = 0;
		if ( b >= _numBins )
			b = _numBins-1;
		return _bin[b];
	};

	void setBin( int b, int value ) 
	{
		if ( b < 0)
			b = 0;
		if ( b > _numBins )
			b = _numBins-1;
		_bin[b] = value; 
	};

	void incBin ( int b)
	{
		if ( b < 0)
			b = 0;
		if ( b >= _numBins )
			b = _numBins-1;
		_bin[b] ++;
	};

	int operator [] ( int i );

	double getMean();
	double getStDev();

	int getMaxValue();

	int spike ( int n);

	inline int min (int i, int j) { return (i<j) ? i: j; };
	inline int max (int i, int j) { return (i>j) ? i: j; };	

	Histogram &operator=(Histogram &hist);
};
#endif

