//////////////////////////////////////////////////
// SiHistogram.h
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
#ifndef SiHISTOGRAM_H
#define SiHISTOGRAM_H
#include "assert.h"
#include <X11/Intrinsic.h>		// only for Boolean!

class SiHistView;

class SiHistogram	{

    protected:

	double 	_binSize;
	double 	_lowerLimit; 
	double	_upperLimit;
	int	_numBins;
	Boolean	_useBinSize;	// True: binSize const.  False: _numBins const.
	Boolean _isIntRange;

	int	*_bin; 		// Pointer to a hist array

	int _numViews;		// Number of dependent views
	SiHistView **_views;	// View objects that depend on this model

	void ctor(double ll, double ul);
	void allocate ();

    public:

	SiHistogram ( double ll, double ul=256, double bs=1 );
	SiHistogram ( int ll, int ul=255, double bs=1 );
	SiHistogram();
	~SiHistogram();

        SiHistogram &operator=(SiHistogram &hist);
	int &operator[] ( int i );	// _bins[i]

	void attachView (SiHistView *);	// Add dependent view object
	void detachView (SiHistView *);   // Delete dependent view object
	void updateViews();		// Called whenever the model's data changes

	// Parameters for the model

	void setNumBins ( int );	// setting one of #bins, binSize sets the other
	int numBins() { return _numBins; }

	int *getAsArray();

	void setBinSize ( double );
 	double getBinSize() { return _binSize; };

	void setLimits ( double minval, double maxval );
	void setLimits ( double minval, double maxval, double binSize );
	void setLimits ( double minval, double maxval, int numBins );
	void setLimits ( int minval, int maxval );
	void setLimits ( int minval, int maxval, double binSize );
	void setLimits ( int minval, int maxval, int numBins );
	double getLowerLimit() { return _lowerLimit; }
	double getUpperLimit()
		{ return (_isIntRange ? _upperLimit-1 : _upperLimit); }
	double getUpperLimitBound() { return _upperLimit; }

	Boolean isIntRange() { return _isIntRange; }

	void clear();
	void clear_noupdate();

	int getBinNumber ( double value ) // get the "address"
	    {
		if ( value < _lowerLimit ) value = _lowerLimit;
		return (value >= _upperLimit) ? (_numBins - 1) 
			: (int ((value - _lowerLimit) / _binSize));
	    };

	double getBinValue ( int bin )	// get domain value for bin #
	    { return (bin * _binSize) + _lowerLimit; }

	int getBin ( int b )
	    {
		if (b < 0 || b >= _numBins)
		    b = 0;
		return _bin[b];
	    };

	void setBin ( int b, int value ) 
	    {
		if (b < 0 || b >= _numBins)
		    b = 0;
		_bin[b] = value; 
	    };

	void incBin ( int b )
	    {
		if (b < 0 || b >= _numBins)
                    b = 0;
		_bin[b] ++;
	    };

	double getMean();
	double getStDev();
	int getMaxValue();

	int spike ( int n );

	inline static int min (int i, int j) { return (i<j) ? i: j; };
	inline static int max (int i, int j) { return (i>j) ? i: j; };	

};
#endif

