//////////////////////////////////////////////////
// Lut.h: model class for Look-up Table
///////////////////////////////////////////////////
#ifndef LUT_H
#define LUT_H
#include <iostream>

class LutView;

class Lut {

protected:

	int _lowerLimit;
	int _upperLimit;

	int *_lut;		// A LUT array (usually 0..255)

	int _numViews;          // Number of dependent views

	LutView **_views;	// View objects that depend on this model

	void updateViews ( );

public:

	Lut ( );
	virtual ~Lut ( );

	void ramp ( );		// Initialize LUT

	void attachView (LutView *);       // Add dependent view object
	void detachView (LutView *);       // Delete dependent view object
 
	// Functions that allow to manipulate the LUT model

	virtual int *getAsArray ( ) { return _lut; };
	virtual void setAsArray (int *array);

	int operator [] ( int i );

	void setDN ( int index, int newDN );

	int getLowerLimit ( ) { return _lowerLimit; };
	int getUpperLimit ( ) { return _upperLimit; };
};
#endif
