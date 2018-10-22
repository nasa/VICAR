//////////////////////////////////////////////////////////
// LutToStrHistGlue.cc
//////////////////////////////////////////////////////////
#include "LutToStrHistGlue.h"
#include "Histogram.h"
#include "LutView.h"
#include "CollectStretchedHist.h"

LutToStrHistGlue::LutToStrHistGlue ( 
		Histogram *histR, Histogram *histG, Histogram *histB,
		Histogram *strhistR, Histogram *strhistG, Histogram *strhistB,
		Lut *lutR, Lut *lutG, Lut *lutB)
	: LutView ( "glue", lutR, lutG, lutB )
{
    _histR = histR;
    _histG = histG;
    _histB = histB;

    _strhistR = strhistR;
    _strhistG = strhistG;
    _strhistB = strhistB;

    if (_lut) _lut ->attachView ( this);
    if (_lut1) _lut1->attachView ( this);
    if (_lut2) _lut2->attachView ( this);
}

void LutToStrHistGlue::update()
{
   // Call the "glue" function to perform the operation
   CollectStretchedHist ( _histR, _histG, _histB,
		_strhistR, _strhistG, _strhistB,
		_lut, _lut1, _lut2);
}
