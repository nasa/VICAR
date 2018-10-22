//////////////////////////////////////////////////////////
// RawHistToStrHistGlue.cc
//////////////////////////////////////////////////////////
#include "RawHistToStrHistGlue.h"
#include "Histogram.h"
#include "LutView.h"
#include "HistView.h"
#include "CollectStretchedHist.h"


RawHistToStrHistGlue::RawHistToStrHistGlue ( 
		Histogram *histR, Histogram *histG, Histogram *histB,
		Histogram *strhistR, Histogram *strhistG, Histogram *strhistB,
		Lut *lutR, Lut *lutG, Lut *lutB)
	: HistView ( "glue")
{
    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;

    _hist  = histR;
    _hist1 = histG;
    _hist2 = histB;

    _strhistR = strhistR;
    _strhistG = strhistG;
    _strhistB = strhistB;

    _hist ->attachView ( this);
    _hist1->attachView ( this);
    _hist2->attachView ( this);

}

void RawHistToStrHistGlue::update()
{
   // Call the "glue" function to perform the operation
   CollectStretchedHist ( _hist, _hist1, _hist2,
                _strhistR, _strhistG, _strhistB,
                _lutR, _lutG, _lutB);
}
