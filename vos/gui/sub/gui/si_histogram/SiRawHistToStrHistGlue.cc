//////////////////////////////////////////////////////////
// RawHistToStrHistGlue.cc
//////////////////////////////////////////////////////////
#include "SiRawHistToStrHistGlue.h"
#include "SiHistogram.h"
#include "LutView.h"
#include "SiHistView.h"
#include "SiCollectStretchedHist.h"

SiRawHistToStrHistGlue::SiRawHistToStrHistGlue ( 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
		SiHistogram *strhistR, SiHistogram *strhistG, SiHistogram *strhistB,
		Lut *lutR, Lut *lutG, Lut *lutB)
	: SiHistView ( histR, histG, histB )
{
    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;

    _strhistR = strhistR;
    _strhistG = strhistG;
    _strhistB = strhistB;

    _histR->attachView ( this );
    _histG->attachView ( this );
    _histB->attachView ( this );
}

void SiRawHistToStrHistGlue::update()
{
   // Call the "glue" function to perform the operation

   SiCollectStretchedHist ( _histR, _histG, _histB,
                _strhistR, _strhistG, _strhistB,
                _lutR, _lutG, _lutB );
}
