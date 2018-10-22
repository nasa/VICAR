//////////////////////////////////////////////////////////////////////
// CollectStretchedHist.cc
//////////////////////////////////////////////////////////////////////

#include "CollectStretchedHist.h"
#include "Lut.h"
#include "Histogram.h"

void CollectStretchedHist ( 
        Histogram *histR, Histogram *histG, Histogram *histB,
        Histogram *strhistR, Histogram *strhistG, Histogram *strhistB,
        Lut *lutR, Lut *lutG, Lut* lutB)
{
    CollectStretchedHist ( histR, strhistR, lutR);
    CollectStretchedHist ( histG, strhistG, lutG);
    CollectStretchedHist ( histB, strhistB, lutB);
}


void CollectStretchedHist ( Histogram *hist, Histogram *strhist, Lut *lut)
{

   int i, origTempValue, strTempValue;
   int *lut_vector;
   int str_i;

// Copy primary histogram parameters to stretched hist

   if (hist->isIntRange())
      strhist->setLimits((int)hist->getLowerLimit(),(int)hist->getUpperLimit(),
		hist->numBins());
   else
      strhist->setLimits(hist->getLowerLimit(),hist->getUpperLimit(),
		hist->numBins());

   lut_vector = lut->getAsArray();

   strhist->clear_noupdate();

   for ( i=0; i<hist->numBins(); i++) {

	//  Do the translation  to get a stretched histogram model

	//!!!! FIX THIS when Lut is updated to more than 8 bits !!!!
	// Assume Lut value of 0..255 maps to full histogram range.
	// Pick corresponding value out of LUT.
      str_i = ((lut->getUpperLimit()-lut->getLowerLimit()+1) * i) /
							hist->numBins();

      origTempValue = hist->getBin ( i );
      strTempValue = strhist->getBin ( lut_vector[str_i] );
      strhist->setBin ( lut_vector[str_i], origTempValue + strTempValue);
   }

   strhist->updateViews();

}

