////////////////////////////////////////////////////////////////////////
// LutToStrHistGlue: class that serves as a "glue" class between a
// set of LUT objects and another set of 
// Histogram objects (the stretched ones).  The class is a
// registered View to the LUT, so whenever it 
// receives an update(), it recollects the histograms (which in turn 
// will cause them to update their own views).  This class, 
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef SILUTTOSTRHISTGLUE_H
#define SILUTTOSTRHISTGLUE_H
#include "LutView.h"

class SiHistogram;

class SiLutToStrHistGlue : public LutView {

 protected:

   SiHistogram *_histR;
   SiHistogram *_histG;
   SiHistogram *_histB;

   SiHistogram *_strhistR;
   SiHistogram *_strhistG;
   SiHistogram *_strhistB;

 public:

   SiLutToStrHistGlue ( 
	SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
	SiHistogram *strhistR, SiHistogram *strhistG, SiHistogram *strhistB,
	Lut *lutR, Lut *lutG, Lut *lutB);

   virtual void update();	// the whole reason for the class existing

   virtual const char *const className() { return  "SiLutToStrHistGlue"; }

};
#endif
