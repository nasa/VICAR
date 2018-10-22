////////////////////////////////////////////////////////////////////////
// LutToStrHistGlue: class that serves as a "glue" class between a
// set of LUT objects and another set of 
// Histogram objects (the stretched ones).  The class is a
// registered View to the LUT, so whenever it 
// receives an update(), it recollects the histograms (which in turn 
// will cause them to update their own views).  This class, 
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef LUTTOSTRHISTGLUE_H
#define LUTTOSTRHISTGLUE_H
#include "LutView.h"

class Histogram;

class LutToStrHistGlue : public LutView {

 protected:

   Histogram *_histR;
   Histogram *_histG;
   Histogram *_histB;
   Histogram *_strhistR;
   Histogram *_strhistG;
   Histogram *_strhistB;

 public:

   LutToStrHistGlue ( 
		Histogram *histR, Histogram *histG, Histogram *histB,
		Histogram *strhistR, Histogram *strhistG, Histogram *strhistB,
		Lut *lutR, Lut *lutG, Lut *lutB);

   virtual void update();	// the whole reason for the class existing

   virtual const char *const className() { return  "LutToStrHistGlue"; }

};
#endif
