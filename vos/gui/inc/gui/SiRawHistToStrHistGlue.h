////////////////////////////////////////////////////////////////////////
// RawHistToStrHistGlue: class that serves as a "glue" class between a
// set of Histogram objects (the unstretched ones) and another set of 
// Histogram objects (the stretched ones).  The class is a
// registered View to the unstretched Histogram, so whenever it 
// receives an update(), it recollects the histograms (which in turn 
// will cause them to update their own views).  This class, 
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef SIRAWHISTTOSTRHISTGLUE_H
#define SIRAWHISTTOSTRHISTGLUE_H
#include "SiHistView.h"

class SiHistogram;
class Lut;

class SiRawHistToStrHistGlue : public SiHistView {

 protected:

   SiHistogram *_strhistR;
   SiHistogram *_strhistG;
   SiHistogram *_strhistB;

   Lut *_lutR;
   Lut *_lutG;
   Lut *_lutB;

 public:

   SiRawHistToStrHistGlue (
	SiHistogram *histR, SiHistogram *histG, SiHistogram *histB,
	SiHistogram *strhistR, SiHistogram *strhistG, SiHistogram *strhistB,
	Lut *lutR, Lut *lutG, Lut *lutB);

   virtual void update();	// the whole reason for the class existing

   virtual const char *const className() { return  "SiRawHistToStrHistGlue"; }

};
#endif
