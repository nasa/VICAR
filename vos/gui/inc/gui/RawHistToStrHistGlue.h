////////////////////////////////////////////////////////////////////////
// RawHistToStrHistGlue: class that serves as a "glue" class between a
// set of Histogram objects (the unstretched ones) and another set of 
// Histogram objects (the stretched ones).  The class is a
// registered View to the unstretched Histogram, so whenever it 
// receives an update(), it recollects the histograms (which in turn 
// will cause them to update their own views).  This class, 
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef RAWHISTTOSTRHISTGLUE_H
#define RAWHISTTOSTRHISTGLUE_H
#include "HistView.h"

class Histogram;
class Lut;

class RawHistToStrHistGlue : public HistView {

 protected:

   Histogram *_strhistR;
   Histogram *_strhistG;
   Histogram *_strhistB;

   Lut *_lutR;
   Lut *_lutG;
   Lut *_lutB;

 public:

   RawHistToStrHistGlue (
		Histogram *histR, Histogram *histG, Histogram *histB,
		Histogram *strhistR, Histogram *strhistG, Histogram *strhistB,
		Lut *lutR, Lut *lutG, Lut *lutB);

   virtual void update();	// the whole reason for the class existing

   virtual const char *const className() { return  "RawHistToStrHistGlue"; }

};
#endif
