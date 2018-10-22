////////////////////////////////////////////////////////////////////////
// ImageToHistGlue: class that serves as a "glue" class between an
// ImageData object and a (set of) Histogram objects.  The class is a
// View to ImageData, so whenever it receives an update() from ImageData,
// it recollects the histograms (which in turn cause them to update their
// own views).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef IMAGETOHISTGLUE_H
#define IMAGETOHISTGLUE_H
#include "BasicImageView.h"

class ImageData;
class Histogram;

class ImageToHistGlue : public BasicImageView {

 private:

 protected:
   Histogram *_histR;
   Histogram *_histG;
   Histogram *_histB;

   void *_collectionActive;

 public:

   ImageToHistGlue (ImageData *model,
			Histogram *histR, Histogram *histG, Histogram *histB);
   virtual ~ImageToHistGlue ();

   virtual void update();	// the whole reason for the class existing
   virtual void updatePart(int flags);

   virtual const char *const className() { return  "ImageToHistGlue"; }

};
#endif

