////////////////////////////////////////////////////////////////////////
// ImageToHistGlue: class that serves as a "glue" class between an
// ImageData object and a (set of) Histogram objects.  The class is a
// View to ImageData, so whenever it receives an update() from ImageData,
// it recollects the histograms (which in turn cause them to update their
// own views).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef SIIMAGETOHISTGLUE_H
#define SIIMAGETOHISTGLUE_H
#include "BasicImageView.h"

class ImageData;
class SiHistogram;

class SiImageToHistGlue : public BasicImageView {

 protected:

   SiHistogram *_histR;
   SiHistogram *_histG;
   SiHistogram *_histB;

   void *_collectionActive;

 public:

   SiImageToHistGlue (ImageData *model,
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB);
   virtual ~SiImageToHistGlue ();

   virtual void update();	// the whole reason for the class existing
   virtual void updatePart(int flags);

   virtual const char *const className() { return  "SiImageToHistGlue"; }

};
#endif

