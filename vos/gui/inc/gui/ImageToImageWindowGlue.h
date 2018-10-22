////////////////////////////////////////////////////////////////////////
// ImageToImageWindowGlue: class that serves as a "glue" class between an
// ImageData object and an ImageWindow object. The class is a
// View to ImageData, so that the ImageWindow can take appropriate
// action whenever something changes w.r.t. the image.
//
// This class, though a UIComponent, creates no widget, and therefore 
// should never be managed.
////////////////////////////////////////////////////////////////////////

#ifndef IMAGETOIMAGEWINDOWGLUE_H
#define IMAGETOIMAGEWINDOWGLUE_H
#include "BasicImageView.h"

class ImageData;
class ImageWindow;

class ImageToImageWindowGlue : public BasicImageView {

 private: 

 protected:
  
  ImageWindow *_window;

//  void *_collectionActive;
 
 public:

   ImageToImageWindowGlue (ImageData *model, ImageWindow *window);
   virtual ~ImageToImageWindowGlue ();

   virtual void update();	// the whole reason for the class existing
   virtual void updatePart(int flags);

   virtual const char *const className() { return  "ImageToImageWindowGlue"; }

};
#endif

