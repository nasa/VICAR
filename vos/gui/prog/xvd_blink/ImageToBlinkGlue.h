////////////////////////////////////////////////////////////////////////
// ImageToBlinkGlue: class that serves as a "glue" class between an
// ImageData object and the BlinkControl object.  It resets the filename
// whenever the image is updated.
//
// This class, though a UIComponent, creates no widget, and therefore 
// should never be managed.
////////////////////////////////////////////////////////////////////////

#ifndef IMAGETOBLINKGLUE_H
#define IMAGETOBLINKGLUE_H
#include "BasicImageView.h"

class ImageData;
class BlinkControl;

class ImageToBlinkGlue : public BasicImageView {

 private: 

 protected:

   BlinkControl *_blinkControl;
   int _which;

 public:

   ImageToBlinkGlue (ImageData *model, BlinkControl *blinkControl, int which);

   virtual ~ImageToBlinkGlue ();

   virtual void update();
   virtual void updatePart(int flags);

   virtual const char *const className() { return  "ImageToBlinkGlue"; }

};
#endif

