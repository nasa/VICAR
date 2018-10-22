////////////////////////////////////////////////////////////////////////
//
//    ImageToLabelGlue.h: 
//
//    This is a class derived form BasicImageView that serves as 
//    a "glue" class between an ImageData object and a TextDisplayModel object.
//
//    This class, though a UIComponent, creates no widget, and therefore 
//    should never be managed.
//
////////////////////////////////////////////////////////////////////////

#ifndef IMAGETOLABELGLUE_H
#define IMAGETOLABELGLUE_H
#include "BasicImageView.h"

class ImageData;
class LabelCmd;

class ImageToLabelGlue : public BasicImageView {

 protected:

   LabelCmd *_labelCmd;

 public:

   ImageToLabelGlue (ImageData *model, LabelCmd *labelCmd);

   virtual ~ImageToLabelGlue ();

   virtual void update();
   virtual void updatePart(int flags);

   virtual const char *const className() { return  "ImageToLabelGlue"; }

};
#endif

