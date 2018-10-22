////////////////////////////////////////////////////////////////
// ImageSizeView.h
//
//	Class for image size information.
//	
///////////////////////////////////////////////////////////////
#ifndef IMAGESIZEVIEW_H
#define IMAGESIZEVIEW_H
#include "BasicImageView.h"
#include "XvicImage.h"
#include "ImageData.h"

class ImageSizeView : public BasicImageView {

   protected:

      Widget _label, _textfield, _form;

      // ADD A LABEL-TEXTFIELD COMPONENT FOR DISPLAYING IMAGE SIZE INFO - 
      virtual void addNewSubView(const char *displayName);

      // UPDATE THE DISPLAY WITH THE LATEST VALUE
      virtual void updateValue(int ns, int nl);

   public:

      ImageSizeView(Widget parent, const char *name, ImageData *imageSizeModel);
      virtual ~ImageSizeView();

      // UPDATE CALLED BY MODEL TO UPDATE ITS VIEWS
      virtual void update();

      // CLASSNAME
      virtual const char *const className() { return "ImageSizeView" ; } 
};
#endif
