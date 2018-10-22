////////////////////////////////////////////////////////////////
// BasicImageView.h:
//	An abstract class for views with images to be displayed.
////////////////////////////////////////////////////////////////
#ifndef BASICIMAGEVIEW_H
#define BASICIMAGEVIEW_H
#include "UIComponent.h"
#include "ZoomFactor.h"
#include <Xm/Xm.h>

class ImageData;

class BasicImageView : public UIComponent {

 protected:

   ImageData *_model;	

   ZoomFactor _userZoom;
   ZoomFactor _imageZoom;
   ZoomFactor _preZoom;
   Boolean _zoom2Fit;

   // Calculate zoom factor needed for image to fit window
   virtual void calcZoomToFit(Widget iw, int *in, int *out);

 public:

   BasicImageView(const char *name, ImageData *model) : UIComponent(name) 
	{  _model = model;  }

   // Views that care about updatePart() should override to not call update().
   virtual void update() = 0;
   virtual void updatePart(int /*flags*/) { update(); }

   // Get image widget ID - should be overridden by subclass
   virtual Widget getWidget() { return NULL; }

   // Get object to fill in user zoom
   virtual ZoomFactor &getUserZoom() { return _userZoom; }

   // Is image set at zoom to fit?
   virtual Boolean isZoom2Fit() { return _zoom2Fit; }

   // Set user zoom factor on image - should be overridden by subclass
   virtual void setUserZoom(ZoomFactor &) { }

   // Set user zoom-to-fit on image - should be overridden by subclass
   virtual void setUserZoom2Fit() { }

   // Get current zoom - these should be overridden by subclasses
   virtual ZoomFactor & getImageZoom() { return _imageZoom; }
   virtual ZoomFactor & getPreZoom() { return _preZoom; }

   // Class name
   virtual const char *const className() { return  "BasicImageView"; }

};
#endif

