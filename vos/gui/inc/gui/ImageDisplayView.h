////////////////////////////////////////////////////////////////
// ImageDisplayView.h
//
//	This view works with ImageModel (model) for displaying an
//	image on screen.  This is a subclass of BasicImageView.
//
//	Notes:  XvicCreateImage sets default resources.
////////////////////////////////////////////////////////////////
#ifndef IMAGEDISPLAYVIEW_H
#define IMAGEDISPLAYVIEW_H
#include "BasicImageView.h"

class ImageTile;

class ImageDisplayView : public BasicImageView {

 private:

   static void exposeCallback(Widget, XtPointer client_data, XtPointer call_data);
   static void resizeCallback(Widget, XtPointer client_data, XtPointer call_data);

 protected:

   Widget _iw; 			// Image widget, child of Scrolled Window
   int _buffer_startLine;	// startLine number from most recent read
   int _buffer_startSample;	// startSample number from most recent read
   int _bufferHeight;		// height of buffer from most recent read
   int _bufferWidth;		// width of buffer from most recent read
   unsigned char _SaveScrollBarDisplayPolicyDuringZoom2Fit;

   ImageTile *_tilePtr;

   // Create an image widget.  Called only from constructor
   virtual void createImageWidget(Widget parent,
			Dimension view_height, Dimension view_width);

   // Init widget info
   virtual void initWidget();		

   // Resize image.  Called from callback and zoom-to-fit routine
   virtual void resizeImage();

   // Expose image.  Called from callback when widget is loading data
   virtual void exposeImage(XtPointer call_data);

 public:

   ImageDisplayView(Widget parent, const char *name, ImageData *model,
				Dimension view_height, Dimension  view_width);
   virtual ~ImageDisplayView();

   // Set window title to filename(s)
   virtual void setTitle(char *title);

   // Update called by model.
   virtual void update();
   virtual void updatePart(int flags);

   // Get ID of actual image widget.  baseWidget() returns the scrolled window
   // parent.
   virtual Widget getWidget() { return _iw; }

   // Set user zoom is called when user selects a new zoom on image
   virtual void setUserZoom(ZoomFactor &zoom);

   // Get image zoom returns the current zoom on the image
   virtual ZoomFactor &getImageZoom();
   virtual ZoomFactor &getPreZoom();

   // Set zoom to fit exactly within the window on screen.
   virtual void setUserZoom2Fit();

   // Get and set the size of the view
   virtual void setViewSize(Dimension w, Dimension h, Boolean scrollbars);
   virtual void getViewSize(Dimension &w, Dimension &h);

   // Standard classname
   virtual const char *const className() { return "ImageView"; }

};
#endif

