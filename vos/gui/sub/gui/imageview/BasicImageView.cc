////////////////////////////////////////////////////////////////
// BasicImageView.cc
//	An abstract class for views with images to be displayed.
////////////////////////////////////////////////////////////////
#include "BasicImageView.h"
#include "XvicBasicImage.h"

////////////////////////////////////////////////////////////////
// calcZoomToFit
// 	Calculate the zoom factor needed to fit in the window
//	and return values.
////////////////////////////////////////////////////////////////

void  BasicImageView::calcZoomToFit(Widget iw, int *in, int *out)
{
   Dimension view_width, view_height;
   int image_width, image_height;

   XtVaGetValues(iw,
		XvicNviewWidth, &view_width, 
		XvicNviewHeight, &view_height,
		XvicNimageWidth, &image_width, 
		XvicNimageHeight, &image_height,
		NULL);

   // sanity checks

   if (view_width <= 0)
      view_width = 1;
   if (view_height <= 0)
      view_height = 1;
   if (image_width <= 0)
      image_width = 1;
   if (image_height <= 0)
      image_height = 1;

   // We must choose the smaller of the magnifications

   if (((double)view_width / (double)image_width) <
       ((double)view_height / (double)image_height)) {
      *in = view_width;		// Let the widget reduce the fraction
      *out = image_width;
   }
   else {
      *in = view_height;
      *out = image_height;
   }
}

