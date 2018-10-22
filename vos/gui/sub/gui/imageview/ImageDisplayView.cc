////////////////////////////////////////////////////////////////
// ImageDisplayView.cc
//
//	This view works with ImageModel (model) for 
//	displaying an image on screen.  This is a sub-
//	class of BasicImageView.
////////////////////////////////////////////////////////////////
#include "ImageDisplayView.h"
#include "ImageData.h"
#include "ZoomFactor.h"
#include "XvicImage.h"
#include "ErrorDialogManager.h"
#include "Application.h"
#include <string.h>
#include <stdio.h>

// This routine (in a separate file) moves info into struct
// used by the Xiw image widget to display
void getXvicImageDataStruct(ImageTile &tile, XvicImageData &img);
 
////////////////////////////////////////////////////////////////
// Constructor:
//	Inits variables and creates image widget
////////////////////////////////////////////////////////////////
ImageDisplayView::ImageDisplayView(Widget parent, const char *name,
				ImageData *model,
				Dimension view_height, Dimension view_width) 
		: BasicImageView(name, model)
{
   // Init some data members
   _buffer_startLine = 0;
   _buffer_startSample = 0;
   _bufferHeight = 0;
   _bufferWidth = 0;
   _tilePtr = 0;
   _zoom2Fit = False;
   _iw = NULL;

   // Create image widget (and its parent scrolled window)
   createImageWidget(parent, view_height, view_width);

   // Attach view to image data model
   model->attachView(this);
}

////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////
ImageDisplayView::~ImageDisplayView()
{
   // Detach itself from the model so that the are no more updates sent
   _model->detachView(this);

   XtDestroyWidget(_iw);
   if (_tilePtr != 0)
      delete _tilePtr;
}

////////////////////////////////////////////////////////////////
// Expose Callback, calls ExposeImage
////////////////////////////////////////////////////////////////
void ImageDisplayView::exposeCallback(Widget, 
		XtPointer client_data, XtPointer call_data)
{
   ImageDisplayView *view = (ImageDisplayView *) client_data;
   view->exposeImage(call_data);
}

////////////////////////////////////////////////////////////////
// ExposeImage
//	Called by exposeCallback for loading & drawing window
////////////////////////////////////////////////////////////////
void ImageDisplayView::exposeImage(XtPointer call_data)
{
   XvicImageData img;
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) call_data;

   int width;
   int height;

   StatusType status;

   // If the file isn't open, write a blank area and then return.  The widget
   // is not happy if its data requests are not responded to with a call to
   // XvicImageWrite (see IW docs)

   if (!_model->isDataSourceOpened()) {
      img.x = cb->x;
      img.y = cb->y;
      img.width = cb->width;
      img.height = cb->height;
      img.memory_control = XvicMEMORY_APPLIC;
      img.line_width = cb->prezoom_width;
      img.start_offset = 0;
      img.bw_pixels = img.red_pixels = img.grn_pixels = img.blu_pixels =
		new unsigned char[cb->prezoom_width * cb->prezoom_height];
      if (img.bw_pixels)
         memset(img.bw_pixels, 0, cb->prezoom_width * cb->prezoom_height);
      XvicImageWrite(_iw, &img, False);
      if (img.bw_pixels)
         delete img.bw_pixels;

      return;
   }

   // If buffer size is insufficient, destroy old tile

   if (_tilePtr != NULL && (((cb->prezoom_width > _tilePtr->getBufferWidth()) ||
			    (cb->prezoom_height > _tilePtr->getBufferHeight()))
		|| _model->isNewBufferNeeded(*_tilePtr))) {
      delete _tilePtr;
      _tilePtr = NULL;
   }

   // Create new tile if needed

   if (_tilePtr == NULL) {
      XtVaGetValues(_iw,XvicNtileWidth, &width,
			XvicNtileHeight, &height,
			NULL);
      _model->setUnzoomedTileSize(height, width); 
      ZoomFactor &preZoom = getPreZoom();
      ImageTile &tile = _model->createTile(preZoom);
      _tilePtr = &tile;
   }

   // Move data into new tile

   _buffer_startLine = cb->y;
   _buffer_startSample= cb->x;
   _bufferHeight = cb->height;
   _bufferWidth = cb->width;
   status = _model->readTile(_buffer_startSample, _buffer_startLine,
				_bufferWidth, _bufferHeight, *_tilePtr);
   if (status != imSUCCESS) {
      if (!_model->errorMsgIssued()) {
         theErrorDialogManager->post(_model->getErrorMsg());
      }
   }

   // Copy data from tile into widget 

   getXvicImageDataStruct(*_tilePtr, img);   // memory_control = MEMORY_APPLIC

   // Draw image in widget

   XvicImageWrite(_iw, &img, False);

}

////////////////////////////////////////////////////////////////
// Resize Callback
//	Calls resize image
////////////////////////////////////////////////////////////////
void ImageDisplayView::resizeCallback(Widget, XtPointer client_data, XtPointer)
{
   ImageDisplayView *view = (ImageDisplayView *) client_data;
   view->resizeImage();
}

////////////////////////////////////////////////////////////////
// Resize Image
//	Called when zoom2fit is selected and when window is resized.
////////////////////////////////////////////////////////////////
void ImageDisplayView::resizeImage()
{
   int zoom_in, zoom_out;
   int Xsubpan, Ysubpan;

   // If not zoom to fit, nothing to do

   if (!_zoom2Fit)
      return;

   if (!_model->isDataSourceOpened()) {
      zoom_in = 1;			// Input not avail, use a default
      zoom_out = 1;
   }
   else
      calcZoomToFit(_iw, &zoom_in, &zoom_out);

   // Get subpixel pan
   XtVaGetValues(_iw, XvicNxSubpixelPan, &Xsubpan,
		      XvicNySubpixelPan, &Ysubpan,
                      NULL);	

   // Get new prezoom
   ZoomFactor &newPreZoom = _model->calcTileZoom(_tilePtr, zoom_in, zoom_out,
			zoom_in, zoom_out, Xsubpan, Ysubpan);

   // Update widget with new zoom and prezoom
   XtVaSetValues(_iw,	XvicNxZoomIn,		zoom_in,
			XvicNxZoomOut,		zoom_out,
			XvicNyZoomIn,		zoom_in,
			XvicNyZoomOut,		zoom_out,
			XvicNxSubpixelPan,	Xsubpan,
			XvicNySubpixelPan,	Ysubpan,
			XvicNxPreZoomIn,   	newPreZoom.getXIn(),
			XvicNxPreZoomOut,   	newPreZoom.getXOut(),
			XvicNyPreZoomIn,   	newPreZoom.getYIn(),
			XvicNyPreZoomOut,   	newPreZoom.getYOut(),
			XvicNxPreSubpixelPan,	newPreZoom.getSubPixelPanX(),
			XvicNyPreSubpixelPan,	newPreZoom.getSubPixelPanY(),
			NULL);
}

////////////////////////////////////////////////////////////////
// update
//	called by model when something in model changes (usually a new image).
////////////////////////////////////////////////////////////////
void ImageDisplayView::update()
{
   char *newName;

   // Invalidate old tile
   if ( _tilePtr ) {
      delete _tilePtr;
      _tilePtr = NULL;
   }

   initWidget();

   // Reset the prezoom in the widget, because it's possible to change the
   // prezoom without changing the zoom (e.g. when RotatedImageData changes
   // rotation mode). This is a bit of a kludge; there should be a better way...

   if (_zoom2Fit)
      resizeImage();
   else {
      getImageZoom();		// saved in _imageZoom
      setUserZoom(_imageZoom);
   }

   // Force a redisplay of the widget
   XvicImageClear(_iw);

   newName = strdup(_model->getInputDataSourceName());
   setTitle(newName);
   delete [] newName;
}

////////////////////////////////////////////////////////////////
// updatePart
//	called by model when part of the model changes (e.g. data range)
////////////////////////////////////////////////////////////////
void ImageDisplayView::updatePart(int flags)
{
   double max, min;

   if (flags & IMAGE_DATA_UPDATE_RANGE) {
      _model->getDataRange(min, max);
      if (_iw) {
         XtVaSetValues(_iw, XvicNrawDataMin, XvicDOUBLE_ARG(min),
			    XvicNrawDataMax, XvicDOUBLE_ARG(max), NULL);
      }
   }
}

////////////////////////////////////////////////////////////////
// Create Image Widget
//	private routine, called by constructor.
////////////////////////////////////////////////////////////////
void ImageDisplayView::createImageWidget(Widget parent,
			Dimension view_height, Dimension view_width)
{
   Arg args[20];
   int image_height = 0;
   int image_width = 0;

   // Get image size, if available

   if (_model->isDataSourceOpened() == TRUE) {
      image_height = _model->getNumbLines(); 
      image_width  = _model->getNumbSamples();
   }

   // Set view size
   // Set the view size to 3/4 of the screen size, or to the image	
   // size, whichever is smaller.  If the image size is 0, set the
   // view size to 1/4 of the screen size so it looks better.

   if (view_height == 0) {
      view_height = (XDisplayHeight(XtDisplay(parent),
		     XDefaultScreen(XtDisplay(parent))) * 3) / 4;
      if (image_height == 0) 
         view_height = (XDisplayHeight(XtDisplay(parent),
			XDefaultScreen(XtDisplay(parent))) * 1) / 4;
      else
         if ((image_height < (int)view_height) || (view_height == 0))
            view_height = (Dimension) image_height;
   }
   if (view_width == 0) {
      view_width = (XDisplayWidth(XtDisplay(parent),
		    XDefaultScreen(XtDisplay(parent))) * 3) / 4;
      if (image_width == 0) 
         view_width = (XDisplayWidth(XtDisplay(parent),
		       XDefaultScreen(XtDisplay(parent))) * 1) / 4;
      else
         if ((image_width < (int) view_width) || (view_width == 0))
            view_width = (Dimension)  image_width;
   }

   int n = 0;
   XtSetArg(args[n], (char *)XvicNviewHeight, view_height); n++;
   XtSetArg(args[n], (char *)XvicNviewWidth, view_width); n++;

   // Create image widget and its scrolled window parent
   _iw = XvicCreateImage(parent, _name, args, n);  // _iw is image widget child

   if (_model->isDataSourceOpened())
      initWidget();

   _w = XtParent(_iw);
   XtUnmanageChild(_w);
   XtManageChild(_iw);  // manage iw here
   installDestroyHandler();

   // Save current state for later use
   XtVaGetValues(_iw, XvicNscrollBarDisplayPolicy,
		&_SaveScrollBarDisplayPolicyDuringZoom2Fit, NULL);

   // Add callbacks
   XtAddCallback(_iw, XvicNexposeCallback, &ImageDisplayView::exposeCallback,
			(XtPointer) this);
   XtAddCallback(_iw, XvicNresizeCallback, &ImageDisplayView::resizeCallback,
			(XtPointer) this);
}

////////////////////////////////////////////////////////////////
// initWidget
//	called when data source is opened
////////////////////////////////////////////////////////////////
void ImageDisplayView::initWidget()
{
   Arg args[20];
   int n = 0;
   int image_height, image_width;	// Actual image size
   int tile_height, tile_width;
   double min, max;

   if (!_iw)
      return;			// If no widget yet, nothing to do

   // Set image mode (color or bw)

   image_height = _model->getNumbLines();
   image_width  = _model->getNumbSamples();
   if (_model->getMode() == COLORmode) {
      XtSetArg(args[n], (char *)XvicNimageMode, XvicCOLOR); n++;
   }
   else {
      XtSetArg(args[n], (char *)XvicNimageMode, XvicBW); n++;
   }

   // Set image and tile sizes in widget
   // Set the tile size to the entire width unless it's really big,
   // in which case we set it a little smaller for improved panning
   // (at the expense of taking longer to load the entire image).

   _model->getSuggestedUnzoomedTileSize(tile_height, tile_width);
   if (tile_width > 2048)
      tile_width = 2048;

   XtSetArg(args[n], (char *)XvicNimageWidth, image_width); n++;
   XtSetArg(args[n], (char *)XvicNimageHeight, image_height); n++;
   XtSetArg(args[n], (char *)XvicNtileWidth, tile_width); n++;
   XtSetArg(args[n], (char *)XvicNtileHeight, tile_height); n++;

   XtSetArg(args[n], (char *)XvicNdataType,_model->getPixelType().getXiw());n++;
   _model->getDataRange(min, max);
   XtSetArg(args[n], (char *)XvicNrawDataMin, XvicDOUBLE_ARG(min)); n++;
   XtSetArg(args[n], (char *)XvicNrawDataMax, XvicDOUBLE_ARG(max)); n++;

   XtSetValues(_iw, args, n);
}

////////////////////////////////////////////////////////////////
// setUserZoom
//	Set new zoom on image.  Called from outside zoom controls
////////////////////////////////////////////////////////////////
void ImageDisplayView::setUserZoom(ZoomFactor &userZoom)
{

   // Get new prezoom

   ZoomFactor &newPreZoom = _model->calcTileZoom(_tilePtr,
			userZoom.getXIn(), userZoom.getXOut(),
			userZoom.getYIn(), userZoom.getYOut(),
			userZoom.getSubPixelPanX(), userZoom.getSubPixelPanX());
	
   // Update widget with new zoom and prezoom

   XtVaSetValues(_iw,	XvicNxZoomIn,		userZoom.getXIn(),
			XvicNyZoomIn,		userZoom.getYIn(),
			XvicNxZoomOut,		userZoom.getXOut(),
			XvicNyZoomOut,		userZoom.getYOut(),
			XvicNxSubpixelPan,	userZoom.getSubPixelPanX(),
			XvicNySubpixelPan,	userZoom.getSubPixelPanY(),
			XvicNxPreZoomIn,   	newPreZoom.getXIn(),
			XvicNxPreZoomOut,   	newPreZoom.getXOut(),
			XvicNyPreZoomIn,   	newPreZoom.getYIn(),
			XvicNyPreZoomOut,   	newPreZoom.getYOut(),
			XvicNxPreSubpixelPan,	newPreZoom.getSubPixelPanX(),
			XvicNyPreSubpixelPan,	newPreZoom.getSubPixelPanY(),
			NULL);

   // Set scrollbar policy with saved value

   if (_zoom2Fit) {
      _zoom2Fit = False;
      XtVaSetValues(_iw, XvicNscrollBarDisplayPolicy,
			 _SaveScrollBarDisplayPolicyDuringZoom2Fit,
			 NULL);
   }
}

////////////////////////////////////////////////////////////////
// setUserZoom2Fit
//	Set zoom so that image fills window.  Called from outsize zoom controls
////////////////////////////////////////////////////////////////
void ImageDisplayView::setUserZoom2Fit()
{

   // Save old scrollbar policy and set to XvicNEVER so we can return
   // to this policy when user selects a new zoom.

   if (_zoom2Fit == False) {
      _zoom2Fit = True;
     XtVaGetValues(_iw, XvicNscrollBarDisplayPolicy,
			&_SaveScrollBarDisplayPolicyDuringZoom2Fit,
			NULL);	
   }

   // There is a motif bug that causes the scrollbars to display erroneously
   // in this mode so since they are not needed, we just hardcode the "never".
   // If the bug were fixed, the scrollbars would not display anyway when the
   // policy is set to "XvicAS_NEEDED".

   XtVaSetValues(_iw, XvicNscrollBarDisplayPolicy, XvicNEVER, NULL);
   resizeImage();
}

////////////////////////////////////////////////////////////////
// getImageZoom()
//	Get current zoom that is set in image.
////////////////////////////////////////////////////////////////
ZoomFactor &ImageDisplayView::getImageZoom()
{
   int Xin, Xout, Yin, Yout, Xpan, Ypan;

   // Get zoom info from widget

   XtVaGetValues(_iw, XvicNxZoomIn, &Xin,
		      XvicNxZoomOut, &Xout,
                      XvicNyZoomIn, &Yin,
                      XvicNyZoomOut, &Yout,
                      XvicNxSubpixelPan, &Xpan,
                      XvicNySubpixelPan, &Ypan,
                      NULL);

   // Fill zoom factor object with info 

   _imageZoom.setX(Xin, Xout);
   _imageZoom.setY(Yin, Yout);
   _imageZoom.setSubPixelPanX(Xpan);
   _imageZoom.setSubPixelPanY(Ypan);

   return _imageZoom;
}

////////////////////////////////////////////////////////////////
// getPreZoom()
//	Get current prezoom that is set in image.
////////////////////////////////////////////////////////////////
ZoomFactor & ImageDisplayView::getPreZoom()
{
   int Xin, Xout, Yin, Yout, Xpan, Ypan;

   // Get zoom info from widget
   XtVaGetValues(_iw, XvicNxPreZoomIn, &Xin,
                      XvicNxPreZoomOut, &Xout,
                      XvicNyPreZoomIn, &Yin,
                      XvicNyPreZoomOut, &Yout,
                      XvicNxPreSubpixelPan, &Xpan,
                      XvicNyPreSubpixelPan, &Ypan,
                      NULL);

   // Fill zoom factor object with info 
   _preZoom.setX(Xin, Xout);
   _preZoom.setY(Yin, Yout);
   _preZoom.setSubPixelPanX(Xpan);
   _preZoom.setSubPixelPanY(Ypan);

   return _preZoom;
}

////////////////////////////////////////////////////////////////
// Return the current view size
////////////////////////////////////////////////////////////////
void ImageDisplayView::getViewSize(Dimension &w, Dimension &h)
{
   if (_iw)
      XtVaGetValues(_iw, XvicNviewWidth, &w, XvicNviewHeight, &h, NULL);
   else {
      w = 0;
      h = 0;
   }
}

////////////////////////////////////////////////////////////////
// Set the current view size.  scrollbars==True means to display
// scrollbars as needed (using whatever default we started with).
// scrollbars==False means to definitely not display them.
////////////////////////////////////////////////////////////////
void ImageDisplayView::setViewSize(Dimension w, Dimension h, Boolean scrollbars)
{
   unsigned char sbDP;

   if (!_iw)
      return;		// oops

   if (scrollbars) {
      sbDP = _SaveScrollBarDisplayPolicyDuringZoom2Fit;
   }
   else {
      if (!_zoom2Fit) {		// save it if not already
         XtVaGetValues(_iw, XvicNscrollBarDisplayPolicy,
		&_SaveScrollBarDisplayPolicyDuringZoom2Fit, NULL);
      }
      sbDP = XvicNEVER;
   }

   XtVaSetValues(_iw, XvicNviewWidth, w, XvicNviewHeight, h,
	XvicNscrollBarDisplayPolicy, sbDP, NULL);
}

////////////////////////////////////////////////////////////////
// Set the window title to the image file name without the 
// directory path.  
// Ex.: The title string comes in the following form:
// 	<path><filename>,<path><filename>,<path><filename> .
// 	The window title is set to the string in the following form:
// 	<appName>: <filename>,<filename>,<filename> 
////////////////////////////////////////////////////////////////
void ImageDisplayView::setTitle(char *title)
{
   if ((title == NULL) || (strlen(title) <= 0))
      return;

   // Find the shell widget

   Widget shell = _w;
   do {
      shell = XtParent(shell);
   } while (shell && !XtIsShell(shell));

#ifdef __VMS
   char delimiter = ']';
   char specialVmsDelimiter = ':';
#else
   char delimiter = '/';
#endif

   // Duplicate title string, because strtok will modify it

   char *buf = strdup(title);

   // Get application name.  The returned strings are owned by the Intrinsics
   // and must not be modified
 
   String appName, appClass;
   XtGetApplicationNameAndClass(theApplication->display(),
				&appName, &appClass);

   // Allocate enough space to hold the title string

   char *winTitle = new char [strlen(buf) + strlen(appName) + 5];
   char *oneFileFullName = strtok(title, ",");
   char *oneFileName;
   if (oneFileFullName == NULL) {
      oneFileName = title;
   }
   else {
      // Remove the path, leave the filename only
     if ((oneFileName = strrchr(oneFileFullName, delimiter)))
         oneFileName++;			// Skip delimiter
#ifdef __VMS
      else if (oneFileName = strrchr(oneFileFullName, specialVmsDelimiter))
	 oneFileName++;   		// Skip delimiter
#endif
      else oneFileName = title;         // No delimiters
   }

   strcpy(winTitle, appName);
   strcat(winTitle, ": ");
   strcat(winTitle, oneFileName);

   while ((oneFileFullName = strtok(NULL, ",")) != NULL) {
     if ((oneFileName = strrchr(oneFileFullName, delimiter)))
         oneFileName++;
#ifdef __VMS
      else if (oneFileName = strrchr(oneFileFullName, specialVmsDelimiter))
	 oneFileName++; 
#endif
      else
         oneFileName = oneFileFullName;

      strcat(winTitle, ", ");
      strcat(winTitle, oneFileName);
   }

   XtVaSetValues(shell, XmNtitle, winTitle, NULL);

   delete [] winTitle;
   delete [] buf;
}

