////////////////////////////////////////////////////////////////////////
// ImageToImageWindowGlue: 
////////////////////////////////////////////////////////////////////////
#include "ImageToImageWindowGlue.h"
#include "ImageData.h"
#include "ImageWindow.h"
#include "ImageDisplayer.h"

ImageToImageWindowGlue::ImageToImageWindowGlue (ImageData *model,
						ImageWindow *window)
		: BasicImageView("glue", model)
{
    _window = window;

   _model->attachView(this);
}

ImageToImageWindowGlue::~ImageToImageWindowGlue ( )
{
    // Detach itself from the model so that the are no more updates sent
    _model->detachView(this);
}

////////////////////////////////////////////////////////////////////////
// This is the meat of the class.  Whenever the image changes,
// reset the Lat/Lon interfaces.  Note that this function can also
// be used to perform other ImageWindow-related tasks that need to be
// done when the image changes or a new image is loaded.
////////////////////////////////////////////////////////////////////////
void ImageToImageWindowGlue::update()
{
  // Lat/Lon interfaces: check for an MP object and de/activate the IFs
  // accordingly
  
  if( _window->getImageData()->isMapProjInfoPresent() )
    _window->enableLatLonIFs(True); 
  else {
    _window->enableLatLonIFs(False);

    // If the image has no MP info but the LatLonBar is still showing, hide it
    // (i.e.; when the user loads a new image that can't take advantage of the
    //  lat/lon capability)
    
    ImageDisplayer *displayer = _window->getImageDisplayer();

    if ( displayer == NULL ) return;
    
    if( displayer->IsLatLonBarDisplayed())
      displayer->showLatLonBar( False );
  }
}

void ImageToImageWindowGlue::updatePart(int /* flags */) { }
