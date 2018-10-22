////////////////////////////////////////////////////////////////////////
// ImageToBlinkGlue: class that serves as a "glue" class between an
// ImageData object and the BlinkControl object.  It resets the filename
// whenever the image is updated.
//
// This class, though a UIComponent, creates no widget, and therefore 
// should never be managed. 
////////////////////////////////////////////////////////////////////////
#include "ImageToBlinkGlue.h"
#include "ImageData.h"
#include "BlinkControl.h"

ImageToBlinkGlue::ImageToBlinkGlue(ImageData *model, BlinkControl *blinkControl,
				int which)
		: BasicImageView("glue", model)
{
    _blinkControl = blinkControl;
    _which = which;
 
    _model->attachView(this);
}

ImageToBlinkGlue::~ImageToBlinkGlue ( )
{
    // Detach itself from the model so that the are no more updates sent
    _model->detachView(this);
}

////////////////////////////////////////////////////////////////////////
// Whenever the image changes, set the filename in the BlinkControl.
////////////////////////////////////////////////////////////////////////
void ImageToBlinkGlue::update()
{
    _blinkControl->setImageFilename(_which, _model->getInputDataSourceName());
}

void ImageToBlinkGlue::updatePart(int /* flags */) { }

