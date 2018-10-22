////////////////////////////////////////////////////////////////////////
// ImageToHistGlue: class that serves as a "glue" class between an
// ImageData object and a (set of) Histogram objects.  The class is a
// View to ImageData, so whenever it receives an update() from ImageData,
// it recollects the histograms (which in turn cause them to update their
// own views).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#include "ImageToHistGlue.h"
#include "Histogram.h"
#include "CollectHistBG.h"
#include "ImageData.h"

ImageToHistGlue::ImageToHistGlue (ImageData *model,
			Histogram *histR, Histogram *histG, Histogram *histB)
		: BasicImageView("glue", model)
{
   _histR = histR;
   _histG = histG;
   _histB = histB;

   _collectionActive = NULL;

   _model->attachView(this);
}

ImageToHistGlue::~ImageToHistGlue ( )
{
    // Detach itself from the model so that the are no more updates sent
    _model->detachView(this);
}

////////////////////////////////////////////////////////////////////////
// This is the meat of the function.  Whenever the image changes,
// recompute the histogram.  Note that when ROI is implemented, this
// class should be a "view" of the ROI rather than the image model directly.
////////////////////////////////////////////////////////////////////////
void ImageToHistGlue::update()
{

   updatePart(IMAGE_DATA_UPDATE_RANGE);		// Set hist limits

   if (_model->isDataSourceOpened()) {
      CollectHistBG(_model, _histR, _histG, _histB, &_collectionActive);
   }
   else {		// No data, so clear out histograms
      if (_histR)
         _histR->clear();
      if (_histG)
         _histG->clear();
      if (_histB)
         _histB->clear();
   }
}

void ImageToHistGlue::updatePart(int flags)
{
   if (flags & IMAGE_DATA_UPDATE_RANGE) {
      if (_model->getPixelType().isIntegral()) {
         if (_histR)
            _histR->setLimits((int)_model->getMinDataRange(),
			      (int)_model->getMaxDataRange());
         if (_histG)
            _histG->setLimits((int)_model->getMinDataRange(),
			      (int)_model->getMaxDataRange());
         if (_histB)
            _histB->setLimits((int)_model->getMinDataRange(),
			      (int)_model->getMaxDataRange());
      }
      else {
         if (_histR)
            _histR->setLimits(_model->getMinDataRange(),
			      _model->getMaxDataRange());
         if (_histG)
            _histG->setLimits(_model->getMinDataRange(),
			      _model->getMaxDataRange());
         if (_histB)
            _histB->setLimits(_model->getMinDataRange(),
			      _model->getMaxDataRange());
      }
   }
}

