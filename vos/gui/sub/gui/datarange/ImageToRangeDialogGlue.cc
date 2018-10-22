////////////////////////////////////////////////////////////////////////
// ImageToRangeDialogGlue: class that serves as a "glue" class between an
// ImageData object and the Data Range dialog.  The class is a View to
// ImageData, so whenever it receives an update() from ImageData,
// it tells the RangeDialog to update its min/max value displays.
// This class, even though it's a UIComponent, creates no widget,
// therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#include "ImageToRangeDialogGlue.h"
#include "DataRangeDialog.h"
#include "ImageData.h"

ImageToRangeDialogGlue::ImageToRangeDialogGlue(ImageData *model,
					DataRangeDialog *dialog)
	: BasicImageView("range glue", model)
{
   _dialog = dialog;
   _model->attachView(this);
}

ImageToRangeDialogGlue::~ImageToRangeDialogGlue()
{
   _model->detachView(this);
}

void ImageToRangeDialogGlue::update()
{
   updatePart(IMAGE_DATA_UPDATE_RANGE);		// Set hist limits
}

void ImageToRangeDialogGlue::updatePart(int flags)
{
   if (flags & IMAGE_DATA_UPDATE_RANGE) {
      double min, max;
      _model->getDataRange(min, max);
      _dialog->setDataRange(min, max);
   }
}

