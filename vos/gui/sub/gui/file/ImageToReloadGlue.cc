////////////////////////////////////////////////////////////////////////
// ImageToReloadGlue: class that serves as a "glue" class between an
// ImageData object and a ReloadFileCmd object.
//
// This class, though a UIComponent, creates no widget, and therefore 
// should never be managed. 
////////////////////////////////////////////////////////////////////////
#include "ImageToReloadGlue.h"
#include "ImageData.h"
#include "ReloadFileCmd.h"

ImageToReloadGlue::ImageToReloadGlue (ImageData *model, Cmd *reloadFileCmd)
  : BasicImageView("glue", model)
{
  _reloadFileCmd = reloadFileCmd;
 
  _model->attachView(this);
}

ImageToReloadGlue::~ImageToReloadGlue ( )
{
    // Detach itself from the model so that the are no more updates sent
    _model->detachView(this);
}

////////////////////////////////////////////////////////////////////////
// Whenever the image changes, reset the ReloadCmd interface.
////////////////////////////////////////////////////////////////////////
void ImageToReloadGlue::update()
{
  if(_model->isDataSourceOpened())
    _reloadFileCmd->activate();
  else
    _reloadFileCmd->deactivate();
}

void ImageToReloadGlue::updatePart(int /* flags */) { }
