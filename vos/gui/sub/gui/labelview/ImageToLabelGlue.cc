////////////////////////////////////////////////////////////////////////
//
//    ImageToLabelGlue.cc: 
//
//    This is a class derived form BasicImageView that serves as 
//    a "glue" class between an ImageData object and a TextDisplayModel object.
//
//    This class, though a UIComponent, creates no widget, and therefore
//    should never be managed.
//
////////////////////////////////////////////////////////////////////////
#include "ImageToLabelGlue.h"
#include "ImageData.h"
#include "LabelCmd.h"

ImageToLabelGlue::ImageToLabelGlue (ImageData *model, LabelCmd *labelCmd)
   : BasicImageView("glue", model)
{
   _labelCmd = labelCmd;
 
   _model->attachView(this);
}

ImageToLabelGlue::~ImageToLabelGlue ( )
{
   // Detach itself from the model so that the are no more updates sent
   _model->detachView(this);
}

////////////////////////////////////////////////////////////////////////
// Whenever the image changes, reset the LabelCmd's image.   
////////////////////////////////////////////////////////////////////////
void ImageToLabelGlue::update()
{
   if (_model->isDataSourceOpened()) {
      _labelCmd->setImage(_model);
      if (_labelCmd->isCreated()) {
         _labelCmd->resetLabelWindow();
      }
   }
}

void ImageToLabelGlue::updatePart(int /* flags */) { }
