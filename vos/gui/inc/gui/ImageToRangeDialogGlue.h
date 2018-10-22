////////////////////////////////////////////////////////////////////////
// ImageToRangeDialogGlue: class that serves as a "glue" class between an
// ImageData object and the Data Range dialog.  The class is a View to
// ImageData, so whenever it receives an update() from ImageData,
// it tells the RangeDialog to update its min/max value displays.
// This class, even though it's a UIComponent, creates no widget,
// therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef IMAGETORANGEDIALOGGLUE_H
#define IMAGETORANGEDIALOGGLUE_H
#include "BasicImageView.h"

class ImageData;
class DataRangeDialog;

class ImageToRangeDialogGlue : public BasicImageView {

 private:

 protected:
   DataRangeDialog *_dialog;

 public:

   ImageToRangeDialogGlue (ImageData *model, DataRangeDialog *dialog);

   virtual ~ImageToRangeDialogGlue();

   virtual void update();	// the whole reason for the class existing
   virtual void updatePart(int flags);

   virtual const char *const className() { return  "ImageToRangeDialogGlue"; }

};
#endif

