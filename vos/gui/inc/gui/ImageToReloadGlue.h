////////////////////////////////////////////////////////////////////////
// ImageToReloadGlue: class that serves as a "glue" class between an
// ImageData object and a ReloadFileCmd object.
//
// This class, though a UIComponent, creates no widget, and therefore 
// should never be managed.
////////////////////////////////////////////////////////////////////////

#ifndef IMAGETORELOADGLUE_H
#define IMAGETORELOADGLUE_H
#include "BasicImageView.h"

class ImageData;
class Cmd;

class ImageToReloadGlue : public BasicImageView {

 private: 

 protected:

   Cmd *_reloadFileCmd;

 public:

   ImageToReloadGlue (ImageData *model, Cmd *reloadFileCmd);

   virtual ~ImageToReloadGlue ();

   virtual void update();
   virtual void updatePart(int flags);

   virtual const char *const className() { return  "ImageToReloadGlue"; }

};
#endif

