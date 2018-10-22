////////////////////////////////////////////////////////////////////////
// ImageToPseudoGlue: class that serves as a "glue" class between an
// ImageData object and a (set of) Pseudocolor objects.  The class is a
// View to ImageData, so whenever it receives an update() from ImageData,
// it disables the pseudocolor tool if the image is color and enables it
// if the image is b&w.  This class, even though it's a UIComponent, 
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef IMAGETOPSEUDOGLUE_H
#define IMAGETOPSEUDOGLUE_H
#include "BasicImageView.h"

class ImageData;
class Cmd;
class MenuDialog;

class ImageToPseudoGlue : public BasicImageView {

 protected:

   Cmd        *_pseudoModeCmd;
   MenuDialog *_pseudoDialog;
   Cmd	      *_pseudoCmd;
   Cmd        *_postLutCmd;
   Cmd        *_postPseudoCmd;

 public:

   ImageToPseudoGlue(ImageData *model, Cmd *modeCmd, MenuDialog *pseudoDialog, 
                     Cmd *pseudoCmd, Cmd *postLutCmd, Cmd *postPseudoCmd);

   virtual void update();	// the whole reason for the class existing

   virtual const char *const className() { return  "ImageToPseudoGlue"; }

};
#endif

