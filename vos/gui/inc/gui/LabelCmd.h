/////////////////////////////////////////////////////////////
//
//   LabelCmd.h: 
//
//   This is a class derived from NoUndoCmd.
//   It is for setting up the image label display window.
//
////////////////////////////////////////////////////////////
#ifndef LABELCMD_H
#define LABELCMD_H
#include <X11/Intrinsic.h>
#include "NoUndoCmd.h"

class LabelWindow;
class ImageData;

class LabelCmd : public NoUndoCmd {

  private:

     int _created;
     ImageData *_image;
     LabelWindow *_labelWindow;

  protected:
    
     virtual void doit();

  public:
    
     LabelCmd( const char *, int, ImageData *);
     virtual ~LabelCmd();
     void resetLabelWindow();
     int isCreated() { return _created; }
     void setImage(ImageData *image) { _image = image; }
     virtual const char *const className () { return "LabelCmd"; }
};
#endif
