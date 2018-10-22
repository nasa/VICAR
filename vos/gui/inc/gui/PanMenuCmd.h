/////////////////////////////////////////////////////////////
// PanMenuCmd.h: Include file to handle the Pan command
//                button from the Image widget.
/////////////////////////////////////////////////////////////
#ifndef PANMENUCMD_H
#define PANMENUCMD_H
#include "NoUndoCmd.h"
#include "PanToolWindow.h"

class MainWindow;
class ImageData;
class Lut;

class PanMenuCmd : public NoUndoCmd {

  private:

     int        _created;
     ImageData  * _imageData;
     Widget     _imageViewWidget;
     MainWindow *_panToolWindow;
     Lut	*_rlut, *_glut, *_blut, *_rplut, *_gplut, *_bplut;

  protected:
    
    virtual void doit();

  public:

    PanMenuCmd ( const char*, int, ImageData*, Widget,
		Lut *rlut=NULL,  Lut *glut=NULL,  Lut *blut=NULL,
		Lut *rplut=NULL, Lut *gplut=NULL, Lut *bplut=NULL);
    virtual const char *const className () { return "PanMenuCmd"; }
};
#endif
