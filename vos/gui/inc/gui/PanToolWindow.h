////////////////////////////////////////////////////////////////////////
// Compenent that creates a popup window containing a pan tool.
////////////////////////////////////////////////////////////////////////
#ifndef PANTOOLWINDOW_H
#define PANTOOLWINDOW_H

#include "MainWindow.h"
class PanTool;
class ImageData;
class Lut;

#define XvicNpanDesiredSize	"panDesiredSize"
#define XvicCpanDesiredSize	"PanDesiredSize"

class PanToolWindow : public MainWindow {

 private:
   static XtResource _resources[];

 protected:

   Widget _form;
   PanTool *_panTool;
   ImageData *_model;
   Widget _big_iw;

   int _pan_desired_size;		// from the resource
   Lut *_rlut, *_glut, *_blut;
   Lut *_rplut, *_gplut, *_bplut;

   virtual Widget createWorkArea(Widget);

 public:

   PanToolWindow(const char *name, ImageData *model, Widget big_iw,
	Lut *rlut=NULL, Lut *glut=NULL, Lut *blut=NULL,
	Lut *rplut=NULL, Lut *gplut=NULL, Lut *bplut=NULL);

};

#endif

