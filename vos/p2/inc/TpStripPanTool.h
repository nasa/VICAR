////////////////////////////////////////////////////////////////////////
// TpStripPanTool.h: This component is very similar to StripPanTool and 
// should eventually be replaced with StripPanTool, but after there is 
// a fix to Motif bug that causes crashes when setUserZoom2Fit() is called 
// and scrollbar policy is set.
////////////////////////////////////////////////////////////////////////
#ifndef TPSTRIPPANTOOL_H
#define TPSTRIPPANTOOL_H

#include "ImageDisplayView.h"
#include "XvicImage.h"

class Lut;
class LutToImageWidgetGlue;

#define XvicNpanBoxColor	"panBoxColor"
#define XvicCPanBoxColor	"PanBoxColor"
#define XvicNapplyStretchToPan	"applyStretchToPan"
#define XvicCApplyStretchToPan	"ApplyStretchToPan"

class TpStripPanTool : public ImageDisplayView {
 private:
   static void bigWidgetChangeCallback(Widget, XtPointer, XtPointer);
   static void inputCallback(Widget, XtPointer, XtPointer);

   static XtResource _resources[];

   Dimension computeSizeX(int, int, Boolean, Widget);
   Dimension computeSizeY(int, int, Boolean, Widget);

 protected:
   Widget _big_iw;
   XvicID _box_id;
   String _box_color_string;	// From the resource
   XvicColor _box_color;
   XvicGC _box_gc;
   int _box_x, _box_y;		// Image coords of upper left
   int _input_x, _input_y;	// Saved coords for mouse-panning
   Boolean _preserve_aspect;	// True if aspect ratio of shell should be set
   Boolean _apply_stretch_to_pan;	// From the resource

   Lut *_rlut, *_glut, *_blut;
   Lut *_rplut, *_gplut, *_bplut;
   LutToImageWidgetGlue *_lutGlue, *_pseudoLutGlue;

   // Calculate zoom factor needed for image to fit window
                                                  
   virtual void calcZoomToFit(Widget iw, int *in, int *out);

   void bigWidgetChange(XtPointer);
   void input(XtPointer);

   void drawNewBox();
   void moveBox();

   void setAspectRatio();
   void copyDisplayModeResources();
   void copyDataRangeResources();

   void newSize();

 public:

   TpStripPanTool(Widget parent, const char *name, ImageData *model,
		Widget _big_iw,
		Dimension view_height, Dimension view_width,
		Boolean preserve_aspect = True,
		Lut *rlut=NULL, Lut *glut=NULL, Lut *blut=NULL,
		Lut *rplut=NULL, Lut *gplut=NULL, Lut *bplut=NULL);
   ~TpStripPanTool();

   virtual void setUserZoom2Fit();
};

#endif

