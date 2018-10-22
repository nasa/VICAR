////////////////////////////////////////////////////////////////////////
// Compenent that implements a pan tool.  This tool is intended to control
// another image widget display.  It presents a small zoom-to-fit version
// of the same image, with a box in the overlay representing the area
// currently being displayed in the other window.  Various actions allow
// the user to move the box in this display, which changes the pan on the
// other widget.  Likewise, other methods of changing the pan on the main
// widget will cause the box to move.
////////////////////////////////////////////////////////////////////////
#ifndef PANTOOL_H
#define PANTOOL_H

#include "ImageDisplayView.h"
#include "XvicImage.h"

class Lut;
class LutToImageWidgetGlue;

#define XvicNpanBoxColor	"panBoxColor"
#define XvicCPanBoxColor	"PanBoxColor"
#define XvicNapplyStretchToPan	"applyStretchToPan"
#define XvicCApplyStretchToPan	"ApplyStretchToPan"

class PanTool : public ImageDisplayView {
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

   void bigWidgetChange(XtPointer);
   void input(XtPointer);

   void drawNewBox();
   void moveBox();

   void setAspectRatio();
   void copyDisplayModeResources();
   void copyDataRangeResources();

   void newSize();

 public:

   PanTool(Widget parent, const char *name, ImageData *model, Widget _big_iw,
		Dimension view_height, Dimension view_width,
		Boolean preserve_aspect = True,
		Lut *rlut=NULL, Lut *glut=NULL, Lut *blut=NULL,
		Lut *rplut=NULL, Lut *gplut=NULL, Lut *bplut=NULL);
   ~PanTool();

};

#endif

