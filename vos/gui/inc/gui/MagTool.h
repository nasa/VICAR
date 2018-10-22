////////////////////////////////////////////////////////////////////////
// Component that implements a magnifying glass tool.  It presents a
// small zoomed-out portion of the same image.  The window has to be
// positioned in the way that the cursor is always in the middle of the
// magnifying glass.  Moving the cursor within the main image will change
// what's displayed inside the magnifying glass.  Moving the cursor outside
// of the main image removes the mag glass window.
////////////////////////////////////////////////////////////////////////

#ifndef MAGTOOL_H
#define MAGTOOL_H

#include "ImageDisplayView.h"
#include "XvicImage.h"

class CursorPositionView;
class CursorDnView;
class MagInfo;
class Lut;
class LutToImageWidgetGlue;

class MagTool : public ImageDisplayView {

 private:

   static void bigWidgetChangeCallback(Widget, XtPointer, XtPointer);
   static void cursorCallback(Widget, XtPointer, XtPointer);
   static void inputCallback(Widget, XtPointer, XtPointer);
   static void motionHandler(Widget, XtPointer, XEvent *, Boolean *);

 protected:

   Widget _big_iw;
   int _width, _height;
   int _init_xin, _init_xout, _init_yin, _init_yout;
   CursorPositionView *_posView;
   CursorDnView *_dnView;
   MagInfo *_magInfo;

   LutToImageWidgetGlue *_lTIWG, *_pseudoLTIWG;

   void setInitZoom();
   void bigWidgetChange(XtPointer);
   void cursor(XtPointer);
   void motion(XMotionEvent *);
   void input(XtPointer);

   void copyDisplayModeResources();
   void copyDataRangeResources();
   Boolean isOnImage(XvicImageCallbackStruct * cb);
   Boolean isOnImage(int x, int y);

 public:

   MagTool(Widget parent, const char *name, ImageData *model, Widget _big_iw,
		Lut *rlut, Lut *glut, Lut *blut,
		Lut *rpseudo, Lut *gpseudo, Lut *bpseudo,
		MagInfo *magInfo=NULL,
		CursorPositionView * = NULL, CursorDnView * = NULL,
		Dimension view_height=100, Dimension view_width=100);
   virtual ~MagTool();

   virtual void manage();

   void setSize(Dimension width, Dimension height);

   virtual const char *const className () { return "MagTool"; }

};

#endif
