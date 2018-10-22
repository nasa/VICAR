////////////////////////////////////////////////////////////////////////
// Compenent that implements a pan tool.  This tool is intended to control
// another image widget display.  It presents a small zoom-to-fit version
// of the same image, with a box in the overlay representing the area
// currently being displayed in the other window.  Various actions allow
// the user to move the box in this display, which changes the pan on the
// other widget.  Likewise, other methods of changing the pan on the main
// widget will cause the box to move.  If the applyStretchToPan resource
// is True, then the passed-in LUT's are used to stretch the pan image.
// If it is False, the LUT's are ignored.
////////////////////////////////////////////////////////////////////////

#include "PanTool.h"
#include "XvicImage.h"
#include <math.h>
#include "LutToImageWidgetGlue.h"

// Resources for this class

XtResource PanTool::_resources[] = {
 {
   (char *)XvicNapplyStretchToPan,
   (char *)XvicCApplyStretchToPan,
   XmRBoolean,
   sizeof(Boolean),
   XtOffset(PanTool *, _apply_stretch_to_pan),
   XmRImmediate,
   (XtPointer) False,
 },
 {
   (char *)XvicNpanBoxColor,
   (char *)XvicCPanBoxColor,
   XmRString,
   sizeof(String),
   XtOffset(PanTool *, _box_color_string),
   XmRImmediate,
   (XtPointer) "red",
 },
};

PanTool::PanTool(Widget parent, const char *name, ImageData *model,
		Widget big_iw,
		Dimension view_height, Dimension view_width,
		Boolean preserve_aspect,
        	Lut *rlut, Lut *glut, Lut *blut,
		Lut *rplut, Lut *gplut, Lut *bplut) :
			// = True
	ImageDisplayView(parent, name, model,
		computeSizeY(view_height, view_width, preserve_aspect, big_iw),
		computeSizeX(view_height, view_width, preserve_aspect, big_iw))
{
   XGCValues values;

   _preserve_aspect = preserve_aspect;
   setAspectRatio();

   getResources(_resources, XtNumber(_resources));

   _big_iw = big_iw;
   setUserZoom2Fit();

   copyDisplayModeResources();
   copyDataRangeResources();

   // Add callbacks to keep track of the big widget

   XtAddCallback(_big_iw, XvicNvisibleAreaCallback,
		&PanTool::bigWidgetChangeCallback, (XtPointer) this);
   XtAddCallback(_big_iw, XvicNresizeCallback,
		&PanTool::bigWidgetChangeCallback, (XtPointer) this);
   XtAddCallback(_big_iw, XvicNpanCallback,
		&PanTool::bigWidgetChangeCallback, (XtPointer) this);

   // Add an Input callback so the user can manipulate our widget

   XtAddCallback(_iw, XvicNinputCallback,
		&PanTool::inputCallback, (XtPointer) this);

   // Connect the stretch to the main, if requested

   _lutGlue = NULL;
   _pseudoLutGlue = NULL;
   if (_apply_stretch_to_pan && rlut != NULL && glut != NULL && blut != NULL)
      _lutGlue = new LutToImageWidgetGlue(rlut, glut, blut, _iw, True);
   if (_apply_stretch_to_pan && rplut != NULL && gplut != NULL && bplut != NULL)
      _pseudoLutGlue = new LutToImageWidgetGlue(rplut, gplut, bplut, _iw,False);

   _box_id = 0;

   XColor xcolor;
   XParseColor(XtDisplay(_iw), DefaultColormapOfScreen(XtScreen(_iw)),
	_box_color_string, &xcolor);
   _box_color = XvicImageGetGrColor(_iw, &xcolor);

   values.line_width = 1;
   values.dashes = 4;
   values.line_style = LineOnOffDash;
   _box_gc=XvicImageCreateGC(_iw, GCLineWidth|GCDashList|GCLineStyle, &values);

   _input_x = 0;
   _input_y = 0;

   drawNewBox();

}

////////////////////////////////////////////////////////////////////////

PanTool::~PanTool()
{
   if (_lutGlue)
      delete _lutGlue;
   if (_pseudoLutGlue)
      delete _pseudoLutGlue;

   XtRemoveCallback(_big_iw, XvicNvisibleAreaCallback,
                &PanTool::bigWidgetChangeCallback, (XtPointer) this);
   XtRemoveCallback(_big_iw, XvicNresizeCallback,
                &PanTool::bigWidgetChangeCallback, (XtPointer) this);
   XtRemoveCallback(_big_iw, XvicNpanCallback,
                &PanTool::bigWidgetChangeCallback, (XtPointer) this);
   XtRemoveCallback(_iw, XvicNinputCallback,
                &PanTool::inputCallback, (XtPointer) this);

}

////////////////////////////////////////////////////////////////////////
// Callback from the big widget to let us know something changed.
////////////////////////////////////////////////////////////////////////

void PanTool::bigWidgetChangeCallback(Widget, XtPointer clientData,
					      XtPointer callData)
{
   PanTool *obj = (PanTool *)clientData;

   obj->bigWidgetChange(callData);
}

////////////////////////////////////////////////////////////////////////
// Something changed in the big widget.  Deal with it.
////////////////////////////////////////////////////////////////////////

void PanTool::bigWidgetChange(XtPointer callData)
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;

   switch (cb->reason) {

      case XvicCR_VISIBLE_AREA:
         if (cb->flags & XvicSIZE_CHANGED) {
            newSize();
            drawNewBox();
         }
         if ((cb->flags & XvicZOOM_CHANGED) ||
	     (cb->flags & XvicSIZE_CHANGED) ||
	     (cb->flags & XvicSUBPIXEL_CHANGED)) {
            drawNewBox();
         }
         else if (cb->flags & XvicPAN_CHANGED) {
            moveBox();		// drawNewBox will handle pans as well
         }
         if (cb->flags & XvicDITHER_CHANGED) {
            copyDisplayModeResources();
         }
         if (cb->flags & XvicRANGE_CHANGED) {
            copyDataRangeResources();
         }
         // MODE_CHANGED is ignored because we must be notified of that
         // (color/bw and data type) via the model.

         break;

      case XvicCR_RESIZE:
         setAspectRatio();
         drawNewBox();
         break;

      case XvicCR_PAN:
         moveBox();
   }
}

////////////////////////////////////////////////////////////////////////
// Callback from our widget when the user takes an action.
////////////////////////////////////////////////////////////////////////

void PanTool::inputCallback(Widget, XtPointer clientData,
				    XtPointer callData)
{
   PanTool *obj = (PanTool *)clientData;

   obj->input(callData);
}

////////////////////////////////////////////////////////////////////////
// Process user input from our widget.  You will notice a remarkable
// similarity between the Input actions here and the true Actions
// provided by the image widget.  Possible actions are:
//	Input(pan_mouse, start)	Start a mouse pan.  May be triggered by
//				ButtonPress/Release, Enter/LeaveNotify,
//				KeyPress/Release, or MotionNotify.
//	Input(pan_mouse, drag)	Continue a mouse pan.  This one actually does
//				a pan (based on the deltas from "start").
//				The same events are valid.  A "start" action
//				must happen first or the results are undefined.
//	Input(pan_one, direct)	Move box one image pixel in the indicated
//				direction (one of "left", "right", "up", "down")
//	Input(pan_half_view, direct) Move box half of its view size in the
//				indicated direction.
//	Input(pan_edge, direct)	Move box to edge of image in the indicated
//				direction.
////////////////////////////////////////////////////////////////////////

void PanTool::input(XtPointer callData)
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;
   int x_pan, y_pan;

   if (cb->reason != XvicCR_INPUT)
      return;				// oops

   if (cb->input_num_params != 2)
      return;				// oops

   XtVaGetValues(_big_iw, XvicNxPan, &x_pan, XvicNyPan, &y_pan, NULL);

   if (strcmp(cb->input_params[0], "pan_mouse") == 0) {
      if (strcmp(cb->input_params[1], "start") == 0) {
         _input_x = cb->x;
         _input_y = cb->y;
      }
      else if (strcmp(cb->input_params[1], "drag") == 0) {
         XtVaSetValues(_big_iw, XvicNxPan, x_pan + (cb->x - _input_x),
				XvicNyPan, y_pan + (cb->y - _input_y),
				NULL);
         _input_x = cb->x;
         _input_y = cb->y;
      }
   }

   else {
      int x_amount = 0;
      int y_amount = 0;

      if (strcmp(cb->input_params[0], "pan_one") == 0) {
         x_amount = 1;
         y_amount = 1;
      }
      else if (strcmp(cb->input_params[0], "pan_half_view") == 0) {
         // Don't just use viewH/W / 2 because we want Image coordinates
         int x1, x2, y1, y2;
         XvicImageDisplayBounds(_big_iw, &x1, &y1, &x2, &y2);
         x_amount = (x2-x1+1) / 2;
         y_amount = (y2-y1+1) / 2;
      }
      else if (strcmp(cb->input_params[0], "pan_edge") == 0) {
         XtVaGetValues(_big_iw, XvicNimageWidth, &x_amount,
				XvicNimageHeight, &y_amount, NULL);
      }

      if (x_amount == 0 && y_amount == 0)
         return;				// invalid string

      if (strcmp(cb->input_params[1], "left") == 0)
         XtVaSetValues(_big_iw, XvicNxPan, x_pan - x_amount, NULL);
      else if (strcmp(cb->input_params[1], "right") == 0)
         XtVaSetValues(_big_iw, XvicNxPan, x_pan + x_amount, NULL);
      else if (strcmp(cb->input_params[1], "up") == 0)
         XtVaSetValues(_big_iw, XvicNyPan, y_pan - y_amount, NULL);
      else if (strcmp(cb->input_params[1], "down") == 0)
         XtVaSetValues(_big_iw, XvicNyPan, y_pan + y_amount, NULL);
   }
}

////////////////////////////////////////////////////////////////////////
// Draw a new box, eraseing the old one first if necessary
////////////////////////////////////////////////////////////////////////

void PanTool::drawNewBox()
{
   int x1, x2, y1, y2;

   if (_box_id) {
      XvicImageEraseObject(_iw, _box_id);
   }

   // Get the currently displayed area

   XvicImageDisplayBounds(_big_iw, &x1, &y1, &x2, &y2);

   // Draw the box.  We use four separate lines rather than a rectangle or
   // polyline because the bounding box of a rect or polyline is the entire
   // enclosed area, while the bounding box for four separate lines is just
   // the lines.  Since the widget will repaint the whole bounding box,
   // this is actually much faster.

   // Top
   _box_id = XvicImageDrawLine(_iw, 0, _box_gc, _box_color, x1, y1, x2, y1);
   // Right
   XvicImageDrawLine(_iw, _box_id, _box_gc, _box_color, x2, y1, x2, y2);
   // Bottom
   XvicImageDrawLine(_iw, _box_id, _box_gc, _box_color, x2, y2, x1, y2);
   // Right
   XvicImageDrawLine(_iw, _box_id, _box_gc, _box_color, x1, y2, x1, y1);

   _box_x = x1;
   _box_y = y1;
}

////////////////////////////////////////////////////////////////////////
// Simply move the existing box based on the new pan
////////////////////////////////////////////////////////////////////////

void PanTool::moveBox()
{
   int x_pan, y_pan;

   if (_box_id == 0) {		// oops!
      drawNewBox();
      return;
   }

   XtVaGetValues(_big_iw, XvicNxPan, &x_pan, XvicNyPan, &y_pan, NULL);

   XvicImageMoveObject(_iw, _box_id, (x_pan - _box_x), (y_pan - _box_y));

   _box_x = x_pan;
   _box_y = y_pan;
}

////////////////////////////////////////////////////////////////////////
// Set the Shell's aspect ratio based on the image size
////////////////////////////////////////////////////////////////////////

void PanTool::setAspectRatio()
{
   Widget shell;

   if (!_preserve_aspect)
      return;

   if (!_iw)
      return;

   shell = _iw;
   do {
      shell = XtParent(shell);
   } while (shell && !XtIsShell(shell));

   if (shell) {		// found the shell
      int width, height;
      XtVaGetValues(_iw, XvicNimageWidth, &width, XvicNimageHeight, &height,
			NULL);
      XtVaSetValues(shell,
		XmNminAspectX, width, XmNmaxAspectX, width,
		XmNminAspectY, height, XmNmaxAspectY, height,
		NULL);
   }
}

////////////////////////////////////////////////////////////////////////
// Copy the display-related resources (those that could affect what colormap
// is used) from the big widget to here, to prevent flashing.
////////////////////////////////////////////////////////////////////////

void PanTool::copyDisplayModeResources()
{
   unsigned char bw_dither, color_dither, visual_type;
   unsigned char stretch_policy, colormap_policy, lut_type, lut16_type;
   int gray_levels, red_levels, green_levels, blue_levels;

   XtVaGetValues(_big_iw,
		XvicNvisualType, &visual_type,
		XvicNbwDither, &bw_dither,
		XvicNcolorDither, &color_dither,
		XvicNstretchPolicy, &stretch_policy,
		XvicNcolormapPolicy, &colormap_policy,
		XvicNlutType, &lut_type,
		XvicNlut16Type, &lut16_type,
		XvicNgrayLevels, &gray_levels,
		XvicNredLevels, &red_levels,
		XvicNgreenLevels, &green_levels,
		XvicNblueLevels, &blue_levels,
		NULL);

   XtVaSetValues(_iw,
		XvicNvisualType, visual_type,
		XvicNbwDither, bw_dither,
		XvicNcolorDither, color_dither,
		XvicNstretchPolicy, stretch_policy,
		XvicNcolormapPolicy, colormap_policy,
		XvicNlutType, lut_type,
		XvicNlut16Type, lut16_type,
		XvicNgrayLevels, gray_levels,
		XvicNredLevels, red_levels,
		XvicNgreenLevels, green_levels,
		XvicNblueLevels, blue_levels,
		NULL);
}

////////////////////////////////////////////////////////////////////////
// Copy the data-type resources from the big widget to here, so the
// display will be visible.
////////////////////////////////////////////////////////////////////////

void PanTool::copyDataRangeResources()
{
   double raw_data_min, raw_data_max;
   int scaled_data_max, output_data_max;

   XtVaGetValues(_big_iw,
		XvicNrawDataMin, &raw_data_min,
		XvicNrawDataMax, &raw_data_max,
		XvicNscaledDataMax, &scaled_data_max,
		XvicNoutputDataMax, &output_data_max,
		NULL);

   XtVaSetValues(_iw,
		XvicNrawDataMin, XvicDOUBLE_ARG(raw_data_min),
		XvicNrawDataMax, XvicDOUBLE_ARG(raw_data_max),
		XvicNscaledDataMax, scaled_data_max,
		XvicNoutputDataMax, output_data_max,
		NULL);
}

////////////////////////////////////////////////////////////////////////
// Called whenever the size of the main image changes.  We must set the
// imageW/H resources (which is taken care of by the model update but
// it's needed sooner than that for the aspect ratio), then we must force
// the window to be a new size.  Note that XmNallowShellResize must be True
// on the shell for this to work.  We set the tile size to the same as the
// image size so the widget does minimal work - the tile size will be reset
// when the model update is called.
////////////////////////////////////////////////////////////////////////

void PanTool::newSize()
{
   int width, height;
   Dimension old_width, old_height;
   Dimension new_width, new_height;

   if (!_preserve_aspect)
      return;

   if (!_iw)
      return;

   XtVaGetValues(_big_iw, XvicNimageWidth, &width, XvicNimageHeight, &height,
			NULL);
   XtVaSetValues(_iw, XvicNimageWidth, width, XvicNimageHeight, height,
		      XvicNtileWidth, width, XvicNtileHeight, height, NULL);

   setUserZoom2Fit();		// Recalc zoom since the size changed

   setAspectRatio();

   // Figure out what size to be to cover the same "area"

   XtVaGetValues(_iw, XvicNviewWidth, &old_width,
		      XvicNviewHeight, &old_height, NULL);

   new_width = computeSizeX(old_width, old_height, _preserve_aspect, _big_iw);
   new_height = computeSizeY(old_width, old_height, _preserve_aspect, _big_iw);

   XtVaSetValues(_iw, XvicNviewWidth, new_width, XvicNviewHeight, new_height,
			NULL);

}

////////////////////////////////////////////////////////////////////////
// Determine the size that most closely matches the desired one.
// We want the "area" to be about the same as the desired area, but it
// must match the aspect ratio.  The calling sequence for these is all
// messed up because it must be able to be called from the superclass
// constructor.  Is there a better way to do this??
////////////////////////////////////////////////////////////////////////

Dimension PanTool::computeSizeX(int desired_width, int desired_height,
	Boolean preserveAspect, Widget big_iw)
{
   int width, height;

   if (!preserveAspect)
      return desired_width;

   XtVaGetValues(big_iw, XvicNimageWidth, &width, XvicNimageHeight, &height,
			NULL);

   double aspect = (double)width / (double)height;

   return (int)sqrt(desired_width * desired_height * aspect);
}

Dimension PanTool::computeSizeY(int desired_width, int desired_height,
	Boolean preserveAspect, Widget big_iw)
{
   int width, height;

   if (!preserveAspect)
      return desired_height;

   XtVaGetValues(big_iw, XvicNimageWidth, &width, XvicNimageHeight, &height,
			NULL);

   double aspect = (double)width / (double)height;

   return (int)sqrt(desired_width * desired_height / aspect);
}

