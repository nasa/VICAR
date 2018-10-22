////////////////////////////////////////////////////////////////////////
// SiCursorStretchInterface.h: Cursor-controlled linear stretch.
// Vertical motion controls contrast (slope of line) while horizontal
// motion controls brightness (y-intercept of line).
// Although this is a CmdInterface, there is no specific screen
// control for it... merely turn it on and it uses cursor callbacks
// on the IW and turns itself off.
////////////////////////////////////////////////////////////////////////
#include "SiCursorStretchInterface.h"
#include "StretchValue.h"
#include "XvicImage.h"
#include "Cmd.h"
#include "CallbackCompressor.h"

#include <math.h>

// Initialize statics
Pixmap SiCursorStretchInterface::_cursorPixmap = None;
Pixmap SiCursorStretchInterface::_cursorMask = None;

// See cursorMoved() for description
static const double PI = 3.14159265358979323846;
static const double tanSlope = - PI / 2.0;
static const double tanOffs = PI / 2.0 + .000001;	// .000001 keeps gain>0

////////////////////////////////////////////////////////////////////////
// Cursor shape for the interactive stretch
////////////////////////////////////////////////////////////////////////

#define cur_str_width 32
#define cur_str_height 32
#define cur_str_x_hot 16
#define cur_str_y_hot 14
static unsigned char cur_str_bits[] = {
   0xff, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xfc, 0xff, 0xff, 0xbf, 0xf9, 0xff,
   0xff, 0xdf, 0xf0, 0xff, 0xff, 0xef, 0xe1, 0xff, 0xff, 0xef, 0xe0, 0xff,
   0xff, 0xef, 0xe1, 0xff, 0xff, 0xdf, 0xf0, 0xff, 0x7f, 0xbf, 0xf9, 0xfd,
   0x7f, 0x7f, 0xfc, 0xfd, 0x37, 0xf6, 0xde, 0xd8, 0xcf, 0xf9, 0x3e, 0xe0,
   0xef, 0xfb, 0x3e, 0xe0, 0xf7, 0xf7, 0x1e, 0xc0, 0xf1, 0x07, 0x00, 0x00,
   0xf7, 0xf7, 0x1e, 0xc0, 0xef, 0xfb, 0x3e, 0xe0, 0xcf, 0xf9, 0x3e, 0xe0,
   0x37, 0xf6, 0xde, 0xd8, 0x7f, 0x7f, 0xfd, 0xfd, 0x7f, 0xbf, 0xfa, 0xfd,
   0xff, 0x5f, 0xf5, 0xff, 0xff, 0xaf, 0xea, 0xff, 0xff, 0x5f, 0xf5, 0xff,
   0xff, 0xaf, 0xea, 0xff, 0xff, 0x5f, 0xf5, 0xff, 0xff, 0xbf, 0xfa, 0xff,
   0xff, 0x7f, 0xfd, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};

#define cur_str_msk_width 32
#define cur_str_msk_height 32
static unsigned char cur_str_msk_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x01, 0x00, 0x00, 0xe0, 0x03, 0x00,
   0x00, 0xf0, 0x07, 0x00, 0x00, 0xf8, 0x0f, 0x00, 0x00, 0xfc, 0x1f, 0x00,
   0x00, 0xfc, 0x1f, 0x00, 0x00, 0xfc, 0x1f, 0x00, 0xe0, 0xf8, 0x8f, 0x03,
   0xe0, 0xf8, 0x87, 0x23, 0xec, 0xec, 0xb3, 0x33, 0xfe, 0x8f, 0xf9, 0x3f,
   0xfc, 0x8f, 0xf1, 0x3f, 0xfc, 0x87, 0xf1, 0x1f, 0xff, 0xff, 0xff, 0x7f,
   0xff, 0xff, 0xff, 0x7f, 0xff, 0x9f, 0xfd, 0x7f, 0xfc, 0x87, 0xf1, 0x1f,
   0xfc, 0x8f, 0xf1, 0x3f, 0xfe, 0x8f, 0xf9, 0x3f, 0xe4, 0xe4, 0x93, 0x13,
   0xe0, 0xf0, 0x87, 0x03, 0x00, 0xf8, 0x0f, 0x00, 0x00, 0xfc, 0x1f, 0x00,
   0x00, 0xfc, 0x1f, 0x00, 0x00, 0xfc, 0x1f, 0x00, 0x00, 0xf8, 0x0f, 0x00,
   0x00, 0xf0, 0x07, 0x00, 0x00, 0xe0, 0x03, 0x00, 0x00, 0xc0, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

SiCursorStretchInterface::SiCursorStretchInterface(Widget, Cmd *cmd,
						Widget iw)
		: CmdInterface(cmd)
{
   // No _w is created!  So don't try to manage this.

   _iw = iw;		// save image widget to use

   _undoStretchValue = NULL;
   _saveCursor = NULL;

   _active = False;

   _cb_compress = NULL;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

SiCursorStretchInterface::~SiCursorStretchInterface()
{
   if (_saveCursor)
      delete _saveCursor;
   if (_cb_compress)
      delete _cb_compress;
}

////////////////////////////////////////////////////////////////////////
// Enable the cursor stretch.
////////////////////////////////////////////////////////////////////////

void SiCursorStretchInterface::activate()
{
   if (_active)
      return;

   // Add the callback, with compression of events

   _cb_compress = new CallbackCompressor(
	&SiCursorStretchInterface::cursorMovedCallback, (XtPointer)this,
	sizeof(XvicImageCallbackStruct), False);

   XtAddCallback(_iw, XvicNcursorCallback,
	&CallbackCompressor::callback, (XtPointer)_cb_compress);

   // Save certain values to restore later...

   XtVaGetValues(_iw, XvicNtrackFloatingCursor, &_saveTrackFC,
		XvicNcursorMode, &_saveCursorMode,  NULL);
   if (_saveCursorMode == XvicPLANTED)
      XtVaGetValues(_iw,
		XvicNcursorXfp, &_saveCursX, XvicNcursorYfp, &_saveCursY, NULL);

   // Make sure we're tracking the cursor, etc.

   XtVaSetValues(_iw, XvicNtrackFloatingCursor, True,
		XvicNcursorMode, XvicFLOATING, NULL);

   // Create cursors if necessary (can't be done in ctor because windows
   // aren't realized yet).

   if (_cursorPixmap == None)
      _cursorPixmap =
	XCreateBitmapFromData(XtDisplay(_iw), XtWindow(_iw),
			      (char *) cur_str_bits, cur_str_width, cur_str_height);
   if (_cursorMask == None)
      _cursorMask =
	XCreateBitmapFromData(XtDisplay(_iw), XtWindow(_iw),
			      (char *) cur_str_msk_bits, cur_str_msk_width, cur_str_msk_height);

   // Set up the cursor

   if (_saveCursor != NULL)
      delete _saveCursor;
   _saveCursor = NULL;
   char *curs;		// GetValues returns ptr to internal string
   XtVaGetValues(_iw, XvicNcursor, &curs, NULL);
   if (curs != NULL)
      _saveCursor = strdup(curs);
   if (_saveCursor != NULL) {	// don't change cursor if we can't set it back
      if (_cursorPixmap != None && _cursorMask != None)
         XvicImageSetPixmapCursor(_iw, _cursorPixmap, _cursorMask,
				cur_str_x_hot, cur_str_y_hot);
   }

   // If the current stretch is linear, warp the cursor to the corresponding
   // location as a starting point.

   setLocationFromStretch();

   _active = True;
}

////////////////////////////////////////////////////////////////////////
// Disable the cursor stretch.
////////////////////////////////////////////////////////////////////////

void SiCursorStretchInterface::deactivate()
{

   if (!_active)
      return;

   XtRemoveCallback(_iw, XvicNcursorCallback, 
	&CallbackCompressor::callback, (XtPointer)_cb_compress);
   delete _cb_compress;

   // Reset the cursor

   if (_saveCursor)
      XtVaSetValues(_iw, XvicNcursor, _saveCursor, NULL);

   // Restore saved values

   XtVaSetValues(_iw, XvicNtrackFloatingCursor, _saveTrackFC,
		XvicNcursorMode, _saveCursorMode, NULL);
   if (_saveCursorMode == XvicPLANTED)
      XtVaSetValues(_iw, XvicNcursorXfp, XvicDOUBLE_ARG(_saveCursX),
			 XvicNcursorYfp, XvicDOUBLE_ARG(_saveCursY), NULL);

   _active = False;

}

////////////////////////////////////////////////////////////////////////
// Callback for mouse movement
////////////////////////////////////////////////////////////////////////

void SiCursorStretchInterface::cursorMovedCallback(Widget,
				XtPointer clientData, XtPointer callData)
{
   SiCursorStretchInterface *obj = (SiCursorStretchInterface *)clientData;

   obj->cursorMoved(callData);
}

void SiCursorStretchInterface::cursorMoved(XtPointer callData)
{
   XvicImageCallbackStruct *cbs = (XvicImageCallbackStruct *)callData;

   int x1, x2, y1, y2;
   double x, y;

   // Determine position of cursor within display, normalize to 0..1

   XvicImageDisplayBounds(_iw, &x1, &y1, &x2, &y2);

   x = (cbs->x_fp - x1) / (x2 - x1);
   if (x < 0.0)
      x = 0.0;
   if (x > 1.0)
      x = 1.0;

   y = (cbs->y_fp - y1) / (y2 - y1);
   if (y < 0.0)
      y = 0.0;
   if (y > 1.0)
      y = 1.0;

   // Calculate stretch bounds based on cursor.  This is ripped off from VIDS.
   // But since I (rgd) wrote it in the first place, that's okay...

   // Vertical motion controls contrast (slope of stretch), while horizontal
   // motion controls brightness (y-intercept of stretch).

   // Use a linear transform to define an angle which , when fed into
   // a tangent function, gives a gain for the linear stretch.  The tan
   // function is used because it varies from 0 to infinity on a known
   // range (0 - pi/2) with a value of one at the center point (pi/4).

   double gain = tan(y * tanSlope + tanOffs);
   double offset = x * 256.0;
   offset = offset - (gain * (256.0 - offset));

   double low = -offset / gain;
   double high = (255.0 - offset) / gain;

   if (high < low)		// avoid ugly flashing if it inverts
      high = low + 0.000001;

   // Now actually do the stretch

   StretchValue *str = new StretchValue;

   str->stretchName = LINEAR;
   str->band = STR_ALL;

   str->low = low;
   str->high = high;

   runCmd(str);

}

////////////////////////////////////////////////////////////////////////
// Invert the process... given a stretch, position the cursor appropriately
////////////////////////////////////////////////////////////////////////

void SiCursorStretchInterface::setLocationFromStretch()
{
   StretchValue *str;
   double low, high;
   double gain, offset;
   double x, y;
   int x1, x2, y1, y2;
   double x_coord, y_coord;

   // Get current stretch value from Cmd

   if (_cmd == NULL)
      return;
   str = (StretchValue *)_cmd->getValue();
   if (str == NULL) {		// initial value
      low = 0.0;
      high = 255.0;
   }
   else {

      // See if it's a type we can handle

      if (str->stretchName == ASTRETCH) {	// percent stretch
	 switch (str->band) {
	   case STR_ALL: 
	   case STR_RED:
	       low = str->lPercValueRed;
	       high = str->hPercValueRed;
	       break;
	   case STR_GREEN:
	       low = str->lPercValueGrn;
               high = str->hPercValueGrn;
	       break;
	   case STR_BLUE:
	       low = str->lPercValueBlu;
               high = str->hPercValueBlu;
	       break;
	 }
      }
      else if (str->stretchName == LINEAR) {
         low = str->low;
         high = str->high;
      }
      else
         return;
   }

   if (high < low)
      return;		// can't do anything with inverse stretch
   //!!!! (Actually, we could set inverse post-stretch, but until the i/f
   //!!!! can do this, we shouldn't here)

   // The following is just a mathematical inversion of the formulas in
   // cursorMoved() above.

   if (high == low)
      high = low + 0.000001;	// avoid divide by 0

   gain = 255.0 / (high - low);
   offset = - low * gain;

   offset = (offset + 256.0 * gain) / (1.0 + gain);

   x = offset / 256.0;
   y = (atan(gain) - tanOffs) / tanSlope;

   // Convert from normalized coordinates back to the display

   XvicImageDisplayBounds(_iw, &x1, &y1, &x2, &y2);

   x_coord = x * (x2 - x1) + x1;
   y_coord = y * (y2 - y1) + y1;

   if (x_coord < x1)
      x_coord = x1;
   if (x_coord > x2)
      x_coord = x2;
   if (y_coord < y1)
      y_coord = y1;
   if (y_coord > y2)
      y_coord = y2;

   XtVaSetValues(_iw, XvicNcursorXfp, XvicDOUBLE_ARG(x_coord),
		      XvicNcursorYfp, XvicDOUBLE_ARG(y_coord), NULL);
}

