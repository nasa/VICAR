//////////////////////////////////////////////////////////
// LutToImageWidgetGlue.cc: class that serves as a "glue" class between a
// set of LUT objects and an Image Widget instance.  Whenever the LUT
// changes, update() writes that LUT out to the image widget.
// This class creates no widget, therefore it should never be managed.
// Note:  if the image changes modes, the LUTs need to be reloaded,
// since the BW stretch goes in a different place than the color stretch.
// This is handled automatically by attaching a mode-changed callback
// to the widget.
//////////////////////////////////////////////////////////
#include "LutToImageWidgetGlue.h"
#include "Lut.h"
#include "XvicImage.h"

LutToImageWidgetGlue::LutToImageWidgetGlue(Lut *lutR, Lut *lutG, Lut *lutB,
		Widget iw, Boolean isStretch)
	: LutView ( "iwglue", lutR, lutG, lutB)
{
   _iw = iw;
   _isStretch = isStretch;

   if (_lut)
      _lut ->attachView(this);
   if (_lut1)
      _lut1->attachView(this);
   if (_lut2)
      _lut2->attachView(this);

   XtAddCallback(_iw, XvicNvisibleAreaCallback,
		&LutToImageWidgetGlue::modeChangedCallback, (XtPointer) this);
}

LutToImageWidgetGlue::LutToImageWidgetGlue(Lut *lut,
                Widget iw, Boolean isStretch)
        : LutView ( "iwglue", lut)
{
   _iw = iw;
   _isStretch = isStretch;

   if (_lut)
      _lut ->attachView(this);

   XtAddCallback(_iw, XvicNvisibleAreaCallback,
                &LutToImageWidgetGlue::modeChangedCallback, (XtPointer) this);
}

LutToImageWidgetGlue::~LutToImageWidgetGlue()
{
   XtRemoveCallback(_iw, XvicNvisibleAreaCallback,
                &LutToImageWidgetGlue::modeChangedCallback, (XtPointer) this);
}

void LutToImageWidgetGlue::update()
{
   unsigned char mode;
   int *arrayR = NULL;
   int *arrayG = NULL;
   int *arrayB = NULL;

   if (_lut)
      arrayR = _lut->getAsArray();
   if (_lut1)
      arrayG = _lut1->getAsArray();
   if (_lut2)
      arrayB = _lut2->getAsArray();

   // Get the color/BW mode so we can determine which LUTs to set

   XtVaGetValues(_iw, XvicNimageMode, &mode, NULL);

   // Now set the stretch in the image widget.

   if (mode == XvicCOLOR) {
      if (_isStretch)
         XvicImageSetColorLUT(_iw, arrayR, arrayG, arrayB);
      // Pseudocolor table in COLOR mode has no effect
   }
   else {			// BW mode
      if (_isStretch)
         XvicImageSetMonoLUT(_iw, arrayR);	// stretch table
      else
         XvicImageSetColorLUT(_iw, arrayR, arrayG, arrayB); // pseudocolor table
   }
}

void LutToImageWidgetGlue::modeChangedCallback(Widget, XtPointer clientData,
						       XtPointer callData)
{
   LutToImageWidgetGlue *obj = (LutToImageWidgetGlue *)clientData;
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)callData;

   // We do this check here instead of in modeChanged() for efficiency,
   // because this callback is called many times (like once per pan).
   // Also, it allows us to just call the existing update() function.

   if (cb->reason != XvicCR_VISIBLE_AREA || !(cb->flags & XvicMODE_CHANGED))
      return;

   obj->update();
}

