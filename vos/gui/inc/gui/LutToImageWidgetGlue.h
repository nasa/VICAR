////////////////////////////////////////////////////////////////////////
// LutToImageWidgetGlue: class that serves as a "glue" class between a
// set of LUT objects and an Image Widget instance.  Whenever the LUT
// changes, update() writes that LUT out to the image widget.
// This class creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef LUTTOIMAGEWIDGETGLUE_H
#define LUTTOIMAGEWIDGETGLUE_H
#include "LutView.h"

class LutToImageWidgetGlue : public LutView {

 private:

   static void modeChangedCallback(Widget, XtPointer, XtPointer);

 protected:

   Widget _iw;
   Boolean _isStretch;		// True for stretch table, False for pseudocolor

 public:

   LutToImageWidgetGlue (Lut *lutR, Lut *lutG, Lut *lutB,
		Widget iw, Boolean isStretch);

   LutToImageWidgetGlue (Lut *lut, Widget iw, Boolean isStretch);

   virtual ~LutToImageWidgetGlue();

   virtual void update();	// the whole reason for the class existing

   virtual const char *const className() { return  "LutToImageWidgetGlue"; }

};
#endif
