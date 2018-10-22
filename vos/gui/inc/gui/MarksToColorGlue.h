///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
#ifndef MARKSTOCOLORGLUE_H
#define MARKSTOCOLORGLUE_H
#include "InterpolationChooser.h"

class ColorModel;

class MarksToColorGlue : public InterpolationChooser {

 protected:

   ColorModel *_bwModel, *_colorModel;

 public:

   MarksToColorGlue(Widget parent, const char *name, InterpolationType type, 
		PseudoValue *value, PseudoMarks *marks, PseudoCmdInterface *pci,
		ColorModel *bwModel, ColorModel *colorModel) 
	: InterpolationChooser(parent, name, type, value, marks, pci) 
	  { _bwModel = bwModel; _colorModel = colorModel; }

   virtual void update (PseudoMarks *);
   virtual const char *const className() { return "MarksToColorGlue"; }
};
#endif

