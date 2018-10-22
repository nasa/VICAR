///////////////////////////////////////////////////////
// InterpolationChooser.cc: A component class to choose the interpolation
// on the interval.  The choices are No Interpolation,
// Flat, Linear, Cubic Spline.  Only one option can be chosen
///////////////////////////////////////////////////////
#ifndef INTERPOLATIONCHOOSER_H
#define INTERPOLATIONCHOOSER_H
#include "UIComponent.h"
#include "PseudoDefs.h"

class PseudoValue;
class PseudoMarks;
class PseudoCmdInterface;

class InterpolationChooser : public UIComponent {

 private:

   static void valueChangedCallback(Widget, XtPointer, XtPointer);

 protected:

   Widget _none, _flat, _linear, _cubs;

   PseudoValue *_pseudoValue;
   PseudoMarks *_pseudoMarks;
   InterpolationType _type;
   PseudoCmdInterface *_pseudoCmdInterface;

 public:

   InterpolationChooser(Widget, const char *, InterpolationType, PseudoValue *,
		PseudoMarks *, PseudoCmdInterface *);
   virtual ~InterpolationChooser() { }

   virtual void valueChanged();
   virtual void setValue(InterpolationType);

   virtual void update (PseudoMarks *);
   virtual const char *const className() { return "InterpolationChooser"; }
};
#endif

