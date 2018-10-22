////////////////////////////////////////////////////////////////////////
// MarksToValueGlue: class that serves as a "glue" class between an
// PseudoMarks object and a PseudoValue object.  The class is a
// View to PseudoMarks, so whenever it receives an update() from PseudoMarks,
// it recollects the LUT (which in turn cause it to update its
// own view(s)).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef MARKSTOVALUEGLUE_H
#define MARKSTOVALUEGLUE_H
#include "BasicWedgeOverlay.h"

class PseudoMarks;
class PseudoValue;
class PseudoCmdInterface;

class MarksToValueGlue : public BasicWedgeOverlay {

 protected:

   PseudoValue *_value;
   PseudoCmdInterface *_pseudoCmdInterface;

   void *_collectionActive;

 public:

   MarksToValueGlue (Widget, const char*, PseudoMarks *, PseudoValue *, PseudoCmdInterface *);

   virtual void update(PseudoMarks *);	// the whole reason for the class existing
   virtual void update(PseudoValue *);

   virtual const char *const className() { return  "MarksToValueGlue"; }

};
#endif

