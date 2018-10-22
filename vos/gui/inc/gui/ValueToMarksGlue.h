////////////////////////////////////////////////////////////////////////
// ValueToMarksGlue: Class that serves as a "glue" class between an
// Pseudovalue object and a PseudoMarks object.  The class is a
// View to PseudoValue, so whenever it receives an update() from PseudoValue,
// it recollects the LUT (which in turn cause it to update its
// own view(s)).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef VALUETOMARKSGLUE_H
#define VALUETOMARKSGLUE_H
#include "BasicWedgeOverlay.h"

class PseudoMarks;
class PseudoValue;
class PseudoCmdInterface;

class ValueToMarksGlue : public BasicWedgeOverlay {

 protected:

   PseudoMarks *_marks;
   PseudoCmdInterface *_pseudoCmdInterface;

   void *_collectionActive;

 public:

   ValueToMarksGlue (Widget, const char*, PseudoMarks *, PseudoValue *, PseudoCmdInterface *);

   virtual void update(PseudoValue *);	// the whole reason for the class existing
   virtual void update(PseudoMarks *);  

   virtual const char *const className() { return  "ValueToMarksGlue"; }

};
#endif

