////////////////////////////////////////////////////////////////////////
// MarksToValueGlue: class that serves as a "glue" class between an
// PseudoMarks object and a PseudoValue object.  The class is a
// View to PseudoMarks, so whenever it receives an update() from PseudoMarks,
// it recollects the LUT (which in turn cause it to update its
// own view(s)).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#include "MarksToValueGlue.h"
#include "PseudoMarks.h"
#include "PseudoValue.h"
#include "PseudoCmdInterface.h"

MarksToValueGlue::MarksToValueGlue(Widget, const char *name, PseudoMarks *marks,
        PseudoValue *value, PseudoCmdInterface *pci) : BasicWedgeOverlay (name)
{
   _value = value;
   _pseudoCmdInterface = pci;

   _collectionActive = NULL;

   // This should be done by WedgeOverlayView!!!!
   marks->attachView(this);
}

////////////////////////////////////////////////////////////////////////
// This is the meat of the function.  Whenever the marks change,
// regenerate the pseudocolor tables.  Note that when ROI is implemented, this
// class should be a "view" of the ROI rather than the image model directly.
////////////////////////////////////////////////////////////////////////
void MarksToValueGlue::update(PseudoMarks *marks)
{
   if (_value) {
        _value->regenerate(marks);
        _pseudoCmdInterface->loadTable(_value);
   }
}

void MarksToValueGlue::update(PseudoValue *value)
 {
   value=0;  // Empty. Assigned to 0 to remove compiler warnings
 }
