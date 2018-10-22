////////////////////////////////////////////////////////////////////////
// ValueToMarksGlue: Class that serves as a "glue" class between an
// Pseudovalue object and a PseudoMarks object.  The class is a
// View to PseudoValue, so whenever it receives an update() from PseudoValue,
// it recollects the LUT (which in turn cause it to update its
// own view(s)).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#include "ValueToMarksGlue.h"
#include "PseudoMarks.h"
#include "PseudoValue.h"
#include "PseudoCmdInterface.h"

ValueToMarksGlue::ValueToMarksGlue(Widget, const char *name, PseudoMarks *marks,
	PseudoValue *value, PseudoCmdInterface *pci) : BasicWedgeOverlay (name)
{
   _marks = marks;
   _pseudoCmdInterface = pci;

   _collectionActive = NULL;

   value->attachView(this);
}

////////////////////////////////////////////////////////////////////////
// This is the meat of the function.  Whenever the marks change,
// regenerate the pseudocolor tables.  Note that when ROI is implemented, this
// class should be a "view" of the ROI rather than the image model directly.
////////////////////////////////////////////////////////////////////////
void ValueToMarksGlue::update(PseudoValue *value)
{
   if (value) {
	// _marks->regenerate(marks);
        _pseudoCmdInterface->loadTable(value);
   }
}

void ValueToMarksGlue::update(PseudoMarks *marks)
{
	marks=0;  //assigned to 0 to elim. compiler warnings
	// WedgeOverlayView::update(marks);
}
