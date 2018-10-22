////////////////////////////////////////////////////////////////
// PseudoColorView.h : Applies pseudocolor table to the wedge
////////////////////////////////////////////////////////////////
#ifndef PSEUDOCOLORVIEW_H
#define PSEUDOCOLORVIEW_H
#include "WedgeOverlayView.h"
#include "PseudoMarks.h"

class PseudoValue;
class ColorModel;

class PseudoColorView : public WedgeOverlayView {

  public:

	PseudoColorView(Widget parent, const char * name,
					PseudoValue *pseudoValue);
	~PseudoColorView() { }


	virtual void update ( PseudoValue * );
	virtual void update ( PseudoMarks * m ) { WedgeOverlayView::update(m); }
//	virtual void update ( ColorModel * );

	virtual const char *const className() { return ("PseudoColorView"); }
};
#endif
