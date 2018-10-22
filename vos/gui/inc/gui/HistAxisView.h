//////////////////////////////////////////////////////////////
// HistAxisView.h: A component class to show histogram axis
/////////////////////////////////////////////////////////////
#ifndef HISTAXISVIEW_H
#define HISTAXISVIEW_H
#include "HistDefs.h"
#include "HistView.h"

class HistAxisView : public HistView {

  private:

	static XtResource _resources[];

  protected:

	Widget _ruler;

    	GC _gc;
	XFontStruct *_fontStruct;
	char *_fontname;
	char *_drawColor;

	static String _defaults[];

	// Geometry
	Dimension _drawOffset, _twoTicks, _fourTicks, _eightTicks;
	Dimension _longTickLength, _shortTickLength;

	static void displayCallback ( Widget, XtPointer, XtPointer);

    	virtual void display(){ }

  public:

    	HistAxisView ( Widget, const char *,
			Histogram *, Histogram *, Histogram *, 
			OrientType );
    	virtual ~HistAxisView ();

	virtual void update ();

    	virtual const char *const className() { return "HistAxisView"; }
};
#endif

