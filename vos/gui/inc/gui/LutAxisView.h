//////////////////////////////////////////////////////////////
// LutAxisView.h: A component class to show axis
/////////////////////////////////////////////////////////////
#ifndef LUTAXISVIEW_H
#define LUTAXISVIEW_H
#include "LutView.h"

class LutAxisView : public LutView {

  private:

        static XtResource _resources[];

  protected:

	XmString        xmstr;

	Widget _ruler;

    	GC _gc;
	XFontStruct *_fontStruct;
	char *_fontname;

	static String _defaults[];

        // Geometry
        Dimension _drawOffset, _twoTicks, _fourTicks, _eightTicks;
        Dimension _longTickLength, _shortTickLength;

	static void displayCallback ( Widget, XtPointer, XtPointer);

  public:

    	LutAxisView ( Widget, const char *, Lut *, Lut*, Lut* );
	LutAxisView ( Widget, const char *, Lut *);
    	virtual ~LutAxisView ();

	virtual void update ()=0;

    	virtual const char *const className() { return "LutAxisView"; }
};
#endif

