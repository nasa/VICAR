///////////////////////////////////////////////////////////////////
// LutGraphView.h: A component class to show lut graph 
///////////////////////////////////////////////////////////////////
#ifndef LUTGRAPHVIEW_H
#define LUTGRAPHVIEW_H
#include "LutView.h"

class Lut;

class LutGraphView : public LutView {

  private:

	static XtResource _resources[];

	static void displayCallback ( Widget widget, 
				      XtPointer client_data, 
				      XtPointer call_data );

  protected:

	XColor  colorBg, colorFg, colorR, colorG, colorB;

	char * _red, *_green, *_blue;

	GC _gc;
	Dimension _width, _height;

  public:

	LutGraphView ( Widget, const char *, Lut *, Lut *, Lut * );
	LutGraphView ( Widget, const char *, Lut * );
	virtual ~LutGraphView ();

	virtual void update ();

	virtual const char *const className() { return "LutGraphView"; }
};
#endif

