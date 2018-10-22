//////////////////////////////////////////////////////////////////////////
// HistGraphView.h:  This is a view component that draws histogram.
// BW histogram gets drawn in the appropriate color -- red, green, or blue.
// Color histogram is drawn with colors blended (displayBlend method) or
// stacked on top of each other (displayStacked method).
//////////////////////////////////////////////////////////////////////////
#ifndef HISTGRAPHVIEW_H
#define HISTGRAPHVIEW_H
#include "HistDefs.h"
#include "HistView.h"

class HistGraphView : public HistView {

    private:

	static XtResource _resources[];

	static void displayCallback ( Widget widget, 
				      XtPointer client_data, 
				      XtPointer call_data );

    protected:

	int _spike;
	Boolean _log;

	Boolean _drawInsideTicks;
	int _insideTickLength, _insideMinorTickLength;
	int _insideTickInterval, _insideMinorTickInterval;

	char *_red, *_green, *_blue;
	char *_yellow, *_cyan, *_magenta;
	char *_white, *_bg;
	char *_insideTickColor;

	GC _gc;
	Dimension _width, _height;

	virtual void displayBW ( Boolean R, Boolean G, Boolean B );
	virtual void displayStacked();
	virtual void displayBlend();

	virtual void drawInsideTicks(double maxCountValue, 
				double ppb, int nbins);

    public:

	HistGraphView ( Widget, const char *,
			Histogram *, Histogram *, Histogram *, 
			MethodType, OrientType, VerAxisDirType );

	~HistGraphView ();

	void setSpike (int spike) { _spike = spike; }
	int getSpike() { return _spike; }

	void setLogScale(Boolean log) { _log = log; }
	Boolean logScaleIsSet() { return _log; }

	virtual void update ();

	virtual const char *const className() { return "HistGraphView"; }
};
#endif
