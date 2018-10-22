//////////////////////////////////////////////////////////////////////////
// SgGraphView.h:  This is a view component that draws graphs.
//////////////////////////////////////////////////////////////////////////
#ifndef SGGRAPHVIEW_H
#define SGGRAPHVIEW_H
#include "UIComponent.h"

class SgGraphView : public UIComponent {

  private:

    static XtResource _resources[];

  protected:

    float *_dataSet[3];
    int _numElements;

    Boolean _singleGraph;
    Boolean _blended;
    Boolean _horizontal;
    Boolean _ascending;

    float _maxUserValue;
    float _maxValue;
    int _spike;
    Boolean _log;
    Boolean _filled;

    Boolean _drawInsideTicks;
    int _insideTickLength, _insideMinorTickLength;
    int _insideTickInterval, _insideMinorTickInterval;

    Dimension _topMargin;
    Dimension _width;
    Dimension _height;

    char *_red, *_green, *_blue;
    char *_yellow, *_cyan, *_magenta;
    char *_white, *_bg;
    char *_insideTickColor;

    GC _gc;
    XColor _colorR, _colorG, _colorB;
    XColor _colorM, _colorC, _colorY, _colorW;

    static String _defaults[];

    static void displayCallback ( Widget, XtPointer, XtPointer);
    virtual void display();

    virtual void displayOneFilled();
    virtual void displayOneEmpty();
    virtual void displayThreeStacked();
    virtual void displayThreeBlended();
    virtual void displayThreeEmpty();

    void allocateColors();
    void setDrawingColor ( XColor color );
    static float spike ( float *ds, int size, int spikeCount );
    static float midOfThree ( float, float, float );
    float arrayMax ( float *, int );
    float maxSpikeValue();

    virtual void drawInsideTicks(double maxCountValue, 
				double ppb, int nbins);

  public:

    SgGraphView ( Widget, const char *, float *, float *, float *, int );
    SgGraphView ( Widget, const char *, float *, int );
    SgGraphView ( Widget, const char * );
    virtual ~SgGraphView();

    void setDataSet ( float *, int numElements );
    void setDataSets ( float *, float *, float *, int numElements );

    void setHorizontal ( Boolean h ) { _horizontal = h; display(); }
    Boolean getHorizontal() { return _horizontal; }

    void setAscending ( Boolean a ) { _ascending = a; display(); }
    Boolean getAscending() { return _ascending; }

    void setBlended ( Boolean b ) { _blended = b; display(); }
    Boolean getBlended() { return _blended; }
    
    void setSpike (int spike);
    int getSpike() { return _spike; }

    void setLogScale ( Boolean log ) { _log = log; display(); }
    Boolean logScaleIsSet() { return _log; }

    virtual const char *const className() { return "SgGraphView"; }
};
#endif
