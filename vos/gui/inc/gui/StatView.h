//////////////////////////////////////////////////////////////
// StatView.h: A component class to show histogram statistics
/////////////////////////////////////////////////////////////

#ifndef STATVIEW_H
#define STATVIEW_H
#include "HistView.h"

class StatView : public HistView {

  private:

    Widget _frameMean, _frameStDev;   
    Widget _frameMean1, _frameStDev1;
    Widget _frameMean2, _frameStDev2;

    Widget _mean, _stDev;		// Input area
    Widget _mean1, _stDev1;
    Widget _mean2, _stDev2;

    Widget _labelMean, _labelStDev;	// The label
    Widget _labelMean1, _labelStDev1;
    Widget _labelMean2, _labelStDev2;

    char * _red, *_green, *_blue;

  public:

    StatView ( Widget, const char *, Histogram *, Histogram *, Histogram * );
    ~StatView ();

    void setStat ( double, double );
    void setStatColor ( double mR, double mG, double mB, 
			double sdR, double sdG, double sdB );

    virtual void update ();

    virtual const char *const className() { return "StatView"; }
};
#endif

