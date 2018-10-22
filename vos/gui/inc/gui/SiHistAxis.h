//////////////////////////////////////////////////////////////////////////
// SiHistAxis.h:  This is a view component that draws axis.
//////////////////////////////////////////////////////////////////////////
#ifndef SIHISTAXIS_H
#define SIHISTAXIS_H
#include "SgAxisView.h"
#include "SiHistView.h"

class SiHistAxis : public SgAxisView, public SiHistView {

  public:

    SiHistAxis ( Widget, const char *,
				SiHistogram *, SiHistogram *, SiHistogram * );
    SiHistAxis ( Widget, const char *, SiHistogram * );
    virtual ~SiHistAxis();

    virtual void update();

    virtual const char *const className() { return "SiHistAxis"; }
};
#endif
