//////////////////////////////////////////////////////////////////////////
// SiHistGraph.h:  This is a view component that draws graphs.
//////////////////////////////////////////////////////////////////////////
#ifndef SIHISTGRAPH_H
#define SIHISTGRAPH_H
#include "SgGraphView.h"
#include "SiHistView.h"

class SiHistGraph : public SgGraphView, public SiHistView {

  public:

    SiHistGraph ( Widget, const char *,
				SiHistogram *, SiHistogram *, SiHistogram * );
    SiHistGraph ( Widget, const char *, SiHistogram * );
    virtual ~SiHistGraph();

    virtual void update();

    virtual const char *const className() { return "SiHistGraph"; }
};
#endif
