//////////////////////////////////////////////////////////////////////////
// SiHistGraph.cc:  This is a view component that draws one or three 
// graphs.
//////////////////////////////////////////////////////////////////////////
#include "SiHistGraph.h"
#include "SiHistogram.h"

SiHistGraph::SiHistGraph ( Widget parent, const char *name, 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB )
	: SgGraphView ( parent, name ),
	  SiHistView ( histR, histG, histB )
{
    int i;
    float *r = new float [histR->numBins()];
    for ( i = 0; i < histR->numBins(); i++ )
	r[i] = (float)((*histR)[i]);

    float *g = new float [histG->numBins()];
    for ( i = 0; i < histG->numBins(); i++ )
	g[i] = (float)((*histG)[i]);

    float *b = new float [histB->numBins()];
    for ( i = 0; i < histB->numBins(); i++ )
	b[i] = (float)((*histB)[i]);

    setDataSets ( r, g, b, histR->numBins() );

    delete [] r;
    delete [] g;
    delete [] b;

    _histR->attachView ( this );
    _histG->attachView ( this );
    _histB->attachView ( this );
}

SiHistGraph::SiHistGraph ( Widget parent, const char *name,
                SiHistogram *histR )
        : SgGraphView ( parent, name ),
          SiHistView ( histR, NULL, NULL )
{
    int i;
    float *r = new float [histR->numBins()];
    for ( i = 0; i < histR->numBins(); i++ )
	r[i] = (float)((*histR)[i]);

    setDataSet ( r, histR->numBins() );

    delete [] r;

    _histR->attachView ( this );
}

SiHistGraph::~SiHistGraph()
{
    if ( _histR ) _histR->detachView ( this );
    if ( _histG ) _histG->detachView ( this );
    if ( _histB ) _histB->detachView ( this );
}

void SiHistGraph::update()
{
    int i;

    float *r = new float [_histR->numBins()];
    for ( i = 0; i < _histR->numBins(); i++ )
        r[i] = (float)((*_histR)[i]);

    if ( _histG == NULL ) {
	setDataSet ( r, _histR->numBins() );
	delete [] r;
	display();
	return;
    }
	
    float *g = new float [_histG->numBins()];
    for ( i = 0; i < _histG->numBins(); i++ )
        g[i] = (float)((*_histG)[i]);

    float *b = new float [_histB->numBins()];
    for ( i = 0; i < _histB->numBins(); i++ )
        b[i] = (float)((*_histB)[i]);

    setDataSets ( r, g, b, _histR->numBins() );

    delete [] r;
    delete [] g;
    delete [] b;

    display();
}
