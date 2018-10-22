///////////////////////////////////////////////////////////////////
// SiHistView.h:  View objects that depend on histogram model.
///////////////////////////////////////////////////////////////////
#ifndef SiHistVIEW_H
#define SiHistVIEW_H

class SiHistogram;

class SiHistView {

  protected:

    SiHistogram *_histR, *_histG, *_histB;

    SiHistView ( SiHistogram *r, SiHistogram *g, SiHistogram *b ) 
	{ _histR = r; _histG = g; _histB = b; }
    virtual ~SiHistView () { }

  public:

    virtual void update ()=0;
};
#endif
