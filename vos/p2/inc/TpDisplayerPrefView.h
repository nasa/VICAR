//////////////////////////////////////////////////////////////////////////////
// TpDisplayerPrefView - Called when a preferences file is loaded.  
//////////////////////////////////////////////////////////////////////////////

#ifndef TPDISPLAYERPREFVIEW_H
#define TPDISPLAYERPREFVIEW_H

#include "PrefView.h"
#include "TpDisplayer.h"

class TpDisplayerPrefView : public PrefView {
 
  public:

    TpDisplayerPrefView() { }
    virtual ~TpDisplayerPrefView() { }
    
    virtual int copySize() { return sizeof(TpDisplayer); }
    virtual Boolean modifyCopy() { return True; }
    
    virtual void prefsLoaded(XtPointer object, XtPointer copy)
	{  ((TpDisplayer *)object)->reload((TpDisplayer *)copy); }
    
};

#endif
