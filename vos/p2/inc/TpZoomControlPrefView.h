//////////////////////////////////////////////////////////////////////////////
// TpZoomControlPrefView - Called when a preferences file is loaded.  
//////////////////////////////////////////////////////////////////////////////

#ifndef TPZOOMCONTROLPREFVIEW_H
#define TPZOOMCONTROLPREFVIEW_H

#include "PrefView.h"
#include "TpZoomControl.h"

class TpZoomControlPrefView : public PrefView {
 
  public:

    TpZoomControlPrefView() { }
    virtual ~TpZoomControlPrefView() { }
    
    virtual int copySize() { return sizeof(TpZoomControl); }
    virtual Boolean modifyCopy() { return True; }
    
    virtual void prefsLoaded(XtPointer object, XtPointer copy)
	{  ((TpZoomControl *)object)->reload((TpZoomControl *)copy); }
    
};

#endif
