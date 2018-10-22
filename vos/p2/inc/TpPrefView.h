//////////////////////////////////////////////////////////////////////////////
// TpPrefView - Called when a preferences file is loaded.  
//////////////////////////////////////////////////////////////////////////////

#ifndef TPPREFVIEW_H
#define TPPREFVIEW_H

#include "PrefView.h"

template <class T>
class TpPrefView : public PrefView {
 
  public:

    TpPrefView() { }
    virtual ~TpPrefView() { }
    
    virtual int copySize() { return sizeof(T); }
    virtual Boolean modifyCopy() { return True; }
    
    virtual void prefsLoaded(XtPointer object, XtPointer copy)
	{  ((T *)object)->reload((T *)copy); }
    
};

#endif
