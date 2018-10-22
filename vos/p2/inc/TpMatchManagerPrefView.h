//////////////////////////////////////////////////////////////////////////////
// TpMatchManagerPrefView - Called when a preferences file is loaded.  
//////////////////////////////////////////////////////////////////////////////

#ifndef TPMATCHMANAGERPREFVIEW_H
#define TPMATCHMANAGERPREFVIEW_H

#include "PrefView.h"
#include "TpMatchManager.h"

class TpMatchManagerPrefView : public PrefView {
 
  public:

    TpMatchManagerPrefView() { }
    virtual ~TpMatchManagerPrefView() { }
    
    virtual int copySize() { return sizeof(TpMatchManager); }
    virtual Boolean modifyCopy() { return True; }
    
    virtual void prefsLoaded(XtPointer object, XtPointer copy)
	{  ((TpMatchManager *)object)->reload((TpMatchManager *)copy); }
    
};

#endif
