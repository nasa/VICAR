//////////////////////////////////////////////////////////////////////////////
// TpContrastControlPrefView - Called when a preferences file is loaded.  
//////////////////////////////////////////////////////////////////////////////

#ifndef TPCONTRASTCONTROLPREFVIEW_H
#define TPCONTRASTCONTROLPREFVIEW_H

#include "PrefView.h"
#include "TpContrastControl.h"

class TpContrastControlPrefView : public PrefView {
 
  public:

    TpContrastControlPrefView() { }
    virtual ~TpContrastControlPrefView() { }
    
    virtual int copySize() { return sizeof(TpContrastControl); }
    virtual Boolean modifyCopy() { return True; }
    
    virtual void prefsLoaded(XtPointer object, XtPointer copy)
	{  ((TpContrastControl *)object)->reload((TpContrastControl *)copy); }
    
};

#endif
