////////////////////////////////////////////////////////////////
// CursorModel.h: Calls updates on cursor views such as position 
// view, dn view etc., when something cursor-related happens.
////////////////////////////////////////////////////////////////
#ifndef CURSORMODEL_H
#define CURSORMODEL_H
#include <Xm/Xm.h>		// only for Boolean

class CursorBasicView;

class CursorModel {

    protected:

	int _numViews;
	CursorBasicView  **_views;

	Boolean	_trackingEnabled;
	Widget _iw;

	virtual void updateViews();

    public:
  
	// CONSTRUCTOR/DESTRUCTOR
	CursorModel(Boolean trackingEnabled, Widget iw);
	virtual ~CursorModel();

	// GET IMAGE WIDGET ID
	Widget getImageWidget() { return _iw ; }

      	virtual void attachView(CursorBasicView * view);
        virtual void detachView(CursorBasicView * view);

	// IS TRACKING ENABLED (ON/OFF USER CONTROL TBD?)
	Boolean	isTrackingEnabled() { return _trackingEnabled; }

	// START TRACKING (CALLED BY USER CONTROL)
	void stopTracking() { _trackingEnabled = False; updateViews(); }

	// STOP TRACKING (CALLED BY USER CONTROL )
	void startTracking() { _trackingEnabled = True; updateViews(); }

};
#endif
