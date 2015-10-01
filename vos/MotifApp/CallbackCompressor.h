//////////////////////////////////////////////////////////////
// CallbackCompressor.h: A callback function that will "compress"
// callback events by setting a short timer to see if any more
// similar ones are coming.  This is useful for mouse motion
// compression, when the actual X events are interleaved with other
// events (standard Xt motion compression only works on motion events
// with nothing in between).
//
// Since the call_data structure is generally deallocated as soon as
// the callback returns, we must copy it here.  The size of the structure
// for the particular callback being used must be passed in.  In addition,
// the event itself may be copied, if the event is the second item in
// the structure (standard Motif setup: int reason; XEvent *event; ).
//
// Usage:
//	cb = new CallbackCompressor(...);
//	XtAddCallback(widget, callback_name,
//		&CallbackCompressor::callback, (XtPointer)cb);
// If the callback is removed, the cb object can be deleted.  Note that
// deleting the object may cause a pending callback to be ignored.
// flushCallback() can be called to force out any pending callbacks.
// Only one callback should be attached to any CallbackCompressor object.
/////////////////////////////////////////////////////////////

#ifndef CALLBACKCOMPRESSOR_H
#define CALLBACKCOMPRESSOR_H

#include <Xm/Xm.h>

static const unsigned long timer_default = 5;

class CallbackCompressor {

  private:

    static void timeExpiredTimer(XtPointer, XtIntervalId *);

  protected:

    Widget _widget;
    void *_call_data;
    void *_client_data;

    XtCallbackProc _real_callback;

    int _call_data_size;
    Boolean _copy_event;
    unsigned long _timer_value;

    Boolean _timer_active;
    Boolean _pending_callback;
    XtIntervalId _timer_id;

    virtual void callbackInternal(Widget, XtPointer call_data);
    virtual void timeExpired();

    virtual void freeCallData();
    virtual void copyCallData(XtPointer);

  public:

    CallbackCompressor(XtCallbackProc, void *client_data,
		int call_data_size, Boolean copy_event,
		unsigned long timer=timer_default);

    virtual ~CallbackCompressor();

    static void callback(Widget, XtPointer, XtPointer);
    void flushCallback();

};
#endif

