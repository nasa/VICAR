////////////////////////////////////////////////////////////////////////
// CallbackCompressor.cc: A callback function that will "compress"
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
////////////////////////////////////////////////////////////////////////

#include "CallbackCompressor.h"
#include "Application.h"
#include <string.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

CallbackCompressor::CallbackCompressor(XtCallbackProc proc, void *client_data,
		int call_data_size, Boolean copy_event,
		unsigned long timer)
{
    _widget = NULL;
    _call_data = NULL;

    _client_data = client_data;
    _real_callback = proc;
    _call_data_size = call_data_size;
    _copy_event = copy_event;
    _timer_value = timer;

    _timer_active = False;
    _pending_callback = False;
    _timer_id = None;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

CallbackCompressor::~CallbackCompressor()
{
    if (_timer_active)
        XtRemoveTimeOut(_timer_id);
    freeCallData();
}

////////////////////////////////////////////////////////////////////////
// Callback happened.  Store the current values and set (reset)
// the timer.
////////////////////////////////////////////////////////////////////////

void CallbackCompressor::callback(Widget w, XtPointer client_data,
					XtPointer call_data)
{
    CallbackCompressor *obj = (CallbackCompressor *)client_data;
    if (obj != NULL)
        obj->callbackInternal(w, call_data);
}

void CallbackCompressor::callbackInternal(Widget w, XtPointer call_data)
{
    if (_call_data)			// free old stuff
        freeCallData();
    _call_data = NULL;
    copyCallData(call_data);
    _widget = w;

    _pending_callback = True;

    if (_timer_active)			// reset old timer
        XtRemoveTimeOut(_timer_id);

    _timer_id = XtAppAddTimeOut(theApplication->appContext(), _timer_value,
		&CallbackCompressor::timeExpiredTimer, (XtPointer)this);
    _timer_active = True;
}

////////////////////////////////////////////////////////////////////////
// Timer expired, so send out the callback
////////////////////////////////////////////////////////////////////////

void CallbackCompressor::timeExpiredTimer(XtPointer client_data, XtIntervalId *)
{
    CallbackCompressor *obj = (CallbackCompressor *)client_data;
    if (obj != NULL)
        obj->timeExpired();
}

void CallbackCompressor::timeExpired()
{
    _timer_active = False;		// don't remove timer twice
    flushCallback();
}

////////////////////////////////////////////////////////////////////////
// Flush out the pending callback by calling the application
////////////////////////////////////////////////////////////////////////

void CallbackCompressor::flushCallback()
{
    if (_timer_active)
        XtRemoveTimeOut(_timer_id);
    _timer_active = False;

    if (!_pending_callback)
        return;

    (*_real_callback)(_widget, _client_data, _call_data);
    _pending_callback = False;
}

////////////////////////////////////////////////////////////////////////
// Copy a call_data structure into _call_data according to ctor args.
// Assumes any old values have been cleared out already.  This can be
// overridden by a subclass with unusual call_data requirements.
////////////////////////////////////////////////////////////////////////

void CallbackCompressor::copyCallData(XtPointer call_data)
{
    if (_call_data_size == 0) {		// nothing to copy??
        _call_data = NULL;
        return;
    }

    _call_data = (void *)new char[_call_data_size];
    memcpy(_call_data, call_data, _call_data_size);

    if (_copy_event) {			// copy event structure too
        XmAnyCallbackStruct *cb_src = (XmAnyCallbackStruct *)call_data;
        XmAnyCallbackStruct *cb_dst = (XmAnyCallbackStruct *)_call_data;

        if (cb_src->event) {
            cb_dst->event = (XEvent *) new char[sizeof(XEvent)];
            memcpy(cb_dst->event, cb_src->event, sizeof(XEvent));
        }
        else
            cb_dst->event = NULL;
    }
}

////////////////////////////////////////////////////////////////////////
// Free a call_data structure copied by copyCallData().  This can be
// overridden by a subclass with unusual call_data requirements.
////////////////////////////////////////////////////////////////////////

void CallbackCompressor::freeCallData()
{
    if (_call_data == NULL)
        return;

    if (_copy_event) {
        XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *)_call_data;
        if (cb->event)
            delete (char *)(cb->event);
    }
    delete (char *)_call_data;
}

