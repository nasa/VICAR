/*
 * Copyright (c) 1987, 1988, 1989 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * X11-dependent sensor code
 */
/*
 * Change Log:
 * 19-mar-93    HP compiler does not like some kinds of initialization...rt
 */

#include "itable.h"
#include <InterViews/sensor.h>
#include <InterViews/X11/Xlib.h>
#include <InterViews/X11/eventrep.h>
#include <InterViews/X11/worldrep.h>

Mask motionmask = PointerMotionMask;
Mask keymask = KeyPressMask;
Mask entermask = EnterWindowMask;
Mask leavemask = LeaveWindowMask;
Mask focusmask = FocusChangeMask;
Mask substructmask = SubstructureRedirectMask;
Mask upmask = ButtonPressMask|ButtonReleaseMask|OwnerGrabButtonMask;
Mask downmask = ButtonPressMask|ButtonReleaseMask|OwnerGrabButtonMask;
Mask initmask = PointerMotionHintMask;

static Sensor* grabber;

boolean Sensor::Interesting(Event& e) {
    register XEvent& x = e.Rep()->event();
    switch (x.type) {
	case MotionNotify:
	    e.GetMotionInfo();
	    return true;
	case KeyPress:
	    e.GetKeyInfo();
	    return ButtonIsSet(down, e.button);
	case ButtonPress:
	    e.GetButtonInfo(DownEvent);
        {
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
	    boolean b;
	    b = ButtonIsSet(down, e.button);
#else
	    boolean b = ButtonIsSet(down, e.button);
#endif
	    if (b && ButtonIsSet(up, e.button)) {
		grabber = this;
	    } else {
		grabber = nil;
	    }
	    return b;
        }
	case ButtonRelease:
	    e.GetButtonInfo(UpEvent);
	    return ButtonIsSet(up, e.button) || (grabber != nil);
	case FocusIn:
	    e.eventType = FocusInEvent;
	    return true;
	case FocusOut:
	    e.eventType = FocusOutEvent;
	    return true;
	case EnterNotify:
	    return e.GetCrossingInfo(EnterEvent);
	case LeaveNotify:
	    return e.GetCrossingInfo(LeaveEvent);
	default:
	    /* ignore */;
    }
    return false;
}
