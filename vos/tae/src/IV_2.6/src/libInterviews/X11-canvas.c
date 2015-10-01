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
 * X11-dependent canvas code
 */
/*
 * Change Log:
 * 19-mar-93	HP compiler does not like some kinds of initialization...rt
 */

#include "itable.h"
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/world.h>
#include <InterViews/X11/worldrep.h>
#include <InterViews/X11/Xlib.h>

class CanvasRep {
    friend class Canvas;

    /* nothing needed for X11 */
    int unused;
};

Canvas::Canvas (void* c) {
    id = c;
    width = 0;
    height = 0;
    status = CanvasUnmapped;
}

Canvas::Canvas (int w, int h) {
    id = (void*)XCreatePixmap(
	_world->display(), _world->root(),
        w, h, DefaultDepth(_world->display(), _world->screen())
    );
    width = w;
    height = h;
    status = CanvasOffscreen;
}

Canvas::~Canvas () {
    if (id != nil) {
	if (status == CanvasOffscreen) {
	    XFreePixmap(_world->display(), (Pixmap)id);
	} else {
	    XDestroyWindow(_world->display(), (Window)id);
	    _world->itable()->Remove(id);
	}
	id = nil;
    }
}

void Canvas::WaitForCopy () {
    XEvent xe;
    Interactor* i;

    for (;;) {
	XWindowEvent(_world->display(), (Window)id, ExposureMask, &xe);
	switch (xe.type) {
	    case NoExpose:
		return;
	    case Expose:
		if (_world->itable()->Find(i, (void*)xe.xexpose.window)) {
		    i->Redraw(
			xe.xexpose.x,
			height - xe.xexpose.y - xe.xexpose.height,
			xe.xexpose.x + xe.xexpose.width - 1,
			height - 1 - xe.xexpose.y
		    );
		}
		break;
	    case GraphicsExpose:
        {
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
		XGraphicsExposeEvent* g;
		g = &xe.xgraphicsexpose;
#else
		XGraphicsExposeEvent* g = &xe.xgraphicsexpose;
#endif
		if (_world->itable()->Find(i, (void*)g->drawable)) {
		    i->Redraw(
			g->x, height - g->y - g->height,
			g->x + g->width - 1, height - 1 - g->y
		    );
		}
		if (g->count == 0) {
		    return;
		}
        }
		break;
	}
    }
}

void Canvas::SetBackground (Color* c) {
    if (status != CanvasOffscreen) {
	XSetWindowBackground(_world->display(), (Window)id, c->PixelValue());
    }
}

void Canvas::Clip (Coord, Coord, Coord, Coord) {
    /* Canvas clipping unimplemented for X11. */
}

void Canvas::NoClip () {
    /* Canvas clipping unimplemented for X11. */
}

void Canvas::ClipOn () {
    /* Canvas clipping unimplemented for X11. */
}

void Canvas::ClipOff () {
    /* Canvas clipping unimplemented for X11. */
}

boolean Canvas::IsClipped () {
    return false;
}

void Canvas::Map (Coord& x, Coord& y) {
    /* nothing to do */
}
