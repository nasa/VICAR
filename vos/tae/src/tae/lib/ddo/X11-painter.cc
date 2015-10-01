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
 * X11-dependent painter code
 *      xx-feb-90       Modified for TAE Plus...krw
 *                      compile with special non Interactor version of
 *                      worldrep.h
 *	09-apr-90	More IV_2.6 changes...ljn
 *      02-may-90       merge with motif changes...tpl
 *	15-mar-91	Removed #ifdef MOTIF...ljn
 *	03-apr-91	Header files for VMS...ljn */
// 10-apr-92	Add ctors for Multiple Display support.  Use world that
//		matches display...ga tech, cew
// 06-oct-92	Patch for SGI C++ compiler bug - can't handle the new
//		statement in an inline function...bth/rt
// 19-mar-93	HP compiler does not like some kinds of initialization...rt

#ifdef VMS
#include "btable.h"
#include "inc$interviews:bitmap.h"
#include "inc$interviews:brush.h"
#include "inc$interviews:canvas.h"
#include "inc$interviews:color.h"
#include "inc$interviews:font.h"
#include "inc$interviews:painter.h"
#include "inc$interviews:pattern.h"
#include "inc$interviews:raster.h"
#include "inc$interviews:transformer.h"
#include "painterrep.h"
#include "$taeincxm:worldrep.h" 	//use TAE Plus version
#include <Xlib.h>
#else
#include "btable.h"
#include <InterViews/bitmap.h>
#include <InterViews/brush.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/font.h>
#include <InterViews/painter.h>
#include <InterViews/pattern.h>
#include <InterViews/raster.h>
#include <InterViews/transformer.h>
#include <InterViews/X11/painterrep.h>
#include <wptinc/Xm/worldrep.h> //use TAE Plus version
#include <InterViews/X11/Xlib.h>
#endif

extern WorldRep* FindWorld(Display*); /* dual */
extern Painter* stdpaint; /* dual */

extern void DrawTransformedImage (  /* dual - overloaded new version */
    Display*,  /* dual */
    XImage*, int, int,
    XImage*, int, int,
    Drawable, unsigned int, int, int,
    boolean, unsigned long, unsigned long,
    GC, Transformer*
);


PainterRep::PainterRep() {
    fillgc = XCreateGC(_world->display(), _world->root(), 0, nil);
    dashgc = XCreateGC(_world->display(), _world->root(), 0, nil);
    fillbg = true;
    overwrite = false;
    xor = false;
}

PainterRep::PainterRep(Display* theDisplay) {    /* dual version */
    display = theDisplay;
    WorldRep* _world;

    _world = FindWorld(display);
    fillgc = XCreateGC(_world->display(), _world->root(), 0, nil);
    dashgc = XCreateGC(_world->display(), _world->root(), 0, nil);
    fillbg = true;
    overwrite = false;
    xor = false;
}   /* dual */

PainterRep::~PainterRep() {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */
    XFreeGC(_world->display(), fillgc);
    XFreeGC(_world->display(), dashgc);
}

void PainterRep::PrepareFill(void* info) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */
    if (info == nil) {
        XSetFillStyle(_world->display(), fillgc, FillSolid);
    } else if (fillbg) {
        XSetStipple(_world->display(), fillgc, (Pixmap)info);
        XSetFillStyle(_world->display(), fillgc, FillOpaqueStippled);
    } else {
        XSetStipple(_world->display(), fillgc, (Pixmap)info);
        XSetFillStyle(_world->display(), fillgc, FillStippled);
    }
}

void PainterRep::PrepareDash(int width, void* info, int count) {
    WorldRep* _world = FindWorld(display);   /* dual */
    if (count == 0) {
        XSetLineAttributes(
            _world->display(), dashgc, width,
            LineSolid, CapButt, JoinMiter
        );
        if (info == nil) {
            XSetFillStyle(
                _world->display(), dashgc, FillSolid
            );
        } else if (fillbg) {
            XSetFillStyle(
                _world->display(), dashgc, FillOpaqueStippled
            );
            XSetStipple(_world->display(), dashgc, (Pixmap)info);
        } else {
            XSetFillStyle(
                _world->display(), dashgc, FillStippled
            );
            XSetStipple(_world->display(), dashgc, (Pixmap)info);
        }
    } else {
        XSetFillStyle(_world->display(), dashgc, FillSolid);
        if (info == nil) {
            XSetLineAttributes(
                _world->display(), dashgc, width,
                LineSolid, CapButt, JoinMiter
            );
        } else if (fillbg) {
            XSetLineAttributes(
                _world->display(), dashgc, width,
                LineDoubleDash, CapButt, JoinMiter
            );
            XSetDashes(
                _world->display(), dashgc, 0, (char*)info, count
            );
        } else {
            XSetLineAttributes(
                _world->display(), dashgc, width,
                LineOnOffDash, CapButt, JoinMiter
            );
            XSetDashes(
                _world->display(), dashgc, 0, (char*)info, count
            );
        }
    }
}

/*
 * Short-hand for allocating a vector of X points.
 * The idea is to use a static array if possible; otherwise
 * allocate/deallocate off the heap.
 */

static const XPointListSize = 200;
static XPoint xpoints[XPointListSize];

#ifdef sgi
XPoint* AllocPts(int n) {
    return (n <= XPointListSize) ? xpoints : new XPoint[n];
}
#else
inline XPoint* AllocPts(int n) {
    return (n <= XPointListSize) ? xpoints : new XPoint[n];
}
#endif

inline void FreePts(XPoint* v) {
    if (v != xpoints) {
	delete v;
    }
}

Painter::Painter() {
    rep = new PainterRep;
    Init();
}

Painter::Painter(Painter* copy) {
    display = copy->display;   /* dual */
    WorldRep* _world;   /* dual */
    _world = FindWorld(display);   /* dual */

    rep = new PainterRep(display);   /* dual */
    rep->fillbg = copy->rep->fillbg;
    rep->overwrite = copy->rep->overwrite;
    Copy(copy);
    if (copy->rep->xor) {
	Begin_xor();
    }
    rep->xor = copy->rep->xor;
    if (rep->overwrite) {
	XSetSubwindowMode(_world->display(), rep->fillgc, IncludeInferiors);
	XSetSubwindowMode(_world->display(), rep->dashgc, IncludeInferiors);
    }
}

Painter::Painter (Display* theDisplay) {    /* dual version */
    display = theDisplay;
    rep = new PainterRep(theDisplay);

    WorldRep* _world;

    _world = FindWorld(display);

    black = _world->black();
    white = _world->white();
    stdpaint = _world->stdpaint();
    stdfont = _world->stdfont();
    Init(display);
}   /* dual */

void Painter::Init (Display* theDisplay) {    /* dual version */
    WorldRep* _world;

    _world = FindWorld(theDisplay);

    if (_world->solid() == nil) {
        _world->SetSolid( new Pattern(display, 0xffff));
        _world->SetClear( new Pattern(display, 0));
        _world->SetLightgray( new Pattern(display, 0x8020));
        _world->SetGray( new Pattern(display, 0xa5a5));
        _world->SetDarkgray( new Pattern(display, 0xfafa));
        _world->SetSingle( new Brush(display, 0xffff, 0));
    }
    foreground = nil;
    background = nil;
    pattern = nil;
    br = nil;
    font = nil;
    style = 0;
    matrix = nil;
    SetColors(_world->black(), _world->white());
    SetPattern(_world->solid());
    FillBg(true);
    SetBrush(_world->single());
    SetFont(_world->stdfont());
    SetStyle(Plain);
    SetOrigin(0, 0);
    MoveTo(0, 0);
}  /* dual */

Painter::~Painter() {
    Unref(matrix);
    Unref(font);
    Unref(br);
    Unref(foreground);
    Unref(background);
    Unref(pattern);
    delete rep;
}

void Painter::FillBg(boolean b) {
    if (rep->fillbg != b) {
        if (rep->xor) {
            End_xor();
        }
        rep->fillbg = b;
        if (pattern != nil) {
            rep->PrepareFill(pattern->info);
        }
        if (br != nil) {
            rep->PrepareDash(br->width, br->rep->info, br->rep->count);
        }
    }
}

boolean Painter::BgFilled() {
    return rep->fillbg;
}

void Painter::SetColors(Color* f, Color* b) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    if (rep->xor) {
	End_xor();
    }
    if (f != nil && foreground != f) {
        Unref(foreground);
	foreground = f;
        foreground->Reference();
        XSetForeground(
            _world->display(), rep->fillgc, foreground->PixelValue()
        );
        XSetForeground(
            _world->display(), rep->dashgc, foreground->PixelValue()
        );
    }
    if (b != nil && background != b) {
        Unref(background);
	background = b;
        background->Reference();
	XSetBackground(
            _world->display(), rep->fillgc, background->PixelValue()
        );
	XSetBackground(
            _world->display(), rep->dashgc, background->PixelValue()
        );
    }
}

void Painter::SetPattern(Pattern* pat) {
    if (rep->xor) {
	End_xor();
    }
    if (pattern != pat) {
        Unref(pattern);
	pattern = pat;
        if (pattern != nil) {
            pattern->Reference();
            rep->PrepareFill(pattern->info);
        }
    }
}

void Painter::SetBrush(Brush* b) {
    if (rep->xor) {
	End_xor();
    }
    if (br != b) {
        Unref(br);
	br = b;
	if (br != nil) {
            br->Reference();
            rep->PrepareDash(br->width, br->rep->info, br->rep->count);
	}
    }
}

void Painter::SetFont(Font* f) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    if (font != f) {
        Unref(font);
        font = f;
        if (font != nil) {
            font->Reference();
            XSetFont(_world->display(), rep->fillgc, (XFont)font->rep->id);
        }
    }
}

void Painter::Clip(
    Canvas* c, Coord left, Coord bottom, Coord right, Coord top
) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    XRectangle r[1];
    Coord x, y;

    if (left > right) {
	x = right; r[0].width = left - right + 1;
    } else {
	x = left; r[0].width = right - left + 1;
    }
    if (bottom > top) {
	y = bottom; r[0].height = bottom - top + 1;
    } else {
	y = top; r[0].height = top - bottom + 1;
    }
    r[0].x = x;
    r[0].y = c->height - 1 - y;
    if (r[0].x == 0 && r[0].y == 0 &&
	r[0].width == c->width && r[0].height == c->height
    ) {
	/* clipping to entire canvas is equivalent to no clipping at all */
	XSetClipMask(_world->display(), rep->fillgc, None);
	XSetClipMask(_world->display(), rep->dashgc, None);
    } else {
	XSetClipRectangles(
            _world->display(), rep->fillgc, 0, 0, r, 1, Unsorted
        );
	XSetClipRectangles(
            _world->display(), rep->dashgc, 0, 0, r, 1, Unsorted
        );
    }
}

void Painter::NoClip() {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    XSetClipMask(_world->display(), rep->fillgc, None);
    XSetClipMask(_world->display(), rep->dashgc, None);
}

void Painter::SetOverwrite(boolean children) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    if (rep->overwrite != children) {
	rep->overwrite = children;
	XSetSubwindowMode(
	    _world->display(), rep->fillgc,
            children ? IncludeInferiors : ClipByChildren
	);
	XSetSubwindowMode(
	    _world->display(), rep->dashgc,
            children ? IncludeInferiors : ClipByChildren
	);
    }
}

void Painter::SetPlaneMask(int m) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    XSetPlaneMask(_world->display(), rep->fillgc, m);
    XSetPlaneMask(_world->display(), rep->dashgc, m);
}

void Painter::Map(Canvas* c, Coord x, Coord y, Coord& mx, Coord& my) {
    if (matrix == nil) {
	mx = x; my = y;
    } else {
	matrix->Transform(x, y, mx, my);
    }
    mx += xoff;
    my = c->height - 1 - (my + yoff);
}

void Painter::MapList(
    Canvas* c, Coord x[], Coord y[], int n, Coord mx[], Coord my[]
) {
    register Coord* xp, * yp, * mxp, * myp;
    Coord* lim;

    xp = x; yp = y;
    mxp = mx; myp = my;
    lim = &x[n];
    if (matrix == nil) {
	for (; xp < lim; xp++, yp++, mxp++, myp++) {
	    *mxp = *xp + xoff;
	    *myp = c->height - 1 - (*yp + yoff);
	}
    } else {
	for (; xp < lim; xp++, yp++, mxp++, myp++) {
	    matrix->Transform(*xp, *yp, *mxp, *myp);
	    *mxp += xoff;
	    *myp = c->height - 1 - (*myp + yoff);
	}
    }
}

void Painter::MapList(
    Canvas* c, float x[], float y[], int n, Coord mx[], Coord my[]
) {
    register float* xp, * yp;
    register Coord* mxp, * myp;
    float tmpx, tmpy, * lim;

    xp = x; yp = y;
    mxp = mx; myp = my;
    lim = &x[n];
    if (matrix == nil) {
	for (; xp < lim; xp++, yp++, mxp++, myp++) {
	    *mxp = round(*xp + xoff);
	    *myp = round(c->height - 1 - (*yp + yoff));
	}
    } else {
	for (; xp < lim; xp++, yp++, mxp++, myp++) {
	    matrix->Transform(*xp, *yp, tmpx, tmpy);
	    *mxp = round(tmpx + xoff);
	    *myp = round(c->height - 1 - (tmpy + yoff));
	}
    }
}

void Painter::Begin_xor() {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    if (!rep->xor) {
	rep->xor = true;
	XSetFunction(_world->display(), rep->fillgc, GXxor);
	XSetForeground(_world->display(), rep->fillgc, _world->xor());
	XSetFillStyle(_world->display(), rep->fillgc, FillSolid);
	XSetFunction(_world->display(), rep->dashgc, GXxor);
	XSetForeground(_world->display(), rep->dashgc, _world->xor());
	XSetFillStyle(_world->display(), rep->dashgc, FillSolid);
    }
}

void Painter::End_xor() {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    if (rep->xor) {
	rep->xor = false;
	XSetFunction(_world->display(), rep->fillgc, GXcopy);
	XSetForeground(
            _world->display(), rep->fillgc, foreground->PixelValue()
        );
        if (pattern != nil) {
            rep->PrepareFill(pattern->info);
        }
	XSetFunction(_world->display(), rep->dashgc, GXcopy);
	XSetForeground(
            _world->display(), rep->dashgc, foreground->PixelValue()
        );
        if (br != nil) {
            rep->PrepareDash(br->width, br->rep->info, br->rep->count);
        }
    }
}

inline char _txkey (int i) {
    if (i >= 0) {
        return (
            i < 32 ? i
            : i < 160 ? (24 + (i>>2))
            : i < 672 ? (54 + (i>>4))
            : 127
        );
    } else {
        return (
            i > -32 ? i
            : i > -160 ? (-24 - (i>>2))
            : i > -672 ? (-54 - (i>>4))
            : -127
        );
    }
}

static int TxKey(Transformer* t, int x, int y) {
    if (t == nil) {
        return 0;
    } else {
        Coord x1, y1, x2, y2, x3, y3;
        t->Transform(0, 0, x1, y1);
        t->Transform(0, y, x2, y2);
        t->Transform(x, 0, x3, y3);
        return (
              (_txkey(x2 - x1) << 24)
            + (_txkey(y2 - y1 - y) << 16)
            + (_txkey(x3 - x1 - x) << 8)
            + (_txkey(y3 - y1))
        );
    }
}

void Painter::Stencil(Canvas* c, Coord x, Coord y, Bitmap* d, Bitmap* m) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    if (rep->xor) {
        End_xor();
    }
    int tx = TxKey(matrix, d->Width(), d->Height());
    if (tx == 0) {
        Coord dx, dy;
        Map(c, x + d->Left(), y + d->Top(), dx, dy);
        if (m == nil) {
            XCopyPlane(
                _world->display(), (Pixmap)d->Map(), (Drawable)c->Id(),
                rep->fillgc, 0, 0, d->Width(), d->Height(), dx, dy, 1
            );
        } else if (m == d) {
            XSetForeground(_world->display(), rep->fillgc, 0);
            XSetBackground(_world->display(), rep->fillgc, AllPlanes);
            XSetFunction(_world->display(), rep->fillgc, GXand);
            XCopyPlane(
                _world->display(), (Pixmap)d->Map(), (Drawable)c->Id(),
                rep->fillgc, 0, 0, d->Width(), d->Height(), dx, dy, 1
            );
            XSetForeground(
                _world->display(), rep->fillgc, foreground->PixelValue()
            );
            XSetBackground(_world->display(), rep->fillgc, 0);
            XSetFunction(_world->display(), rep->fillgc, GXxor);
            XCopyPlane(
                _world->display(), (Pixmap)d->Map(), (Drawable)c->Id(),
                rep->fillgc, 0, 0, d->Width(), d->Height(), dx, dy, 1
            );
            XSetBackground(
                _world->display(), rep->fillgc, background->PixelValue()
            );
            XSetFunction(_world->display(), rep->fillgc, GXcopy);
        } else {
            GC gc = XCreateGC(_world->display(), _world->root(), 0, nil);
            XSetForeground(_world->display(), gc, foreground->PixelValue());
            XSetBackground(_world->display(), gc, background->PixelValue());
            XSetGraphicsExposures(_world->display(), gc, False);
            Coord mx, my;
            Map(c, x + m->Left(), y + m->Top(), mx, my);
            XSetClipOrigin(_world->display(), gc, mx, my);
            XSetClipMask(_world->display(), gc, (Pixmap)m->Map());
            XCopyPlane(
                _world->display(), (Pixmap)d->Map(), (Drawable)c->Id(),
                gc, 0, 0, d->Width(), d->Height(), dx, dy, 1
            );
            XFreeGC(_world->display(), gc);
        }
    } else {
        if (m != nil) {
            DrawTransformedImage(
                display,   
                (XImage*)d->rep->GetData(), x + d->Left(), y + d->Bottom(),
                (XImage*)m->rep->GetData(), x + m->Left(), y + m->Bottom(),
                (Drawable)c->Id(), c->height, -xoff, -yoff,
                true, foreground->PixelValue(), background->PixelValue(),
                rep->fillgc, matrix
            );
        } else {
            DrawTransformedImage(
                display,   /* dual */
                (XImage*)d->rep->GetData(), x + d->Left(), y + d->Bottom(),
                nil, 0, 0,
                (Drawable)c->Id(), c->height, -xoff, -yoff,
                true, foreground->PixelValue(), background->PixelValue(),
                rep->fillgc, matrix
            );
        }
    }
}

void Painter::RasterRect(Canvas* c, Coord x, Coord y, Raster* r) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    int tx = TxKey(matrix, r->Width(), r->Height());
    if (tx == 0) {
        Coord mx, my;
        Map(c, x, y + r->Height() - 1, mx, my);
        XPutImage(
            _world->display(), (Drawable)c->id, rep->fillgc,
            (XImage*)r->rep->GetData(),
            0, 0, mx, my, r->Width(), r->Height()
        );
    } else {
        DrawTransformedImage(
            display,   /* dual */
            (XImage*)r->rep->GetData(), x, y,
            nil, 0, 0,
            (Drawable)c->Id(), c->Height(), -xoff, -yoff,
            false, foreground->PixelValue(), background->PixelValue(),
            rep->fillgc, matrix
        );
    }
}

static Bitmap* GetCharBitmap(
    Font* f, int c, int k, Transformer* t
) {
    if (_world->_btable == nil) {
        _world->_btable = new BitmapTable(256);
        _world->_txtable = new BitmapTable(1024);
    }
    Bitmap* basic;
    XFont fid = ((XFontStruct*)f->Info())->fid;
    if (!_world->_btable->Find(basic, fid, c)) {
        basic = new Bitmap(f, c);
	basic->Reference();
        _world->_btable->Insert(fid, c, basic);
    }
    Bitmap* tx;
    Pixmap mapid = Pixmap(basic->Map());
    if (!_world->_txtable->Find(tx, mapid, k)) {
        tx = new Bitmap(basic);
	tx->Reference();
        tx->Transform(t);
        _world->_txtable->Insert(mapid, k, tx);
    }
    return tx;
}

void Painter::Text(Canvas* c, const char* s, int len, Coord x, Coord y) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    Coord x0, y0;
    Coord ybase = y + font->Baseline();
    Coord ytop = y + font->Height() - 1;
    int txstring = TxKey(matrix, font->Width(s, len), font->Height());

    if (style & Reversed) {
        SetColors(GetBgColor(), GetFgColor());
    }
    if (txstring == 0) {
        Map(c, x, ybase, x0, y0);
        if (rep->fillbg) {
            XDrawImageString(
                _world->display(), (Drawable)c->id,
                rep->fillgc, x0, y0, s, len
            );
        } else {
            XDrawString(
                _world->display(), (Drawable)c->id,
                rep->fillgc, x0, y0, s, len
            );
        }
        if (style & Boldface) {
            XDrawString(
                _world->display(), (Drawable)c->id,
                rep->fillgc, x0-1, y0, s, len
            );
        }
    } else {
        Coord curx = x;
        float fx0, fy0;
        Transformer notrans(matrix);
        notrans.Transform(0.0, 0.0, fx0, fy0);
        notrans.Translate(-fx0, -fy0);
        int txchar = TxKey(matrix, font->Width("M"), font->Height());
        Bitmap* bits;
        for (int i = 0; i < len; ++i) {
            Coord nextx = curx + font->Width(s+i, 1);
            if (rep->fillbg) {
                ClearRect(c, curx, y, nextx, ytop);
            }
            switch (_world->txfonts()) {
            case TxFontsOff:
                Map(c, curx, ybase, x0, y0);
                XDrawString(
                    _world->display(), (Drawable)c->id,
                    rep->fillgc, x0, y0, s+i, 1
                );
                if (style & Boldface) {
                    XDrawString(
                        _world->display(), (Drawable)c->id,
                        rep->fillgc, x0-1, y0, s+i, 1
                    );
                }
                break;
            case TxFontsOn:
                bits = new Bitmap(font, s[i]);
                Stencil(c, curx, ybase, bits, bits);
                if (style & Boldface) {
                    Stencil(c, curx-1, ybase, bits, bits);
                }
                break;
            case TxFontsCache:
            case TxFontsDefault:
                bits = GetCharBitmap(font, s[i], txchar, &notrans);
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
		Transformer* oldmatrix;
		oldmatrix = matrix;
#else
	      {
                Transformer* oldmatrix = matrix;
#endif
                matrix = nil;
                oldmatrix->Transform(curx, ybase, x0, y0);
                Stencil(c, x0, y0, bits, bits);
                if (style & Boldface) {
                    oldmatrix->Transform(curx+1, ybase, x0, y0);
                    Stencil(c, x0, y0, bits, bits);
                }
                matrix = oldmatrix;
                break;
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
#else
	      }
#endif
            }
            curx = nextx;
        }
    }
    if (style & Outlined) {
        /* unimplemented */
    }
    if (style & Underlined) {
        Line(c, x, ybase, x + font->Width(s, len) - 1, ybase);
    }
    if (style & Reversed) {
        SetColors(GetBgColor(), GetFgColor());
    }
}

void Painter::Point(Canvas* c, Coord x, Coord y) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    Coord mx, my;

    Map(c, x, y, mx, my);
    XDrawPoint(_world->display(), (Drawable)c->id, rep->fillgc, mx, my);
}

void Painter::MultiPoint(Canvas* c, Coord x[], Coord y[], int n) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    register XPoint* v;
    register int i;

    v = AllocPts(n);
    for (i = 0; i < n; i++) {
	Map(c, x[i], y[i], v[i].x, v[i].y);
    }
    XDrawPoints(
        _world->display(), (Drawable)c->id,
        rep->fillgc, v, n, CoordModeOrigin
    );
    FreePts(v);
}

void Painter::Line(Canvas* c, Coord x1, Coord y1, Coord x2, Coord y2) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    Coord mx1, my1, mx2, my2;

    Map(c, x1, y1, mx1, my1);
    Map(c, x2, y2, mx2, my2);
    XDrawLine(
        _world->display(), (Drawable)c->id, rep->dashgc, mx1, my1, mx2, my2
    );
}

void Painter::Rect(Canvas* c, Coord x1, Coord y1, Coord x2, Coord y2) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    if (matrix != nil && matrix->Rotated() && !matrix->Rotated90()) {
	Coord x[4], y[4];

	x[0] = x[3] = x1;
	x[1] = x[2] = x2;
	y[0] = y[1] = y1;
	y[2] = y[3] = y2;
	Polygon(c, x, y, 4);
    } else {
	Coord left, bottom, right, top, tmp;
	int w, h;

	Map(c, x1, y1, left, bottom);
	Map(c, x2, y2, right, top);
	if (left > right) {
	    tmp = left; left = right; right = tmp;
	}
	if (top > bottom) {
	    tmp = bottom; bottom = top; top = tmp;
	}
	w = right - left;
	h = bottom - top;
	XDrawRectangle(
            _world->display(), (Drawable)c->id, rep->dashgc, left, top, w, h
        );
    }
}

void Painter::FillRect(Canvas* c, Coord x1, Coord y1, Coord x2, Coord y2) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    if (matrix != nil && matrix->Rotated() && !matrix->Rotated90()) {
	Coord x[4], y[4];

	x[0] = x[3] = x1;
	x[1] = x[2] = x2;
	y[0] = y[1] = y1;
	y[2] = y[3] = y2;
	FillPolygon(c, x, y, 4);
    } else {
	Coord left, bottom, right, top, tmp;
	int w, h;

	Map(c, x1, y1, left, bottom);
	Map(c, x2, y2, right, top);
	if (left > right) {
	    tmp = left; left = right; right = tmp;
	}
	if (top > bottom) {
	    tmp = bottom; bottom = top; top = tmp;
	}
	w = right - left + 1;
	h = bottom - top + 1;
	XFillRectangle(
            _world->display(), (Drawable)c->id, rep->fillgc, left, top, w, h
        );
    }
}

void Painter::ClearRect(Canvas* c, Coord x1, Coord y1, Coord x2, Coord y2) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    XSetForeground(_world->display(), rep->fillgc, background->PixelValue());
    Pattern* curpat = pattern;
    SetPattern(_world->solid());   /* dual */
    FillRect(c, x1, y1, x2, y2);
    XSetForeground(_world->display(), rep->fillgc, foreground->PixelValue());
    SetPattern(curpat);
}

void Painter::Circle(Canvas* c, Coord x, Coord y, int r) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    if (matrix != nil && (matrix->Stretched() || matrix->Rotated())) {
	Ellipse(c, x, y, r, r);
    } else {
	Coord left, top, right, bottom;

	Map(c, x-r, y+r, left, top);
        Map(c, x+r, y-r, right, bottom);
	XDrawArc(
	    _world->display(), (Drawable)c->id, rep->dashgc,
            left, top, right-left, bottom-top, 0, 360*64
	);
    }
}

void Painter::FillCircle(Canvas* c, Coord x, Coord y, int r) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    if (matrix != nil && (matrix->Stretched() || matrix->Rotated())) {
	FillEllipse(c, x, y, r, r);
    } else {
	Coord left, top, right, bottom;

	Map(c, x-r, y+r, left, top);
        Map(c, x+r, y-r, right, bottom);
	XFillArc(
	    _world->display(), (Drawable)c->id, rep->fillgc, 
            left, top, right-left, bottom-top, 0, 360*64
	);
    }
}

void Painter::MultiLine(Canvas* c, Coord x[], Coord y[], int n) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    register XPoint* v;
    register int i;

    v = AllocPts(n);
    for (i = 0; i < n; i++) {
	Map(c, x[i], y[i], v[i].x, v[i].y);
    }
    XDrawLines(
        _world->display(), (Drawable)c->id,
        rep->dashgc, v, n, CoordModeOrigin
    );
    FreePts(v);
}

void Painter::MultiLineNoMap(Canvas* c, Coord x[], Coord y[], int n) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    register XPoint* v;
    register int i;

    v = AllocPts(n);
    for (i = 0; i < n; i++) {
	v[i].x = x[i];
	v[i].y = y[i];
    }
    XDrawLines(
        _world->display(), (Drawable)c->id,
        rep->dashgc, v, n, CoordModeOrigin
    );
    FreePts(v);
}

void Painter::Polygon(Canvas* c, Coord x[], Coord y[], int n) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    register XPoint* v;
    register int i;

    v = AllocPts(n+1);
    for (i = 0; i < n; i++) {
	Map(c, x[i], y[i], v[i].x, v[i].y);
    }
    if (x[i-1] != x[0] || y[i-1] != y[0]) {
	v[i] = v[0];
	++i;
    }
    XDrawLines(
        _world->display(), (Drawable)c->id,
        rep->dashgc, v, i, CoordModeOrigin
    );
    FreePts(v);
}

void Painter::FillPolygonNoMap(Canvas* c, Coord x[], Coord y[], int n) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    register XPoint* v;
    register int i;

    v = AllocPts(n);
    for (i = 0; i < n; i++) {
	v[i].x = x[i];
	v[i].y = y[i];
    }
    XFillPolygon(
	_world->display(), (Drawable)c->id, rep->fillgc,
        v, n, Complex, CoordModeOrigin
    );
    FreePts(v);
}

void Painter::FillPolygon(Canvas* c, Coord x[], Coord y[], int n) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    register XPoint* v;
    register int i;

    v = AllocPts(n+1);
    for (i = 0; i < n; i++) {
	Map(c, x[i], y[i], v[i].x, v[i].y);
    }
    XFillPolygon(
	_world->display(), (Drawable)c->id, rep->fillgc,
        v, n, Complex, CoordModeOrigin
    );
    FreePts(v);
}

void Painter::Copy(
    Canvas* src, Coord x1, Coord y1, Coord x2, Coord y2,
    Canvas* dst, Coord x0, Coord y0
) {
    WorldRep* _world;   /* dual */

    _world = FindWorld(display);   /* dual */

    Coord sx1, sy1, sx2, sy2, sx3, sy3, sx4, sy4, dx1, dy1;
    Transformer t(matrix);

    t.Transform(x1, y1, sx1, sy1);
    t.Transform(x1, y2, sx2, sy2);
    t.Transform(x2, y2, sx3, sy3);
    t.Transform(x2, y1, sx4, sy4);
    t.Transform(x0, y0, dx1, dy1);

    int minx = min(sx1, min(sx2, min(sx3, sx4)));
    int maxx = max(sx1, max(sx2, max(sx3, sx4)));
    int miny = min(sy1, min(sy2, min(sy3, sy4)));
    int maxy = max(sy1, max(sy2, max(sy3, sy4)));

    int w = maxx - minx + 1;
    int h = maxy - miny + 1;
    int sx = minx + xoff;
    int sy = src->height - 1 - (maxy + yoff);
    int dx = dx1 - (sx1 - minx) + xoff;
    int dy = dst->height - 1 - (dy1 - (sy1 - maxy) + yoff);

    if ((sx1 == sx2 || sy1 == sy2) && (sx1 == sx4 || sy1 == sy4)) {
        if (src->status == CanvasOffscreen) {
            XSetGraphicsExposures(_world->display(), rep->fillgc, False);
            XCopyArea(
                _world->display(), (Drawable)src->id, (Drawable)dst->id,
                rep->fillgc, sx, sy, w, h, dx, dy
            );
            XSetGraphicsExposures(_world->display(), rep->fillgc, True);
        } else {
            XCopyArea(
                _world->display(), (Drawable)src->id, (Drawable)dst->id,
                rep->fillgc, sx, sy, w, h, dx, dy
            );
            dst->WaitForCopy();
        }
    } else {
        GC copygc = XCreateGC(_world->display(), (Drawable)dst->id, 0, nil);
        Pixmap mask;
        mask = XCreatePixmap(_world->display(), _world->root(), w, h, 1);
        GC maskgc = XCreateGC(_world->display(), mask, 0, nil);
        XSetForeground(_world->display(), maskgc, 0);
        XFillRectangle(_world->display(), mask, maskgc, 0, 0, w, h);
        XSetForeground(_world->display(), maskgc, 1);
        XPoint v[4];
        v[0].x = sx1 - minx; v[0].y = maxy - sy1;
        v[1].x = sx2 - minx; v[1].y = maxy - sy2;
        v[2].x = sx3 - minx; v[2].y = maxy - sy3;
        v[3].x = sx4 - minx; v[3].y = maxy - sy4;
        XFillPolygon(
            _world->display(), mask, maskgc,
            v, 4, Convex, CoordModeOrigin
        );
        XFreeGC(_world->display(), maskgc);
        XSetClipOrigin(_world->display(), copygc, dx, dy);
        XSetClipMask(_world->display(), copygc, mask);
        if (src->status == CanvasOffscreen) {
            XSetGraphicsExposures(_world->display(), copygc, False);
            XCopyArea(
                _world->display(), (Drawable)src->id, (Drawable)dst->id,
                copygc, sx, sy, w, h, dx, dy
            );
            XSetGraphicsExposures(_world->display(), copygc, True);
        } else {
            XCopyArea(
                _world->display(), (Drawable)src->id, (Drawable)dst->id,
                copygc, sx, sy, w, h, dx, dy
            );
            dst->WaitForCopy();
        }
        XFreePixmap(_world->display(), mask);
        XFreeGC(_world->display(), copygc);
    }
}

void Painter::Read(
    Canvas* c, void* dst, Coord x1, Coord y1, Coord x2, Coord y2
) {
    /* unimplemented -- use Raster::Raster(Canvas*, ...) */
}

void Painter::Write(
    Canvas* c, const void* src, Coord x1, Coord y1, Coord x2, Coord y2
) {
    /* unimplemented -- use Painter::RasterRect(Canvas*, ...) */
}
