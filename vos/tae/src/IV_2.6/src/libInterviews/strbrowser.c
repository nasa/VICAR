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
 * StringBrowser implementation.
 */
/*
 * CHANGE LOG
 *
 * 05-oct-92    Fix C++ anachronisms...rt
 * 19-mar-93	HP compiler does not like some kinds of initialization...rt
 */

#include <InterViews/bitmap.h>
#include <InterViews/button.h>
#include <InterViews/cursor.h>
#include <InterViews/font.h>
#include <InterViews/painter.h>
#include <InterViews/perspective.h>
#include <InterViews/sensor.h>
#include <InterViews/shape.h>
#include <InterViews/strbrowser.h>
#include <InterViews/textdisplay.h>

#include <InterViews/Bitmaps/hand.bm>
#include <InterViews/Bitmaps/handMask.bm>
#include <InterViews/Bitmaps/dfast.bm>
#include <InterViews/Bitmaps/dfastMask.bm>
#include <InterViews/Bitmaps/ufast.bm>
#include <InterViews/Bitmaps/ufastMask.bm>

#include <bstring.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

StringBrowser::StringBrowser (
    ButtonState* bs, int r, int c, boolean u, int h, const char* d
) {
    Init(bs, r, c, u, h, d);
}

StringBrowser::StringBrowser (
    const char* name,
    ButtonState* bs, int r, int c, boolean u, int h, const char* d
) {
    SetInstance(name);
    Init(bs, r, c, u, h, d);
}

void StringBrowser::Init (
    ButtonState* bs, int r, int c, boolean u, int h, const char* d
) {
    const int defaultSize = 256;

    SetClassName("StringBrowser");
    input = new Sensor;
    input->Reference();
    input->Catch(DownEvent);
    input->Catch(KeyEvent);

    strbufsize = selbufsize = defaultSize;
    strbuf = new char*[strbufsize];
    selbuf = new char*[selbufsize];
    strcount = selcount = 0;

    display = nil;
    rows = r;
    columns = c;
    uniqueSel = u;
    singleClick = false;
    highlight = h;
    lastx = lasty = -1;
    subject = bs;
    done = d;
    perspective = new Perspective;
    firstResize = true;
    InitTextDisplay();
}

void StringBrowser::InitTextDisplay () {
    delete display;
    display = new TextDisplay;
    display->CaretStyle(NoCaret);

    for (int i = 0; i < strcount; ++i) {
        display->ReplaceText(i, strbuf[i], strlen(strbuf[i]));
    }
    if (canvas != nil) {
        output->ClearRect(canvas, 0, 0, xmax, ymax);
        firstResize = true;
        Resize();
    }
}

StringBrowser::~StringBrowser () {
    Clear();
    delete strbuf;
    delete display;
    Unref(perspective);
}

static void BufCheck (const char**& buf, int& bufsize, int count, int index) {
    char** newbuf;

    if (index >= bufsize) {
        bufsize = (index+1) * 2;
        newbuf = new char*[bufsize];
        bcopy(buf, newbuf, count*sizeof(char*));
        delete buf;
        buf = (const char**)newbuf;
    }
}

static void BufInsert (
    const char* s, int index, const char**& buf, int& bufsize, int& count
) {
    const char** spot;
    index = (index < 0) ? count : index;

    if (index < count) {
        BufCheck(buf, bufsize, count, count+1);
        spot = &buf[index];
        bcopy(spot, spot+1, (count - index)*sizeof(char*));

    } else {
        BufCheck(buf, bufsize, count, index);
        spot = &buf[index];
    }
    *spot = s;
    ++count;
}

static void BufRemove (int index, const char** buf, int& count) {
    if (index < --count) {
        const char** spot = &buf[index];
        bcopy(spot+1, spot, (count - index)*sizeof(char*));
    }
}

static int BufFind (
    int index, 
    const char** srcbuf, int srccount, 
    const char** dstbuf, int dstcount
) {
    if (0 <= index && index < srccount) {
        const char* s = srcbuf[index];

        if (s != nil) {
            for (int i = 0; i < dstcount; ++i) {
                if (dstbuf[i] == s) {
                    return i;
                }
            }
        }
    }
    return -1;
}

void StringBrowser::Insert (const char* s, int index) {
    register Perspective* p = perspective;

    char* copy = new char[strlen(s)+1];
    strcpy(copy, s);
    BufInsert(copy, index, (const char **)strbuf, strbufsize, strcount);

    p->height += lineheight;
    p->cury += lineheight;
    p->Update();
    if (index < strcount-1) {
        display->InsertLinesAfter(index-1, 1);
    }
    display->ReplaceText(index, s, strlen(s));
}

void StringBrowser::Remove (int index) {
    if (0 <= index && index < strcount) {
        register Perspective* p = perspective;

        Unselect(index);
        BufRemove(index, (const char **)strbuf, strcount);
        delete String(index);
    
        p->height -= lineheight;
        p->cury -= lineheight;
        p->Update();
        display->DeleteLinesAfter(index-1, 1);
    }
}

int StringBrowser::Index (const char* s) {
    for (int i = 0; i < strcount; ++i) {
        if (strcmp(s, strbuf[i]) == 0) {
            return i;
        }
    }
    return -1;
}

char* StringBrowser::String (int index) { 
    return (0 <= index && index < strcount) ? strbuf[index] : nil;
}

void StringBrowser::Clear () {
    for (int i = 0; i < strcount; ++i) {
        delete strbuf[i];
    }
    strcount = selcount = 0;
    InitTextDisplay();
}

void StringBrowser::Select (int index) {
    if (index < strcount && !Selected(index)) {
        BufInsert(String(index), selcount, (const char **)selbuf, selbufsize, selcount);
        display->Style(index, 0, index, columns, highlight);
    }
}

void StringBrowser::Unselect (int index) {
    int selindex;

    if (index < strcount && (selindex = SelectionIndex(index)) >= 0) {
        BufRemove(selindex, (const char **)selbuf, selcount);
        display->Style(index, 0, index, columns, Plain);
    }
}

int StringBrowser::Selection (int selindex) {
    return BufFind(selindex, (const char **)selbuf, selcount,(const char **) strbuf, strcount);
}

int StringBrowser::SelectionIndex (int index) {
    return BufFind(index, (const char **)strbuf, strcount, (const char **)selbuf, selcount);
}

void StringBrowser::Browse () {
    Event e;
    e.target = nil;
    e.eventType = EnterEvent;
    Handle(e);
}

boolean StringBrowser::HandleDownEvent (Event& e) {
    boolean done = true;

    if (e.target == this) {
        if (e.button == LEFTMOUSE) {
            done = LeftButtonDown(e);
        } else if (e.button == MIDDLEMOUSE) {
            GrabScroll(e);
        } else if (e.button == RIGHTMOUSE) {
            RateScroll(e);
        }
    } else {
        UnRead(e);
    }
    return done;
}

boolean StringBrowser::HandleKeyEvent (Event& e) {
    boolean done = false;

    if (e.len != 0) {
        done = HandleChar(e.keystring[0]);
    }
    return done;
}    

void StringBrowser::Handle (Event& e) {
    if (e.eventType == KeyEvent) {
        HandleKeyEvent(e);

    } else {
        boolean done = false;

        do {
            switch (e.eventType) {
            case DownEvent:
                done = HandleDownEvent(e);
                break;

            case KeyEvent:
                done = HandleKeyEvent(e);
                break;
            }
            if (!done) {
                Read(e);
            }
        } while (!done);
    }
}

boolean StringBrowser::HandleChar (char c) {
    int index = Selection();

    switch (c) {
    case SBFirstString:
        ScrollTo(0);
        break;
    case SBLastString:
        ScrollTo(strcount-1);
        break;
    case SBScrollDown:
        ScrollBy(1);
        break;
    case SBScrollUp:
        ScrollBy(-1);
        break;
    case SBSelectAll:
        if (!uniqueSel) {
            SelectAll();
        }
        break;
    case SBUnselectAll:
    case SBUnselectAllAlt:
        UnselectAll();
        break;
    case SBSelectPreviousString:
        Unselect(index);
        index = max(0, min(--index, strcount-1));
        Select(index);
        ScrollTo(index);
        break;
    case SBSelectNextString:
        Unselect(index);
        index = max(0, min(++index, strcount-1));
        Select(index);
        ScrollTo(index);
        break;
    case SBSelectTopString:
        Unselect(index);
        index = Locate(0, ymax);
        Select(index);
        break;
    case SBSelectBottomString:
        Unselect(index);
        index = Locate(0, 0);
        Select(index);
        break;
    case SBPageDown:
        ScrollBy((ymax+1) / lineheight);
        break;
    case SBPageUp:
        ScrollBy(-(ymax+1) / lineheight);
        break;
    case SBHalfPageDown:
        ScrollBy((ymax+1) / lineheight / 2);
        break;
    case SBHalfPageUp:
        ScrollBy(-(ymax+1) / lineheight / 2);
        break;
    default:
	{
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
	int i;
        for (i = 0; done[i] != '\0'; ++i) {
#else
        for (int i = 0; done[i] != '\0'; ++i) {
#endif
            if (c == done[i]) {
                subject->SetValue(c);
                return true;
            }
        }
	}
        break;
    }
    return false;
}

void StringBrowser::Adjust (Perspective& np) {
    register Perspective* p = perspective;
    float scale = float(np.height) / float(p->height);
    ScrollTo(0, p->y0 + int((np.cury - np.y0) / scale));
}

static Cursor* handCursor;
static Cursor* upCursor;
static Cursor* dnCursor;

void StringBrowser::Reconfig () {
    if (handCursor == nil) {
        Bitmap hand(
            hand_bits, hand_width, hand_height, hand_x_hot, hand_y_hot
        );
        Bitmap handmask(hand_mask_bits, hand_mask_width, hand_mask_height);
        Bitmap up(
            ufast_bits, ufast_width, ufast_height, ufast_x_hot, ufast_y_hot
        );
        Bitmap upmask(ufast_mask_bits, ufast_mask_width, ufast_mask_height);
        Bitmap dn(
            dfast_bits, dfast_width, dfast_height, dfast_x_hot, dfast_y_hot
        );
        Bitmap dnmask(dfast_mask_bits, dfast_mask_width, dfast_mask_height);

        handCursor = new Cursor(
            &hand, &handmask, output->GetFgColor(), output->GetBgColor()
        );
        upCursor = new Cursor(
            &up, &upmask, output->GetFgColor(), output->GetBgColor()
        );
        dnCursor = new Cursor(
            &dn, &dnmask, output->GetFgColor(), output->GetBgColor()
        );
    }

    Font* f = output->GetFont();
    shape->hunits = f->Width("n");
    shape->vunits = f->Height();
    lineheight = shape->vunits;
    shape->Rect(shape->hunits*columns, shape->vunits*rows);
    shape->Rigid(hfil, hfil, shape->height - lineheight, vfil);

    const char* attrib = GetAttribute("singleClick");
    singleClick = (attrib != nil && strcmp(attrib, "on") == 0);

    attrib = GetAttribute("clickDelay");
    clickDelay = (attrib == nil) ? 250 : atoi(attrib);
}

void StringBrowser::Resize () {
    register Perspective* p = perspective;

    p->sx = shape->hunits;
    p->sy = lineheight;
    p->lx = xmax+1;
    p->ly = ymax+1;
    p->width = columns * shape->hunits;
    p->height = Count() * lineheight;
    p->curwidth = xmax+1;
    p->curheight = ymax+1;

    if (firstResize) {
        p->curx = 0;
        p->cury = p->height - p->curheight;
        firstResize = false;
    }
    p->Update();
    display->Draw(output, canvas);
    display->LineHeight(lineheight);
    display->Resize(0, -lineheight, xmax, ymax);
}

void StringBrowser::Redraw (Coord l, Coord b, Coord r, Coord t) {
    display->Redraw(l, b, r, t);
}

void StringBrowser::Select (int dot, int mark) {
    for (int i = min(dot, mark); i <= max(dot, mark); ++i) {
        Select(i);
    }
}

void StringBrowser::SelectAll () {
    for (int i = 0; i < strcount; ++i) {
        BufInsert(strbuf[i], selcount, (const char **)selbuf, selbufsize, selcount);
    }
    display->Style(0, 0, strcount, 0, highlight);
}

void StringBrowser::Unselect (int dot, int mark) {
    for (int i = min(dot, mark); i <= max(dot, mark); ++i) {
        Unselect(i);
    }
}

void StringBrowser::UnselectAll () {
    selcount = 0;
    display->Style(0, 0, strcount, 0, Plain);
}

void StringBrowser::ScrollBy (int, int dy) {
    ScrollTo(0, perspective->cury + dy);
}

void StringBrowser::ScrollBy (int lines) {
    ScrollBy(0, -lines*lineheight);
}

void StringBrowser::ScrollTo (int x, int y) {
    register Perspective* p = perspective;
    int maxy = p->height - p->curheight;
    int miny = min(maxy, 1-lineheight);

    p->cury = max(miny, min(y, maxy));
    p->Update();
    int topmargin = p->height - p->curheight - p->cury;
    int line = topmargin / lineheight;
    display->Scroll(line, x, ymax);
}

void StringBrowser::ScrollTo (int index) {
    register Perspective* p = perspective;
    Coord y0 = p->y0 + p->cury;
    Coord y = p->height - (index+1)*lineheight - y0;

    if (y > ymax) {
        ScrollTo(0, y0 - (ymax-y));
    } else if (y < 0) {
        y -= (p->curheight % lineheight == 0) ? 0 : lineheight;
        ScrollTo(0, y0 - (-y));
    }
}

void StringBrowser::ScrollToView (Coord, Coord y) {
    register Perspective* p = perspective;

    if (y > ymax) {
        ScrollTo(0, p->y0 + p->cury - (ymax-y));
    } else if (y < 0) {
        ScrollTo(0, p->y0 + p->cury - (-y));
    }
}

int StringBrowser::Locate (Coord, Coord y) {
    register Perspective* p = perspective;

    y = max(p->curheight % lineheight, min(y, p->curheight-1));
    return display->LineNumber(y);
}

void StringBrowser::Note (Event& e) {
    lasttime = e.timestamp;
    lastx = e.x;
    lasty = e.y;
}

boolean StringBrowser::DoubleClicked (Event& e) {
    if (e.eventType != DownEvent) {
	return false;
    }
    const int distThresh = 4;
    int time = abs(int(e.timestamp - lasttime));
    int dist = abs(e.x - lastx) + abs(e.y - lasty);

    return time < clickDelay && dist < distThresh;
}

void StringBrowser::UpdateSelection (int d, int m, int style) {
    int oldl = min(lastdot, lastmark);
    int oldr = max(lastdot, lastmark);
    int newl = min(d, m);
    int newr = max(d, m);

    if (newr < oldl || newl > oldr) {           // no overlap
        if (style == highlight) {
            Unselect(oldl, oldr);
        }
        if (style == highlight) {
            Select(newl, newr);
        } else {
            Unselect(newl, newr);
        }
    } else {                                    // overlap
        if (newl < oldl) {
            if (style == highlight) {
                Select(newl, oldl);
            } else {
                Unselect(newl, oldl);
            }
        } else if (newl > oldl) {
            if (style == highlight) {
                Unselect(oldl, newl-1);
            }
        }
        if (newr > oldr) {
            if (style == highlight) {
                Select(oldr, newr);
            } else {
                Unselect(oldr, newr);
            }
        } else if (newr < oldr) {
            if (style == highlight) {
                Unselect(newr+1, oldr);
            }
        }
    }
    lastdot = d;
    lastmark = m;
}

boolean StringBrowser::LeftButtonDown (Event& e) {
    boolean status = false;

    if (DoubleClicked(e)) {
        subject->SetValue(done[0]);
        status = true;

    } else if (uniqueSel) {
        if (Selections() == 0) {
            Select(Locate(e.x, e.y));
        } else {
            Unselect(Selection());
            if (!e.shift) {
                Select(Locate(e.x, e.y));
            }
        }

    } else {
        lastdot = lastmark = Locate(e.x, e.y);

        if (Selected(lastdot) && e.shift) {
            Unselect(lastdot);
            do {
                ScrollToView(e.x, e.y);
                UpdateSelection(lastdot, Locate(e.x, e.y), Plain);
                Poll(e);
            } while (e.leftmouse);

        } else {
            if (!e.shift) {
                UnselectAll();
            }
            Select(lastdot);
            do {
                ScrollToView(e.x, e.y);
                UpdateSelection(lastdot, Locate(e.x, e.y), highlight);
                Poll(e);
            } while (e.leftmouse);
        }
    }
    Note(e);
    if (singleClick) {
        subject->SetValue(done[0]);
        status = true;
    }
    return status;
}

void StringBrowser::GrabScroll (Event& e) {
    int y = e.y;
    int x = e.x;
    Cursor* origCursor = GetCursor();
    SetCursor(handCursor);

    do {
        ScrollBy(x - e.x, y - e.y);
        y = e.y;
        x = e.x;
        Poll(e);
    } while (e.middlemouse);

    SetCursor(origCursor);
}

void StringBrowser::RateScroll (Event& e) {
    Cursor* origCursor = GetCursor();
    int y = e.y;
    int x = e.x;

    do {
        ScrollBy(e.x - x, e.y - y);
        if (e.y - y < 0) {
            SetCursor(dnCursor);
        } else {
            SetCursor(upCursor);
        }
        Poll(e);
    } while (e.rightmouse);

    SetCursor(origCursor);
}
