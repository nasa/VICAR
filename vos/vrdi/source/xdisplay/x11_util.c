/* SCCS %W% %G%  %Q% */
/******************************************************************************/
/*                                                                            */
/*  File: x11_util.c                                                          */
/*                                                                            */
/*  Routines:                                                                 */
/*                                                                            */
/*	display_message(dpy, win, buf, refresh)                               */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		char *buf;                                                    */
/*		int refresh;                                                  */
/*                                                                            */
/*	message_on(dpy, win)                                                  */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	message_off(dpy, win)                                                 */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	map_controls(dpy, win)                                                */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	map_message(dpy, win)                                                 */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	error(dpy, win, buf)                                                  */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		char *buf;                                                    */
/*	-- print a fatal error message then exit program                      */
/*                                                                            */
/*	soft_error(dpy, win, buf)                                             */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		char *buf;                                                    */
/*	-- print an error message                                             */
/*                                                                            */
/*	delay_seconds(secs)                                                   */
/*		float secs;                                                   */
/*                                                                            */
/*	delay(timeout)                                                        */
/*		struct timeval timeout;                                       */
/*                                                                            */
/*	make_geometry(buf, width, height, x, y)                               */
/*		char *buf;                                                    */
/*		int width, height, x, y;                                      */
/*	-- create a geometry string used by X                                 */
/*                                                                            */
/*	create_expose_event(dpy, win, left, top, width, height)               */
/*		Display *dpy;                                                 */
/*		Window win;                                                   */
/*		short int width, height, top, left;                           */
/*	-- put an expose event on the X event queue.                          */
/*                                                                            */
/*	create_clientmessage_event(dpy, win)                                  */
/*		Display *dpy;                                                 */
/*		Window win;                                                   */
/*                                                                            */
/*	refresh_windows(dpy, win)                                             */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	unsigned int button_status(dpy, win)                                  */
/*		Display *dpy;                                                 */
/*		Window win;                                                   */
/*                                                                            */
/*	DisplayErrorMsg(dpy, win, buf)                                        */
/*		Display *dpy;                                                 */
/*		Window win;                                                   */
/*		char *buf;                                                    */
/*                                                                            */
/*	imp2raw(imp, ximp, yimp, xraw, yraw)                                  */
/*		int imp, *ximp, *yimp, *xraw, *yraw;                          */
/*                                                                            */
/*	raw2imp(imp, xraw, yraw, ximp, yimp)                                  */
/*		int imp, xraw, yraw, *ximp, *yimp;                            */
/*                                                                            */
/*	cursor_coords(imp, ximp, yimp, es, el, xraw, yraw, ns, nl)            */
/*		int imp, *ximp, *yimp, es, el, *xraw, *yraw, *ns, *nl;        */
/*                                                                            */
/*	int check_display_mode()                                              */
/*                                                                            */
/*      redraw_button(dpy, window, pixmap)				      */
/*		Display *dpy;						      */
/*		Window window;						      */
/*		Pixmap pixmap;						      */
/*                                                                            */
/*  Written by:			Mark Mann                                     */
/*  Date:			???                                           */
/*                                                                            */
/*  Cognizant Programmer:	Paul Bartholomew                              */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  08-01-82   ---   PDB-Complete rewrite.                                    */
/*                                                                            */
/******************************************************************************/

#include "xvmaininc.h"
#include "xdexterns.h"
#if VMS_OS
#include <time.h>
#include <socket.h>		/* for timeval */
#include <stdio.h>		/* for NULL (!) */
#else
#include <sys/time.h>
#endif

#if VMS_OS && ALPHA_ARCH
#define CADDR_T		/* prevent multiple define with socket.h and xlib.h */
#endif
#include "x11_device_main.h"
#include <X11/Xutil.h>

/* #define DEBUG */
#ifdef DEBUG
#define DPR(A)  {printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

extern int unit;
extern Atom wm_proto;
extern char MESSAGE[];
extern int MESSAGE_LEN;
extern int fg, bg;
extern GC message_gc, button_gc;
extern XFontStruct *font_struct;
extern Colormap colormap;
extern int fontx, fonty;


display_message(dpy, win, buf, refresh)
Display *dpy;
MAIN_WINDOW *win;
char *buf;
int refresh;
{
   register len;

   if (refresh) {
      XClearWindow(dpy, win->message);
      XDrawImageString(dpy, win->message, message_gc, 5, BDR_WW-4,
           MESSAGE, MESSAGE_LEN);
   }
   else {
      len = strlen(buf);
      if (len < MESSAGE_LEN)
         XClearWindow(dpy, win->message);

      if (len > 0)
         XDrawImageString(dpy, win->message, message_gc, 5, BDR_WW-4, buf, len);
      MESSAGE_LEN = len;
      memcpy(MESSAGE, buf, len);
   }
}


message_on(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   map_message(dpy, win);

   MESSAGE_LEN = 0;
   XClearWindow(dpy, win->message);
}


message_off(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   map_controls(dpy, win);
}


map_controls(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   XMapWindow(dpy, win->lock);
   if (X_Image_Depth == 8)
      XMapWindow(dpy, win->bit_mode);
   XMapWindow(dpy, win->s1);
   XMapWindow(dpy, win->up);
   XMapWindow(dpy, win->dn);
   XMapWindow(dpy, win->ve);
   XMapWindow(dpy, win->s2);
   XMapWindow(dpy, win->lt);
   XMapWindow(dpy, win->rt);
   XMapWindow(dpy, win->he);
   XMapWindow(dpy, win->hist_but);
   XMapWindow(dpy, win->cursor);
   XUnmapWindow(dpy, win->right_fill);
   XUnmapWindow(dpy, win->message);
}


map_message(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   XUnmapWindow(dpy, win->lock);
   if (X_Image_Depth == 8)
      XUnmapWindow(dpy, win->bit_mode);
   XUnmapWindow(dpy, win->s1);
   XUnmapWindow(dpy, win->up);
   XUnmapWindow(dpy, win->dn);
   XUnmapWindow(dpy, win->ve);
   XUnmapWindow(dpy, win->s2);
   XUnmapWindow(dpy, win->lt);
   XUnmapWindow(dpy, win->rt);
   XUnmapWindow(dpy, win->he);
   XUnmapWindow(dpy, win->hist_but);
   XUnmapWindow(dpy, win->cursor);
   XMapWindow(dpy, win->right_fill);
   XMapWindow(dpy, win->message);
}


error(dpy, win, buf)
Display *dpy;
MAIN_WINDOW *win;
char *buf;
{
   DisplayErrorMsg(dpy, win, buf);
   exit(-1);
}


soft_error(dpy, win, buf)
Display *dpy;
MAIN_WINDOW *win;
char *buf;
{
   DisplayErrorMsg(dpy, win, buf);
}


delay_seconds(secs)
float secs;
{
   int seconds;
   int mseconds;
   struct timeval timeout;

   seconds = (int) secs;
   mseconds = (int) (1000000.0*secs - 1000000.0*seconds);

   timeout.tv_sec  = seconds;
   timeout.tv_usec = mseconds;

   delay(timeout);
}


delay(timeout)
struct timeval timeout;
{
   int width = get_dtablesize();

   /* select used as a delay mechanism */
   select(width, 0, 0, 0, &timeout);
}


create_expose_event(dpy, win, left, top, width, height)
Display *dpy;
Window win;
short int width, height, top, left;
{
   XExposeEvent event;

   event.type       = Expose;
   event.send_event = True;
   event.display    = dpy;
   event.window     = win;
   event.x          = left;
   event.y          = top;
   event.width      = width;
   event.height     = height;
   event.count      = 0;
   XSendEvent(dpy, win, True, -1, (XEvent *)&event);
}


create_clientmessage_event(dpy, win)
Display *dpy;
Window win;
{
   XClientMessageEvent event;

   event.type = ClientMessage;
   event.send_event = True;
   event.display = dpy;
   event.window = win;
   event.message_type = wm_proto;
   event.format = 32;
   XSendEvent(dpy, win, True, -1, (XEvent *)&event);
}


refresh_windows(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   int imp;

/*   reconfigure(dpy, win); */
   reconf_imp(dpy, win, 1, 1, ZN_SAMPS, ZN_LINES);
   refresh_icon(dpy, win);
}


unsigned int button_status(dpy, win)
Display *dpy;
Window   win;
{
   Window root_ret, child_ret;
   int root_x, root_y, child_x, child_y;
   unsigned int state;

   XQueryPointer(dpy, win, &root_ret, &child_ret, &root_x, &root_y,
        &child_x, &child_y, &state);

   return(state);
}


DisplayErrorMsg(dpy, win, buf)
Display *dpy;
MAIN_WINDOW *win;
char *buf;
{
   int x, y, wd, ht;
   int oklen, devlen, mlen, cont;
   XSizeHints size;
   XWindowAttributes xwa;
   XEvent event;
   unsigned long int mask;
   int dtx, mtx;
   XGCValues xgcv;
   GC main_gc, ok_gc;
   Window main, ok;
   char devname[20];

   DPR(("x11_util: displaying error msg '%s'\n", buf));

   sprintf(devname, "VRDI Device %s:", win->dev);
   devlen = XTextWidth(font_struct, devname, strlen(devname)) + 12;
   mlen = XTextWidth(font_struct, buf, strlen(buf)) + 12;
   oklen = XTextWidth(font_struct, "OK", 2) + 8;

   x  = 1;
   y  = 1;
   wd = MAX(mlen + 2*BDR_WW, devlen + 2*BDR_WW);
   ht = 7*BDR_WW;

   main = XCreateSimpleWindow(dpy, win->root, x, y, wd, ht, 10, bg, fg);
   DPR(("x11_util: error message window created\n"));

   if (main == None) {
      printf("x11_util: couldn't create message window\n");
      printf("%s\n", devname);
      printf("error message is: %s\n", buf);
      return;
   }

   ok = XCreateSimpleWindow(dpy, main, ((wd/2)-(oklen/2)), ht-(2*BDR_WW), oklen,
        BDR_WW, 2, bg, fg);
   DPR(("x11_util: 'ok' button created\n"));

   size.flags = USPosition | USSize;
   size.x = x;
   size.y = y;
   size.width = wd;
   size.height = ht;
   XSetNormalHints(dpy, main, &size);
   XStoreName(dpy, main, "VRDI Message");

   XSelectInput(dpy, main, ExposureMask);
   XSelectInput(dpy, ok, ExposureMask | ButtonPress);

   mask =  GCFont | GCForeground | GCBackground | GCFunction;
   xgcv.font = font_struct->fid;
   xgcv.foreground = bg;
   xgcv.background = fg;
   xgcv.function   = GXcopy;

   main_gc = XCreateGC(dpy, main, mask, &xgcv);
   ok_gc   = XCreateGC(dpy, ok, mask, &xgcv);

   XMapWindow(dpy, main);
   XMapSubwindows(dpy, main);

   mtx = MAX(BDR_WW, (wd-mlen)/2);
   dtx = MAX(BDR_WW, (wd-devlen)/2);
   cont = TRUE;
   DPR(("x11_util: heading for the event loop\n"));
   while(cont) {
      XNextEvent(dpy, &event);
      switch (event.type) {
         case Expose:
            DPR(("x11_util: expose event for message window\n"));
            XDrawImageString(dpy, main, main_gc, dtx, fonty+BDR_WW, devname,
                 strlen(devname));
            XDrawImageString(dpy, main, main_gc, mtx, fonty+3*BDR_WW, buf,
                 strlen(buf));
            XDrawImageString(dpy, ok, ok_gc, 4, fonty+4, "OK", 2);
            break;
         case ButtonPress:
            DPR(("x11_util: button press received\n"));
            if (event.xkey.window == ok) {
               cont = FALSE;
               XUnmapWindow(dpy, main);
               XDestroyWindow(dpy, main);
               XFreeGC(dpy, main_gc);
               XFreeGC(dpy, ok_gc);
               XFlush(dpy);
            }
            break;
      }
   }
}


imp2raw(imp, ximp, yimp, es, el, xraw, yraw, ns, nl)
int imp, *ximp, *yimp, es, el, *xraw, *yraw, *ns, *nl;
{
   int esraw, elraw;

   *xraw = (*ximp - X_DW_LEFT(imp)) * X_ZOOM(imp) + 1;
   *yraw = (*yimp - X_DW_TOP(imp)) * X_ZOOM(imp) + 1;
   esraw = (es - X_DW_LEFT(imp)) * X_ZOOM(imp) + 1;
   elraw = (el - X_DW_TOP(imp)) * X_ZOOM(imp) + 1;

   DPR(("imp2raw: ximp=%d, yimp=%d, xraw=%d, yraw=%d, esraw=%d, elraw=%d\n",
        *ximp, *yimp, *xraw, *yraw, esraw, elraw));

   if (*yraw > ZN_LINES)
      *yraw = *yraw - ZN_LINES * X_ZOOM(imp);

   if (*yraw < 1) {
      *yraw = *yraw + ZN_LINES * X_ZOOM(imp);
      if (*yraw > ZN_LINES) {
         *yimp = X_DW_TOP(imp);
         *yraw = 1;
      }
   }

   if (elraw < 1)
      elraw = elraw + ZN_LINES * X_ZOOM(imp);

   if (elraw > ZN_LINES) {
      elraw = elraw - ZN_LINES * X_ZOOM(imp);
      if (elraw < 1)
         elraw = ZN_LINES - X_ZOOM(imp) + 1;
   }

   if (*xraw > ZN_SAMPS)
      *xraw = *xraw - ZN_SAMPS * X_ZOOM(imp);

   if (*xraw < 1) {
      *xraw = *xraw + ZN_SAMPS * X_ZOOM(imp);
      if (*xraw > ZN_SAMPS) {
         *ns = ZN_SAMPS;
         *ximp = X_DW_LEFT(imp);
         *xraw = 1;
      }
   }

   if (esraw < 1)
      esraw = esraw + ZN_SAMPS * X_ZOOM(imp);

   if (esraw > ZN_SAMPS) {
      esraw = esraw - ZN_SAMPS * X_ZOOM(imp);
      if (esraw < 1)
         esraw = ZN_SAMPS - X_ZOOM(imp) + 1;
   }

   *ns = esraw - *xraw + X_ZOOM(imp);
   *nl = elraw - *yraw + X_ZOOM(imp);
   if (*nl < 1) *nl = *nl + ZN_LINES;
   if (*ns < 1) *ns = *ns + ZN_SAMPS;

   DPR(("imp2raw: ximp=%d, yimp=%d, xraw=%d, yraw=%d, esraw=%d, elraw=%d\n",
        *ximp, *yimp, *xraw, *yraw, esraw, elraw));
   DPR(("imp2raw: ns=%d, nl=%d\n", *ns, *nl));
}


raw2imp(imp, xraw, yraw, ximp, yimp)
int imp, xraw, yraw, *ximp, *yimp;
{
   *ximp = (xraw - 1) / X_ZOOM(imp) + X_DW_LEFT(imp);
   *yimp = (yraw - 1) / X_ZOOM(imp) + X_DW_TOP(imp);

   if (*ximp > ZN_SAMPS)
      *ximp = *ximp - ZN_SAMPS;
   if (*yimp > ZN_LINES)
      *yimp = *yimp - ZN_LINES;
}


cursor_coords(imp, ximp, yimp, es, el, xraw, yraw, ns, nl)
int imp, *ximp, *yimp, es, el, *xraw, *yraw, *ns, *nl;
{
   int fudge_factor;

   *xraw = *ximp;
   *yraw = *yimp;

   *ximp = (*xraw - 1) / X_ZOOM(imp) + X_DW_LEFT(imp);
   *yimp = (*yraw - 1) / X_ZOOM(imp) + X_DW_TOP(imp);

   if (*ximp > ZN_SAMPS)
      *ximp = *ximp - ZN_SAMPS;
   if (*yimp > ZN_LINES)
      *yimp = *yimp - ZN_LINES;

   *ns = es - *xraw + 1;
   *nl = el - *yraw + 1;

   fudge_factor = *xraw % X_ZOOM(imp) - X_ZOOM(imp) + 1;
   *ns = *ns - fudge_factor;
   *xraw = *xraw + fudge_factor;

   fudge_factor = *yraw % X_ZOOM(imp) - X_ZOOM(imp) + 1;
   *nl = *nl - fudge_factor;
   *yraw = *yraw + fudge_factor;
}


int check_display_mode()
{
   int dispmode, lut, i;

   DPR(("x11_util: checking display mode.  valid=%d, mode=%d\n",
        ZVALID_MODE, ZOUTPUT_MODE));

   if (ZVALID_MODE)
      return (ZOUTPUT_MODE);

   ZVALID_MODE = TRUE;
   dispmode = BLACK_AND_WHITE;
   for (lut = 2; lut <= ZN_LUTS && dispmode == BLACK_AND_WHITE; lut++) {
      if (X_WHICH_IMP(1) != X_WHICH_IMP(lut)) {
         dispmode = FULL_COLOR;
         ZOUTPUT_MODE = dispmode;
      }
   }

   if (dispmode == FULL_COLOR) {
      DPR(("x11_util:  display mode = full color\n"));
      return (dispmode);
   }

   for (lut = 2; lut <= ZN_LUTS && dispmode == BLACK_AND_WHITE; lut++) {
      for (i = 0; i < X_LUT_SIZE && dispmode == BLACK_AND_WHITE; i++) {
         if (VRDI_LUT(X_RED, i) != VRDI_LUT(lut, i))
            dispmode = PSEUDO_COLOR;
      }
   }

   ZOUTPUT_MODE = dispmode;
   DPR(("x11_util: display mode=%d, output mode=%d\n", dispmode, ZOUTPUT_MODE));
   return (dispmode);
}


redraw_button(dpy, window, pixmap)
Display *dpy;
Window window;
Pixmap pixmap;
{
   XCopyArea(dpy, pixmap, window, button_gc, 0, 0, BDR_WW, BDR_WW, 0, 0);
}

